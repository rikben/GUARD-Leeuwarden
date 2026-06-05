import math
import os
import shutil
import sys
from datetime import datetime, timedelta, timezone

import geopandas as gpd
import rasterio
from rasterio.mask import mask
from rasterio.merge import merge

from oauthlib.oauth2 import BackendApplicationClient, TokenExpiredError
from requests.exceptions import HTTPError, RequestException, SSLError
from requests_oauthlib import OAuth2Session


def _resolve_verify_setting():
    ca_bundle = os.getenv("CDSE_CA_BUNDLE", "").strip()
    if ca_bundle:
        return ca_bundle

    ssl_verify = os.getenv("CDSE_SSL_VERIFY", "false").strip().lower()
    if ssl_verify in {"0", "false", "no", "off"}:
        return False

    return True


def _prepare_aoi_geometries(shapefile_path):
    gdf = gpd.read_file(shapefile_path)
    if gdf.empty:
        raise ValueError("AOI shapefile contains no geometries.")

    if gdf.crs is None:
        raise ValueError("AOI shapefile has no CRS. Please define CRS before running.")

    gdf_wgs84 = gdf.to_crs(epsg=4326)
    geom_wgs84 = gdf_wgs84.geometry.union_all()
    if geom_wgs84 is None or geom_wgs84.is_empty:
        raise ValueError("AOI shapefile geometry is empty after merge.")

    gdf_3857 = gdf.to_crs(epsg=3857)
    geom_3857 = gdf_3857.geometry.union_all()
    if geom_3857 is None or geom_3857.is_empty:
        raise ValueError("AOI shapefile geometry is empty after reprojection to EPSG:3857.")

    wgs84_bbox = list(geom_wgs84.bounds)
    bbox_3857 = list(geom_3857.bounds)

    return geom_wgs84, wgs84_bbox, geom_3857, bbox_3857


def _safe_date_str(dt_text):
    return dt_text.replace(":", "-").replace("T", "_").replace("Z", "")


def _build_tiles(bbox_3857, resolution_m, max_pixels=2400):
    minx, miny, maxx, maxy = bbox_3857
    tile_size_m = resolution_m * max_pixels

    n_cols = max(1, math.ceil((maxx - minx) / tile_size_m))
    n_rows = max(1, math.ceil((maxy - miny) / tile_size_m))

    tiles = []
    for row in range(n_rows):
        y0 = miny + row * tile_size_m
        y1 = min(maxy, y0 + tile_size_m)
        for col in range(n_cols):
            x0 = minx + col * tile_size_m
            x1 = min(maxx, x0 + tile_size_m)
            tiles.append([x0, y0, x1, y1])

    return tiles


# Your client credentials
client_id = "sh-d43669f7-f402-4b8e-bf7c-75a988fca077"
client_secret = "xdcTtKsgUN3e9fXu0UKGFLJEwO0miR62"
token_url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
catalog_search_url = "https://sh.dataspace.copernicus.eu/api/v1/catalog/1.0.0/search"
process_url = "https://sh.dataspace.copernicus.eu/api/v1/process"

shapefile_path = r"C:\Users\NL1G7U\Documents\stage_proj\Data\leeuwarden_percelen\template_leeuwarden_percelen.shp"
output_dir = r"C:\Users\NL1G7U\Documents\stage_proj\Data\sentinel_leeuwarden_l2a_b2b3b4b8"
time_from = "2020-03-23T00:00:00Z"
time_to = "2020-05-07T23:59:59Z"
max_cloud_coverage = 30
resolution_m = 10
max_items = 100
max_tile_pixels = 2400

verify_setting = _resolve_verify_setting()
os.environ["CDSE_SSL_VERIFY"] = "false"

# Create a session
client = BackendApplicationClient(client_id=client_id)
oauth = OAuth2Session(client=client)


def _fetch_token():
    return oauth.fetch_token(
        token_url=token_url,
        client_secret=client_secret,
        include_client_id=True,
        verify=verify_setting,
        timeout=30,
    )


def _oauth_request(method, url, **kwargs):
    try:
        return oauth.request(method=method, url=url, **kwargs)
    except TokenExpiredError:
        _fetch_token()
        return oauth.request(method=method, url=url, **kwargs)

try:
    # Get token for the session
    token = _fetch_token()

    _, bbox_wgs84, aoi_geom_3857, bbox_3857 = _prepare_aoi_geometries(shapefile_path)
    os.makedirs(output_dir, exist_ok=True)
    tiles_3857 = _build_tiles(bbox_3857, resolution_m, max_pixels=max_tile_pixels)
    print(f"AOI split into {len(tiles_3857)} tile(s) to satisfy API size limits.")

    search_payload = {
        "collections": ["sentinel-2-l2a"],
        "bbox": bbox_wgs84,
        "datetime": f"{time_from}/{time_to}",
        "limit": max_items,
        "filter-lang": "cql2-text",
        "filter": f"eo:cloud_cover <= {max_cloud_coverage}",
        "fields": {
            "include": ["id", "properties.datetime", "properties.eo:cloud_cover"],
            "exclude": ["assets", "links", "geometry", "bbox", "stac_extensions"],
        },
    }

    search_resp = _oauth_request(
        "POST",
        catalog_search_url,
        json=search_payload,
        verify=verify_setting,
        timeout=60,
    )
    search_resp.raise_for_status()
    features = search_resp.json().get("features", [])

    if not features:
        print("No Sentinel-2 L2A scenes found for the requested filters.")
        sys.exit(0)

    features_sorted = sorted(features, key=lambda f: f.get("properties", {}).get("datetime", ""))

    evalscript = """
//VERSION=3
function setup() {
  return {
    input: [{ bands: ["B02", "B03", "B04", "B08"], units: "REFLECTANCE" }],
    output: { bands: 4, sampleType: "FLOAT32" }
  };
}

function evaluatePixel(sample) {
  return [sample.B02, sample.B03, sample.B04, sample.B08];
}
"""

    downloaded = 0
    for feature in features_sorted:
        scene_id = feature.get("id")
        scene_dt = feature.get("properties", {}).get("datetime")
        if not scene_id or not scene_dt:
            continue

        # Process each acquisition in a 1-second window to get one TIFF per date/time.
        dt_obj = datetime.fromisoformat(scene_dt.replace("Z", "+00:00")).astimezone(timezone.utc)
        dt_obj = dt_obj.replace(microsecond=0)
        dt_start = dt_obj.strftime("%Y-%m-%dT%H:%M:%SZ")
        dt_end = (dt_obj + timedelta(seconds=1)).strftime("%Y-%m-%dT%H:%M:%SZ")

        scene_tmp_dir = os.path.join(output_dir, f"_tmp_{scene_id}")
        os.makedirs(scene_tmp_dir, exist_ok=True)

        out_name = f"S2_L2A_{_safe_date_str(scene_dt)}_{scene_id}.tif"
        out_path = os.path.join(output_dir, out_name)
        if os.path.exists(out_path):
            print(f"Skipping existing file: {out_name}")
            continue

        tile_paths = []
        for i, tile_bbox in enumerate(tiles_3857, start=1):
            process_payload = {
                "input": {
                    "bounds": {
                        "bbox": tile_bbox,
                        "properties": {"crs": "http://www.opengis.net/def/crs/EPSG/0/3857"},
                    },
                    "data": [
                        {
                            "type": "sentinel-2-l2a",
                            "dataFilter": {
                                "timeRange": {
                                    "from": dt_start,
                                    "to": dt_end,
                                },
                                "maxCloudCoverage": max_cloud_coverage,
                            },
                        }
                    ],
                },
                "output": {
                    "resx": resolution_m,
                    "resy": resolution_m,
                    "responses": [{"identifier": "default", "format": {"type": "image/tiff"}}],
                },
                "evalscript": evalscript,
            }

            dl_resp = _oauth_request(
                "POST",
                process_url,
                json=process_payload,
                verify=verify_setting,
                timeout=180,
            )
            dl_resp.raise_for_status()

            tile_path = os.path.join(scene_tmp_dir, f"tile_{i:03d}.tif")
            with open(tile_path, "wb") as f:
                f.write(dl_resp.content)
            tile_paths.append(tile_path)

        src_files = [rasterio.open(p) for p in tile_paths]
        try:
            mosaic_data, mosaic_transform = merge(src_files)
            profile = src_files[0].profile.copy()
            profile.update(
                {
                    "driver": "GTiff",
                    "height": mosaic_data.shape[1],
                    "width": mosaic_data.shape[2],
                    "transform": mosaic_transform,
                    "count": mosaic_data.shape[0],
                    "compress": "deflate",
                }
            )

            mosaic_path = os.path.join(scene_tmp_dir, "mosaic.tif")
            with rasterio.open(mosaic_path, "w", **profile) as dst:
                dst.write(mosaic_data)

            with rasterio.open(mosaic_path) as src:
                clipped_data, clipped_transform = mask(
                    src,
                    [aoi_geom_3857.__geo_interface__],
                    crop=True,
                )
                clipped_profile = src.profile.copy()
                clipped_profile.update(
                    {
                        "height": clipped_data.shape[1],
                        "width": clipped_data.shape[2],
                        "transform": clipped_transform,
                        "count": clipped_data.shape[0],
                        "compress": "deflate",
                    }
                )

            with rasterio.open(out_path, "w", **clipped_profile) as dst:
                dst.write(clipped_data)
        finally:
            for src in src_files:
                src.close()
            shutil.rmtree(scene_tmp_dir, ignore_errors=True)

        downloaded += 1
        cloud = feature.get("properties", {}).get("eo:cloud_cover")
        print(f"Downloaded: {out_name} | cloud={cloud}")

    print(f"Completed. Downloaded {downloaded} TIFF files to: {output_dir}")
except SSLError as exc:
    print("TLS/SSL verification failed.")
    print(
        "If your network uses SSL inspection, set CDSE_CA_BUNDLE to your corporate CA certificate path."
    )
    print(
        "Temporary fallback (not secure): set CDSE_SSL_VERIFY=false to disable certificate checks."
    )
    print(f"Details: {exc}")
    sys.exit(1)
except HTTPError as exc:
    response = exc.response
    print("The API request reached CDSE but returned an HTTP error.")
    if response is not None:
        print(f"Status: {response.status_code}")
        print(f"URL: {response.url}")
        print("Response body:")
        print(response.text)
    else:
        print(f"Details: {exc}")
    sys.exit(1)
except RequestException as exc:
    print("Network request failed.")
    print(f"Details: {exc}")
    sys.exit(1)