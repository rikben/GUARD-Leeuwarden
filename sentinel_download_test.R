#Switched from an AOI-based workflow to a parcel-based workflow**: instead of downloading imagery for one large area, the R script processes individual agricultural parcels.
#Removed the tiling and mosaicking system**: the Python script split large AOIs into API-compatible tiles and merged them afterward; the R script assumes parcels are small enough to download in a single request.
#Replaced shapefile AOIs with BRP-derived GeoJSON parcels**: the spatial units are now agricultural parcels rather than a manually defined study-area shapefile.
#Introduced a reusable `download_sentinel(geometry)` function**: the download logic is no longer tied to a specific AOI and can work with any geometry (parcel, polygon, grid cell, etc.).
#Changed the purpose of the pipeline**: the Python script was designed as a general Sentinel-2 scene download and processing workflow, while the R script is being structured as the foundation for parcel-level data extraction that can later feed sampling, training, and national-scale prediction workflows.


library(sf)
library(terra)
library(httr2)
library(jsonlite)
library(dplyr)

# -----------------------
# 1. CONFIG
# -----------------------

config <- list(
  time_from = "2020-03-23T00:00:00Z",
  time_to   = "2020-05-07T23:59:59Z",
  
  cloud_max = 30,
  resolution = 10,
  
  output_dir = "output",
  cache_dir  = "cache"
)

dir.create(config$output_dir, showWarnings = FALSE)
dir.create(config$cache_dir, showWarnings = FALSE)

# -----------------------
# 2. BUILD PARCEL DATASET
# -----------------------

brp <- st_read("BRP.gpkg")

brp <- brp |>
  st_make_valid() |>
  st_transform(4326)

parcels <- brp

st_write(parcels, "data/parcels.geojson", delete_dsn = TRUE)

# -----------------------
# 3. LOAD DATA FOR PIPELINE
# -----------------------

parcels <- st_read("data/parcels.geojson") |> st_transform(4326)
parcels <- st_make_valid(parcels)

# optional (not used yet, but kept for future extension)
# obs <- st_read("data/waarneming.geojson") |> st_transform(4326)
# obs <- st_make_valid(obs)

# -----------------------
# 4. AUTHENTICATION
# -----------------------

get_token <- function() {
  
  resp <- request("https://identity.dataspace.copernicus.eu/...") |>
    req_body_form(
      client_id = Sys.getenv("CDSE_CLIENT_ID"),
      client_secret = Sys.getenv("CDSE_CLIENT_SECRET"),
      grant_type = "client_credentials"
    ) |>
    req_perform()
  
  resp_body_json(resp)$access_token
}

# -----------------------
# 5. GEOMETRY HELPER
# -----------------------

get_bbox_3857 <- function(x) {
  st_bbox(st_transform(x, 3857))
}

# -----------------------
# 6. SENTINEL EVALSCRIPT
# -----------------------

evalscript <- "
//VERSION=3
function setup() {
  return {
    input: [{ bands: ['B02','B03','B04','B08'] }],
    output: { bands: 4 }
  };
}

function evaluatePixel(s) {
  return [s.B02, s.B03, s.B04, s.B08];
}
"

# -----------------------
# 7. DOWNLOAD FUNCTION
# -----------------------

download_sentinel <- function(geometry, token, config) {
  
  bbox <- get_bbox_3857(geometry)
  
  body <- list(
    input = list(
      bounds = list(bbox = bbox),
      data = list(list(
        type = "sentinel-2-l2a",
        dataFilter = list(
          timeRange = list(
            from = config$time_from,
            to   = config$time_to
          ),
          maxCloudCoverage = config$cloud_max
        )
      ))
    ),
    output = list(
      resx = config$resolution,
      resy = config$resolution,
      responses = list(list(
        identifier = "default",
        format = list(type = "image/tiff")
      ))
    ),
    evalscript = evalscript
  )
  
  resp <- request("https://sh.dataspace.copernicus.eu/api/v1/process") |>
    req_auth_bearer_token(token) |>
    req_body_json(body) |>
    req_perform()
  
  file <- file.path(
    config$output_dir,
    paste0("S2_", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"), ".tif")
  )
  
  writeBin(resp_body_raw(resp), file)
  
  file
}

# -----------------------
# 8. PROCESSING
# -----------------------

process_parcel <- function(raster_file, geometry, config) {
  
  r <- rast(raster_file)
  v <- vect(geometry)
  
  clipped <- mask(crop(r, v), v)
  
  out_file <- file.path(
    config$output_dir,
    paste0("clip_", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"), ".tif")
  )
  
  writeRaster(clipped, out_file, overwrite = TRUE)
  
  out_file
}

# -----------------------
# 9. MAIN LOOP
# -----------------------

token <- get_token()

results <- vector("list", nrow(parcels))

for (i in seq_len(nrow(parcels))) {
  
  cat("Processing parcel", i, "of", nrow(parcels), "\n")
  
  parcel <- parcels[i, ]
  
  raster_file <- download_sentinel(parcel, token, config)
  
  out_file <- process_parcel(raster_file, parcel, config)
  
  results[[i]] <- out_file
}

cat("Done.\n")
