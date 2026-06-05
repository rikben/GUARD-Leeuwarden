# prepare_soil_brp.R

library(sf)
library(dplyr)

# ---- Settings ----
out_dir <- "data"

soil_gpkg <- file.path(out_dir, "BRO_DownloadBodemkaart.gpkg")
soil_url <- "https://service.pdok.nl/tno/bro-bodemkaart/atom/downloads/BRO_DownloadBodemkaart.gpkg"

# Currently known available BRP GPKG years:
# 2020, 2021, 2022, 2023, 2024, 2025
brp_years <- c(2025)
# Example for multiple years:
# brp_years <- c(2023, 2024, 2025)

brp_url_template <- "https://service.pdok.nl/rvo/gewaspercelen/atom/downloads/brpgewaspercelen_definitief_%s.gpkg"

# ---- Create output folder ----
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ---- Helper: download only if missing ----
download_if_missing <- function(url, destfile, min_size_mb = 100, timeout_seconds = 3600) {
  file_ok <- file.exists(destfile) &&
    file.info(destfile)$size > min_size_mb * 1024^2
  
  if (file_ok) {
    message("File already exists and looks valid, skipping download: ", destfile)
  } else {
    if (file.exists(destfile)) {
      message("Existing file looks incomplete, removing: ", destfile)
      file.remove(destfile)
    }
    
    message("Downloading: ", url)
    old_timeout <- getOption("timeout")
    options(timeout = timeout_seconds)
    on.exit(options(timeout = old_timeout), add = TRUE)
    
    download.file(
      url,
      destfile,
      mode = "wb",
      method = "libcurl"
    )
  }
}

# ---- Helper: get first spatial layer from a GPKG ----
get_first_spatial_layer <- function(gpkg_path) {
  layers <- st_layers(gpkg_path)
  spatial_layers <- layers$name[!is.na(layers$geomtype)]
  
  if (length(spatial_layers) == 0) {
    stop("No spatial layers found in: ", gpkg_path)
  }
  
  spatial_layers[1]
}

# ---- Download Bodemkaart if needed ----
download_if_missing(soil_url, soil_gpkg)

# ---- Read soil layers ----
soilarea <- st_read(soil_gpkg, layer = "soilarea", quiet = FALSE) |>
  st_transform(28992) |>
  st_make_valid()

soilarea_soilunit <- st_read(soil_gpkg, layer = "soilarea_soilunit", quiet = FALSE) |>
  st_drop_geometry()

soil_units <- st_read(soil_gpkg, layer = "soil_units", quiet = FALSE) |>
  st_drop_geometry()

# ---- Join soil attributes to soil areas ----
soil_classes <- soilarea |>
  left_join(soilarea_soilunit, by = "maparea_id") |>
  left_join(soil_units, by = c("soilunit_code" = "code")) |>
  select(
    maparea_id,
    soilunit_code,
    soilclassification,
    mainsoilclassification
  )

# ---- Process BRP per year ----
for (year in brp_years) {
  
  message("---- Processing BRP year: ", year, " ----")
  
  brp_url <- sprintf(brp_url_template, year)
  brp_gpkg <- file.path(out_dir, paste0("brpgewaspercelen_definitief_", year, ".gpkg"))
  
  out_file_detail <- file.path(out_dir, paste0("brp_soil_intersections_", year, ".geojson"))
  out_file_dominant <- file.path(out_dir, paste0("brp_dominant_soil_", year, ".geojson"))
  
  # ---- Download BRP GPKG if needed ----
  download_if_missing(brp_url, brp_gpkg)
  
  # ---- Read BRP parcels ----
  print(st_layers(brp_gpkg))
  brp_layer <- get_first_spatial_layer(brp_gpkg)
  
  brp <- st_read(brp_gpkg, layer = brp_layer, quiet = FALSE) |>
    st_transform(28992) |>
    st_make_valid()
  
  message("Number of BRP parcels read for ", year, ": ", nrow(brp))
  
  # ---- Add robust parcel ID and parcel area ----
  brp <- brp |>
    mutate(
      brp_year = year,
      parcel_id = paste0(year, "_", row_number()),
      parcel_area_m2 = as.numeric(st_area(brp))
    )
  
  # ---- Spatial overlay: one parcel may intersect multiple soils ----
  brp_soil_detail <- st_intersection(brp, soil_classes) |>
    st_make_valid() |>
    st_collection_extract("POLYGON") |>
    st_cast("MULTIPOLYGON")
  
  # ---- Calculate soil area share within each parcel ----
  brp_soil_detail <- brp_soil_detail |>
    mutate(
      soil_intersection_area_m2 = as.numeric(st_area(brp_soil_detail)),
      soil_share = soil_intersection_area_m2 / parcel_area_m2
    )
  
  # ---- Dominant soil per parcel ----
  brp_soil_dominant <- brp_soil_detail |>
    group_by(parcel_id) |>
    slice_max(
      order_by = soil_intersection_area_m2,
      n = 1,
      with_ties = FALSE
    ) |>
    ungroup() |>
    mutate(
      dominant_soilunit_code = soilunit_code,
      dominant_mainsoilclassification = mainsoilclassification,
      dominant_soil_share = soil_share
    )
  
  # ---- Save detailed intersection output ----
  if (file.exists(out_file_detail)) {
    file.remove(out_file_detail)
  }
  
  st_write(
    brp_soil_detail,
    out_file_detail,
    driver = "GeoJSON",
    quiet = FALSE
  )
  
  # ---- Save dominant soil output ----
  if (file.exists(out_file_dominant)) {
    file.remove(out_file_dominant)
  }
  
  st_write(
    brp_soil_dominant,
    out_file_dominant,
    driver = "GeoJSON",
    quiet = FALSE
  )
  
  message("Detailed soil intersections written to: ", out_file_detail)
  message("Dominant soil per parcel written to: ", out_file_dominant)
  message("Number of detailed parcel-soil features: ", nrow(brp_soil_detail))
  message("Number of dominant parcel features: ", nrow(brp_soil_dominant))
}