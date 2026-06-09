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
  
  out_file_composition <- file.path(out_dir, paste0("brp_soil_composition_", year, ".gpkg"))
  out_file_dominant <- file.path(out_dir, paste0("brp_dominant_soil_", year, ".gpkg"))
  
  download_if_missing(brp_url, brp_gpkg)
  
  print(st_layers(brp_gpkg))
  brp_layer <- get_first_spatial_layer(brp_gpkg)
  
  brp <- st_read(brp_gpkg, layer = brp_layer, quiet = FALSE) |>
    st_transform(28992) |>
    st_make_valid()
  
  message("Number of BRP parcels read for ", year, ": ", nrow(brp))
  
  brp <- brp |>
    mutate(
      brp_year = year,
      parcel_id = paste0(year, "_", row_number()),
      parcel_area_m2 = as.numeric(st_area(brp))
    )
  
  # ---- Intersect BRP with soil map ----
  brp_soil_detail <- st_intersection(brp, soil_classes) |>
    st_make_valid() |>
    st_collection_extract("POLYGON")
  
  brp_soil_detail <- brp_soil_detail |>
    mutate(
      soil_intersection_area_m2 = as.numeric(st_area(brp_soil_detail)),
      soil_share = soil_intersection_area_m2 / parcel_area_m2
    )
  
  # ---- Save soil composition as non-spatial table in GPKG ----
  brp_soil_composition <- brp_soil_detail |>
    st_drop_geometry() |>
    select(
      parcel_id,
      brp_year,
      parcel_area_m2,
      maparea_id,
      soilunit_code,
      soilclassification,
      mainsoilclassification,
      soil_intersection_area_m2,
      soil_share
    )
  
  if (file.exists(out_file_composition)) {
    file.remove(out_file_composition)
  }
  
  st_write(
    brp_soil_composition,
    out_file_composition,
    layer = paste0("brp_soil_composition_", year),
    driver = "GPKG",
    quiet = FALSE
  )
  
  # ---- Determine dominant soil per parcel ----
  dominant_soil_table <- brp_soil_composition |>
    group_by(parcel_id) |>
    slice_max(
      order_by = soil_intersection_area_m2,
      n = 1,
      with_ties = FALSE
    ) |>
    ungroup() |>
    select(
      parcel_id,
      dominant_soilunit_code = soilunit_code,
      dominant_soilclassification = soilclassification,
      dominant_mainsoilclassification = mainsoilclassification,
      dominant_soil_share = soil_share
    )
  
  # ---- Join dominant soil back to original BRP parcel geometry ----
  brp_dominant_soil <- brp |>
    left_join(dominant_soil_table, by = "parcel_id")
  
  # ---- Save BRP parcels with dominant soil as GPKG ----
  if (file.exists(out_file_dominant)) {
    file.remove(out_file_dominant)
  }
  
  st_write(
    brp_dominant_soil,
    out_file_dominant,
    layer = paste0("brp_dominant_soil_", year),
    driver = "GPKG",
    quiet = FALSE
  )
  
  message("Soil composition table written to: ", out_file_composition)
  message("BRP parcels with dominant soil written to: ", out_file_dominant)
  message("Number of parcel-soil combinations: ", nrow(brp_soil_composition))
  message("Number of BRP parcels with dominant soil: ", nrow(brp_dominant_soil))
}