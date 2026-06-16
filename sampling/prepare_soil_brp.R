# prepare_soil_brp_grid_parallel.R

library(sf)
library(dplyr)
library(foreach)
library(doParallel)

# ---- Settings ----
out_dir <- "data"

soil_gpkg <- file.path(out_dir, "BRO_DownloadBodemkaart.gpkg")
soil_url <- "https://service.pdok.nl/tno/bro-bodemkaart/atom/downloads/BRO_DownloadBodemkaart.gpkg"

brp_years <- c(2020, 2025)

brp_url_template <- "https://service.pdok.nl/rvo/gewaspercelen/atom/downloads/brpgewaspercelen_definitief_%s.gpkg"

# Optional manual override:
# selected_grid_file <- "data/nl_grid_10000m.geojson"
selected_grid_file <- NA

# Parallel settings
use_parallel <- TRUE
max_workers_manual <- 6  # e.g. set to 6 if you want to force 6 workers
gb_ram_per_worker <- 15    # conservative memory estimate

# ---- Create output folder ----
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ---- Helpers ----

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
    
    download.file(url, destfile, mode = "wb", method = "libcurl")
  }
}

get_first_spatial_layer <- function(gpkg_path) {
  layers <- st_layers(gpkg_path)
  spatial_layers <- layers$name[!is.na(layers$geomtype)]
  
  if (length(spatial_layers) == 0) {
    stop("No spatial layers found in: ", gpkg_path)
  }
  
  spatial_layers[1]
}

get_total_ram_gb <- function() {
  if (.Platform$OS.type == "unix" && file.exists("/proc/meminfo")) {
    meminfo <- readLines("/proc/meminfo")
    mem_kb <- as.numeric(sub(".*:\\s+([0-9]+)\\s+kB", "\\1", meminfo[grepl("^MemTotal:", meminfo)]))
    return(mem_kb / 1024^2)
  }
  NA_real_
}

choose_workers <- function() {
  available_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(available_cores)) {
    available_cores <- parallel::detectCores(logical = TRUE)
  }
  if (is.na(available_cores)) {
    available_cores <- 1
  }
  
  ram_gb <- get_total_ram_gb()
  
  by_cpu <- max(1, available_cores - 1)
  by_ram <- if (!is.na(ram_gb)) max(1, floor(ram_gb / gb_ram_per_worker)) else by_cpu
  
  workers <- min(by_cpu, by_ram)
  
  if (!is.na(max_workers_manual)) {
    workers <- min(workers, max_workers_manual)
  }
  
  max(1, workers)
}

choose_grid_file <- function(out_dir, selected_grid_file = NA) {
  if (!is.na(selected_grid_file) && file.exists(selected_grid_file)) {
    return(selected_grid_file)
  }
  
  grid_files <- list.files(
    path = out_dir,
    pattern = "^nl_grid_.*m\\.geojson$",
    full.names = TRUE
  )
  
  if (length(grid_files) == 0) {
    stop("No grid files found in ", out_dir, " matching nl_grid_*m.geojson")
  }
  
  if (length(grid_files) == 1) {
    message("Using only available grid file: ", basename(grid_files[1]))
    return(grid_files[1])
  }
  
  cat("\nAvailable grid files:\n")
  for (i in seq_along(grid_files)) {
    cat(i, ": ", basename(grid_files[i]), "\n", sep = "")
  }
  
  choice <- as.integer(readline("Choose grid file number: "))
  
  if (is.na(choice) || choice < 1 || choice > length(grid_files)) {
    stop("Invalid grid choice.")
  }
  
  grid_files[choice]
}

safe_make_valid <- function(x, label = "object") {
  invalid <- !st_is_valid(x)
  if (any(invalid, na.rm = TRUE)) {
    message("Repairing invalid geometries in ", label, ": ", sum(invalid, na.rm = TRUE))
    x <- st_make_valid(x)
  }
  x
}

process_tile <- function(tile_id, grid, brp, soil_classes) {
  tile <- grid[tile_id, ]
  tile_geom <- st_geometry(tile)
  
  brp_tile <- brp[tile, , op = st_intersects]
  soil_tile <- soil_classes[tile, , op = st_intersects]
  
  if (nrow(brp_tile) == 0 || nrow(soil_tile) == 0) {
    return(NULL)
  }
  
  # Clip to tile to avoid double-counting parcels that cross grid boundaries
  brp_tile <- suppressWarnings(st_intersection(brp_tile, tile_geom))
  soil_tile <- suppressWarnings(st_intersection(soil_tile, tile_geom))
  
  if (nrow(brp_tile) == 0 || nrow(soil_tile) == 0) {
    return(NULL)
  }
  
  detail <- suppressWarnings(st_intersection(brp_tile, soil_tile))
  
  if (nrow(detail) == 0) {
    return(NULL)
  }
  
  detail <- st_collection_extract(detail, "POLYGON", warn = FALSE)
  
  if (nrow(detail) == 0) {
    return(NULL)
  }
  
  detail |>
    mutate(
      tile_id = tile_id,
      soil_intersection_area_m2 = as.numeric(st_area(st_geometry(detail))),
      soil_share = soil_intersection_area_m2 / parcel_area_m2
    ) |>
    st_drop_geometry() |>
    select(
      parcel_id,
      brp_year,
      parcel_area_m2,
      maparea_id,
      soilunit_code,
      soilclassification,
      mainsoilclassification,
      soilcharacteristics_code,
      topsoil_description,
      soil_intersection_area_m2,
      soil_share,
      tile_id
    )
}

# ---- Download soil map ----
download_if_missing(soil_url, soil_gpkg)

# ---- Choose and read grid ----
grid_file <- choose_grid_file(out_dir, selected_grid_file)

message("Reading grid: ", grid_file)

grid <- st_read(grid_file, quiet = FALSE) |>
  st_transform(28992)

grid <- safe_make_valid(grid, "grid")

grid$tile_id <- seq_len(nrow(grid))

message("Number of grid tiles: ", nrow(grid))

# ---- Read soil layers ----
message("Reading soil data...")

soilarea <- st_read(soil_gpkg, layer = "soilarea", quiet = FALSE) |>
  st_transform(28992)

soilarea <- safe_make_valid(soilarea, "soilarea")

soilarea_soilunit <- st_read(soil_gpkg, layer = "soilarea_soilunit", quiet = FALSE) |>
  st_drop_geometry()

soil_units <- st_read(soil_gpkg, layer = "soil_units", quiet = FALSE) |>
  st_drop_geometry()

soilarea_soilunit_toplayer <- st_read(
  soil_gpkg,
  layer = "soilarea_soilunit_soilcharacteristicstoplayer",
  quiet = FALSE
) |>
  st_drop_geometry()

soilcharacteristics_toplayer <- st_read(
  soil_gpkg,
  layer = "soilcharacteristics_toplayer",
  quiet = FALSE
) |>
  st_drop_geometry() |>
  rename(topsoil_description = description)

soil_classes <- soilarea |>
  left_join(soilarea_soilunit, by = "maparea_id") |>
  left_join(soil_units, by = c("soilunit_code" = "code")) |>
  left_join(
    soilarea_soilunit_toplayer,
    by = c("maparea_id", "soilunit_sequencenumber")
  ) |>
  left_join(
    soilcharacteristics_toplayer,
    by = c("soilcharacteristics_code" = "code")
  ) |>
  select(
    maparea_id,
    soilunit_code,
    soilclassification,
    mainsoilclassification,
    soilcharacteristics_code,
    topsoil_description
  )

# ---- Process BRP per year ----
for (year in brp_years) {
  
  message("\n---- Processing BRP year: ", year, " ----")
  
  brp_url <- sprintf(brp_url_template, year)
  brp_gpkg <- file.path(out_dir, paste0("brpgewaspercelen_definitief_", year, ".gpkg"))
  
  out_file_composition <- file.path(out_dir, paste0("brp_soil_composition_", year, ".gpkg"))
  out_file_dominant <- file.path(out_dir, paste0("brp_dominant_soil_", year, ".gpkg"))
  
  download_if_missing(brp_url, brp_gpkg)
  
  print(st_layers(brp_gpkg))
  brp_layer <- get_first_spatial_layer(brp_gpkg)
  
  message("Reading BRP layer: ", brp_layer)
  
  brp <- st_read(brp_gpkg, layer = brp_layer, quiet = FALSE) |>
    st_transform(28992)
  
  brp <- safe_make_valid(brp, "BRP")
  
  message("Number of BRP parcels read for ", year, ": ", nrow(brp))
  
  brp <- brp |>
    mutate(
      brp_year = year,
      parcel_id = paste0(year, "_", row_number()),
      parcel_area_m2 = as.numeric(st_area(st_geometry(brp)))
    )
  
  workers <- choose_workers()
  
  message("Selected workers: ", workers)
  
  if (use_parallel && workers > 1) {
    
    message("Starting parallel tile processing...")
    
    # On Ubuntu/Linux, FORK is usually more memory efficient than PSOCK
    cl <- parallel::makeForkCluster(workers)
    doParallel::registerDoParallel(cl)
    
    tile_results <- foreach(
      tile_id = seq_len(nrow(grid)),
      .packages = c("sf", "dplyr"),
      .errorhandling = "pass"
    ) %dopar% {
      process_tile(tile_id, grid, brp, soil_classes)
    }
    
    parallel::stopCluster(cl)
    
  } else {
    
    message("Starting sequential tile processing...")
    
    tile_results <- vector("list", nrow(grid))
    
    for (tile_id in seq_len(nrow(grid))) {
      message("Processing tile ", tile_id, " of ", nrow(grid))
      tile_results[[tile_id]] <- process_tile(tile_id, grid, brp, soil_classes)
    }
  }
  
  # Check tile errors
  tile_errors <- vapply(tile_results, inherits, logical(1), what = "error")
  
  if (any(tile_errors)) {
    warning("Some tiles failed: ", paste(which(tile_errors), collapse = ", "))
    tile_results <- tile_results[!tile_errors]
  }
  
  tile_results <- tile_results[!vapply(tile_results, is.null, logical(1))]
  
  if (length(tile_results) == 0) {
    stop("No parcel-soil intersections produced for year ", year)
  }
  
  message("Combining tile results...")
  
  brp_soil_composition <- bind_rows(tile_results)
  
  # Because parcels are clipped by grid tile, sum parcel-soil areas back together
  brp_soil_composition <- brp_soil_composition |>
    group_by(
      parcel_id,
      brp_year,
      parcel_area_m2,
      maparea_id,
      soilunit_code,
      soilclassification,
      mainsoilclassification,
      soilcharacteristics_code,
      topsoil_description
    ) |>
    summarise(
      soil_intersection_area_m2 = sum(soil_intersection_area_m2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      soil_share = soil_intersection_area_m2 / parcel_area_m2
    )
  
  # ---- Save soil composition table ----
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
      dominant_soilcharacteristics_code = soilcharacteristics_code,
      dominant_topsoil_description = topsoil_description,
      dominant_soil_share = soil_share
    )
  
  brp_dominant_soil <- brp |>
    left_join(dominant_soil_table, by = "parcel_id")
  
  # ---- Save BRP parcels with dominant soil ----
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
