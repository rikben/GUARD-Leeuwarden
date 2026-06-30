required_packages <- c(
  "sits",
  "sf",
  "httr2",
  "tidyverse",
  "terra",
  "dplyr",
  "ranger",
  "readr"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing missing package: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_packages, install_if_missing))
invisible(lapply(required_packages, function(pkg) {
  message("Loading package: ", pkg)
  library(pkg, character.only = TRUE)
}))

dir.create(file.path(getwd(), "tmp"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(getwd(), "metadata"), showWarnings = FALSE, recursive = TRUE)

Sys.setenv(
  TMPDIR = file.path(getwd(), "tmp"),
  TEMP   = file.path(getwd(), "tmp"),
  TMP    = file.path(getwd(), "tmp")
)

terra::terraOptions(
  tempdir = file.path(getwd(), "tmp"),
  memfrac = 0.6,
  todisk = TRUE,
  progress = 1
)

# ─────────────────────────────────────────────
# HELPER FUNCTIONS: RASTER & MASKING
# ─────────────────────────────────────────────

build_band <- function(paths) {
  paths <- paths[!is.na(paths) & nchar(paths) > 0]
  if (length(paths) == 0) return(NULL)
  if (length(paths) == 1) return(terra::rast(paths))
  rast_list <- lapply(paths, terra::rast)
  return(terra::mosaic(terra::sprc(rast_list)))
}

get_full_pixel_mask <- function(raster_template, poly_single, threshold = 0.999) {
  cov_frac  <- terra::rasterize(poly_single, raster_template, cover = TRUE)
  full_mask <- terra::ifel(cov_frac >= threshold, 1, NA)
  return(full_mask)
}

get_combined_mask <- function(raster_template, poly_single, cloud_raster = NULL,
                              scl_clear_values = c(4, 5, 6)) {
  full_mask <- get_full_pixel_mask(raster_template, poly_single)
  
  if (!is.null(cloud_raster)) {
    cloud_cropped <- terra::crop(cloud_raster, poly_single)
    cloud_cropped <- terra::resample(cloud_cropped, raster_template, method = "near")
    
    is_clear   <- Reduce(`|`, lapply(scl_clear_values, function(v) cloud_cropped == v))
    clear_mask <- terra::ifel(is_clear, 1, NA)
    
    combined_mask <- full_mask * clear_mask
  } else {
    combined_mask <- full_mask
  }
  
  return(combined_mask)
}

safe_mean <- function(vals) {
  if (length(vals) == 0) return(NA_real_)
  mean(vals, na.rm = TRUE)
}

# ─────────────────────────────────────────────
# SITS FUNCTIONS: DOWNLOAD, PREPARE & CACHE
# ─────────────────────────────────────────────

download_images_and_prepare_data <- function(vector_file, start_date, end_date) {
  roi_native <- st_read(vector_file)
  roi_wgs84  <- st_transform(roi_native, 4326)
  roi_bbox   <- st_as_sfc(st_bbox(roi_wgs84))
  
  # Only download exactly what the model needs
  cube <- sits_cube(
    source     = "AWS",
    collection = "SENTINEL-2-L2A",
    roi        = roi_bbox,
    bands      = c("B03", "B04", "B05", "B08", "CLOUD"),
    start_date = start_date,
    end_date   = end_date
  )
  
  raw_data_dir <- "data/raw-data"
  if (!dir.exists(raw_data_dir)) dir.create(raw_data_dir, recursive = TRUE)
  
  data_cube <- sits_cube_copy(
    cube,
    roi        = roi_bbox,
    output_dir = raw_data_dir
  )
  
  return(data_cube)
}

check_existing_raw_data <- function(raw_data_dir, year) {
  if (!dir.exists(raw_data_dir)) return(NULL)
  
  all_tifs  <- list.files(raw_data_dir, pattern = "\\.tif$", full.names = TRUE)
  year_tifs <- all_tifs[grepl(as.character(year), basename(all_tifs))]
  
  if (length(year_tifs) == 0) return(NULL)
  
  cat("Found", length(year_tifs), "existing raw .tif files for year", year, "- reloading from disk\n")
  
  cube <- sits::sits_cube(
    source     = "AWS",
    collection = "SENTINEL-2-L2A",
    data_dir   = raw_data_dir,
    parse_info = c("satellite", "sensor", "tile", "band", "date")
  )
  
  cube$file_info <- lapply(cube$file_info, function(fi) {
    fi[grepl(as.character(year), basename(fi$path)), ]
  })
  
  return(cube)
}

check_existing_reg_data <- function(reg_data_dir, year) {
  if (!dir.exists(reg_data_dir)) return(NULL)
  
  all_tifs  <- list.files(reg_data_dir, pattern = "\\.tif$", full.names = TRUE)
  year_tifs <- all_tifs[grepl(as.character(year), basename(all_tifs))]
  
  if (length(year_tifs) == 0) return(NULL)
  
  cat("Found", length(year_tifs), "existing REGULARIZED .tif files for year", year, "- bypassing download and regularization\n")
  
  cube <- sits::sits_cube(
    source     = "AWS",
    collection = "SENTINEL-2-L2A",
    data_dir   = reg_data_dir,
    parse_info = c("satellite", "sensor", "tile", "band", "date")
  )
  
  cube$file_info <- lapply(cube$file_info, function(fi) {
    fi[grepl(as.character(year), basename(fi$path)), ]
  })
  
  return(cube)
}

regularize_cube <- function(satellite_images) {
  reg_data_dir <- "data/regularized-data"
  if (!dir.exists(reg_data_dir)) dir.create(reg_data_dir, recursive = TRUE)
  
  sits_regularize(
    satellite_images,
    period     = "P5D",
    res        = 10,
    output_dir = reg_data_dir
  )
}

# ─────────────────────────────────────────────
# EXTRACTION FUNCTIONS: STATS & INDICES
# ─────────────────────────────────────────────

compute_polygon_stats <- function(satellite_images_reg, my_polygons,
                                  scl_clear_values = c(4, 5, 6)) {
  cube_files   <- dplyr::bind_rows(satellite_images_reg$file_info)
  unique_dates <- as.character(unique(cube_files$date))
  
  # Only B04_mean is required by the model
  bands_of_interest <- c("B04")
  
  my_polygons_ids  <- my_polygons %>% mutate(polygon_id = row_number())
  my_polygons_spat <- terra::vect(my_polygons_ids)
  
  stats_records  <- list()
  record_counter <- 1
  
  for (dt in unique_dates) {
    cat("Computing band statistics for date:", dt, "...\n")
    
    band_rasters <- list()
    for (b in bands_of_interest) {
      paths <- cube_files %>% filter(as.character(date) == dt, band == b) %>% pull(path)
      band_rasters[[b]] <- build_band(paths)
    }
    
    if (any(sapply(band_rasters, is.null))) {
      next
    }
    
    cloud_paths  <- cube_files %>% filter(as.character(date) == dt, band == "CLOUD") %>% pull(path)
    cloud_raster <- if (length(cloud_paths) > 0) build_band(cloud_paths) else NULL
    
    poly_proj <- terra::project(my_polygons_spat, terra::crs(band_rasters[[1]]))
    
    for (i in 1:nrow(poly_proj)) {
      poly_single <- poly_proj[i, ]
      pid         <- poly_single$polygon_id
      
      row_result <- tryCatch({
        ref_cropped <- terra::crop(band_rasters[[1]], poly_single)
        combined_mask <- get_combined_mask(ref_cropped, poly_single, cloud_raster, scl_clear_values)
        
        band_stats <- list()
        for (b in bands_of_interest) {
          band_cropped <- terra::crop(band_rasters[[b]], poly_single)
          band_masked  <- terra::mask(band_cropped, combined_mask)
          vals <- terra::values(band_masked, na.rm = TRUE)
          
          band_stats[[paste0(b, "_mean")]] <- safe_mean(vals)
        }
        
        as.data.frame(c(
          list(polygon_id = pid, Index = dt),
          band_stats
        ))
        
      }, error = function(e) { NULL })
      
      if (!is.null(row_result)) {
        stats_records[[record_counter]] <- row_result
        record_counter <- record_counter + 1
      }
    }
  }
  
  polygon_summary_stats <- dplyr::bind_rows(stats_records)
  return(polygon_summary_stats)
}

generate_image_metadata <- function(satellite_images_reg, vector_file,
                                    polygon_summary_stats,
                                    scl_clear_values = c(4, 5, 6)) {
  
  cube_files   <- dplyr::bind_rows(satellite_images_reg$file_info)
  unique_dates <- as.character(unique(cube_files$date))
  
  my_polygons_spat <- terra::vect(
    st_read(vector_file) %>% mutate(parcel_id = row_number())
  )
  
  image_records    <- list()
  image_id_counter <- 1
  
  for (dt in unique_dates) {
    paths_NIR  <- cube_files %>% filter(as.character(date) == dt, band == "B08") %>% pull(path)
    paths_RED  <- cube_files %>% filter(as.character(date) == dt, band == "B04") %>% pull(path)
    paths_GREEN<- cube_files %>% filter(as.character(date) == dt, band == "B03") %>% pull(path)
    paths_RE   <- cube_files %>% filter(as.character(date) == dt, band == "B05") %>% pull(path)
    
    if (length(paths_NIR) == 0 || length(paths_RED) == 0) {
      next
    }
    
    band_NIR   <- build_band(paths_NIR)
    band_RED   <- build_band(paths_RED)
    band_GREEN <- if (length(paths_GREEN) > 0) build_band(paths_GREEN) else NULL
    band_RE    <- if (length(paths_RE)    > 0) build_band(paths_RE)    else NULL
    
    ndvi_scene  <- (band_NIR - band_RED) / (band_NIR + band_RED)
    ndre_scene  <- if (!is.null(band_RE)) { (band_NIR - band_RE) / (band_NIR + band_RE) } else NULL
    gndvi_scene <- if (!is.null(band_GREEN)) { (band_NIR - band_GREEN) / (band_NIR + band_GREEN) } else NULL
    ndwi_scene <- if (!is.null(band_GREEN)) { (band_GREEN - band_NIR) / (band_GREEN + band_NIR) } else NULL
    nirv_scene  <- band_NIR * ndvi_scene
    
    cloud_paths  <- cube_files %>% filter(as.character(date) == dt, band == "CLOUD") %>% pull(path)
    cloud_raster <- if (length(cloud_paths) > 0) build_band(cloud_paths) else NULL
    
    poly_proj <- terra::project(my_polygons_spat, terra::crs(ndvi_scene))
    
    for (i in 1:nrow(poly_proj)) {
      poly_single <- poly_proj[i, ]
      pid         <- poly_single$parcel_id
      
      summarize_index <- function(index_scene) {
        if (is.null(index_scene)) return(c(mean = NA_real_, n_valid = 0))
        tryCatch({
          cropped       <- terra::crop(index_scene, poly_single)
          combined_mask <- get_combined_mask(cropped, poly_single, cloud_raster, scl_clear_values)
          masked        <- terra::mask(cropped, combined_mask)
          vals          <- terra::values(masked, na.rm = TRUE)
          c(mean = safe_mean(vals), n_valid = length(vals))
        }, error = function(e) c(mean = NA_real_, n_valid = 0))
      }
      
      ndvi_res  <- summarize_index(ndvi_scene)
      ndre_res  <- summarize_index(ndre_scene)
      gndvi_res <- summarize_index(gndvi_scene)
      ndwi_res  <- summarize_index(ndwi_scene)
      nirv_res  <- summarize_index(nirv_scene)
      
      discarded_flag <- if (ndvi_res[["n_valid"]] == 0) "yes" else NA_character_
      
      image_records[[image_id_counter]] <- data.frame(
        image_id              = paste0("IMG_", sprintf("%05d", image_id_counter)),
        parcel_id             = pid,
        image_date            = dt,
        ndvi_mean             = round(ndvi_res[["mean"]], 4),
        ndre_mean             = round(ndre_res[["mean"]], 4),
        gndvi_mean            = round(gndvi_res[["mean"]], 4),
        ndwi_mean             = round(ndwi_res[["mean"]], 4),
        nirv_mean             = round(nirv_res[["mean"]], 4),
        discarded             = discarded_flag,
        stringsAsFactors      = FALSE
      )
      image_id_counter <- image_id_counter + 1
    }
  }
  
  image_metadata <- dplyr::bind_rows(image_records)
  
  poly_stats_renamed <- polygon_summary_stats %>%
    rename(image_date = Index, parcel_id = polygon_id) %>%
    mutate(image_date = as.character(image_date))
  
  image_metadata_full <- image_metadata %>%
    left_join(poly_stats_renamed, by = c("parcel_id", "image_date"))
  
  return(image_metadata_full)
}

# ─────────────────────────────────────────────
# RUN: Download imagery, regularize, extract metadata
#   Expects `yr` and `input_vector_file` to be defined in the
#   calling environment (set in main.R before sourcing this file).
#   Produces `image_metadata` in the calling environment.
# ─────────────────────────────────────────────

run_data_pipeline <- function(yr, input_vector_file) {
  start_date   <- paste0(yr, "-03-01")
  end_date     <- paste0(yr, "-05-15")
  raw_data_dir <- "data/raw-data"
  reg_data_dir <- "data/regularized-data"
  
  cat("\n===== 1. Starting Data Pipeline for year", yr, "=====\n")
  
  # Check for existing regularized data first
  existing_reg_cube <- check_existing_reg_data(reg_data_dir, yr)
  
  if (!is.null(existing_reg_cube)) {
    satellite_images_reg <- existing_reg_cube
  } else {
    # If no regularized data, check for raw data
    existing_raw_cube <- check_existing_raw_data(raw_data_dir, yr)
    
    if (!is.null(existing_raw_cube)) {
      satellite_images <- existing_raw_cube
    } else {
      satellite_images <- download_images_and_prepare_data(
        input_vector_file, start_date, end_date
      )
    }
    
    # Regularize the raw data
    satellite_images_reg <- regularize_cube(satellite_images)
  }
  
  # Extract features
  my_polygons           <- st_read(input_vector_file)
  polygon_summary_stats <- compute_polygon_stats(satellite_images_reg, my_polygons)
  
  # Generate metadata
  image_metadata <- generate_image_metadata(
    satellite_images_reg,
    input_vector_file,
    polygon_summary_stats
  )
  
  return(image_metadata)
}