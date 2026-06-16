library(sits)
library(sf)
library(httr2)
library(tidyverse)
library(terra)
library(dplyr)

# ─────────────────────────────────────────────
# HELPER FUNCTIONS
# ─────────────────────────────────────────────

build_band <- function(paths) {
  paths <- paths[!is.na(paths) & nchar(paths) > 0]
  if (length(paths) == 0) return(NULL)
  if (length(paths) == 1) return(terra::rast(paths))
  rast_list <- lapply(paths, terra::rast)
  return(terra::mosaic(terra::sprc(rast_list)))
}

# Returns a single-layer raster matching `raster_template`'s grid:
# 1 where a pixel is (almost) entirely within poly_single, NA elsewhere.
get_full_pixel_mask <- function(raster_template, poly_single, threshold = 0.999) {
  cov_frac  <- terra::rasterize(poly_single, raster_template, cover = TRUE)
  full_mask <- terra::ifel(cov_frac >= threshold, 1, NA)
  return(full_mask)
}

# Combines the full-pixel-coverage mask with a cloud-free (SCL) mask.
# raster_template: an already-cropped raster defining the target grid/extent.
# cloud_raster: the full-scene SCL raster (NOT yet cropped), or NULL if unavailable.
get_combined_mask <- function(raster_template, poly_single, cloud_raster = NULL,
                              scl_clear_values = c(4, 5, 6, 7)) {
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

# Mean/SD that don't blow up (return NaN) when there are zero valid pixels
safe_mean_sd <- function(vals) {
  if (length(vals) == 0) return(list(mean = NA_real_, sd = NA_real_))
  list(mean = mean(vals), sd = sd(vals))
}

# ─────────────────────────────────────────────
# FUNCTION 1: Download and prepare satellite data
# ─────────────────────────────────────────────

download_images_and_prepare_data <- function(vector_file, start_date, end_date) {
  roi_native <- st_read(vector_file)
  roi_wgs84  <- st_transform(roi_native, 4326)
  roi_bbox   <- st_as_sfc(st_bbox(roi_wgs84))
  
  cube <- sits_cube(
    source     = "AWS",
    collection = "SENTINEL-2-L2A",
    roi        = roi_bbox,
    bands      = c("B02", "B03", "B04", "B05", "B08", "CLOUD"),  # CLOUD = SCL mask
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

# ─────────────────────────────────────────────
# FUNCTION 1b: Check for existing raw data and reload as a sits cube
# ─────────────────────────────────────────────

check_existing_raw_data <- function(raw_data_dir, year) {
  if (!dir.exists(raw_data_dir)) return(NULL)
  
  all_tifs  <- list.files(raw_data_dir, pattern = "\\.tif$", full.names = TRUE)
  year_tifs <- all_tifs[grepl(as.character(year), basename(all_tifs))]
  
  if (length(year_tifs) == 0) return(NULL)
  
  cat("Found", length(year_tifs), "existing .tif files for year", year, "- reloading from disk\n")
  
  # Reload the cached raw files as a proper sits cube object
  # (not just a file_info table) so sits_regularize() can still process it
  cube <- sits::sits_cube(
    source     = "AWS",
    collection = "SENTINEL-2-L2A",
    data_dir   = raw_data_dir,
    parse_info = c("satellite", "sensor", "tile", "band", "date")
  )
  
  # Keep only the rows relevant to this year, since data_dir may contain
  # files from multiple years mixed together
  cube$file_info <- lapply(cube$file_info, function(fi) {
    fi[grepl(as.character(year), basename(fi$path)), ]
  })
  
  return(cube)
}

# ─────────────────────────────────────────────
# FUNCTION 2: Regularize the satellite data cube
# ─────────────────────────────────────────────

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
# FUNCTION 3: Compute per-polygon band statistics
#   - only pixels entirely within the polygon
#   - only pixels classified as "clear" by the SCL cloud mask
# ─────────────────────────────────────────────

compute_polygon_stats <- function(satellite_images_reg, my_polygons,
                                  scl_clear_values = c(4, 5, 6, 7)) {
  cube_files   <- dplyr::bind_rows(satellite_images_reg$file_info)
  unique_dates <- as.character(unique(cube_files$date))
  bands_of_interest <- c("B02", "B03", "B04", "B05", "B08")
  
  cat("Bands available in cube_files:", paste(unique(cube_files$band), collapse = ", "), "\n")
  
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
      cat("  -> Skipping date", dt, "- missing one or more spectral bands\n")
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
          
          ms <- safe_mean_sd(vals)
          band_stats[[paste0(b, "_mean")]] <- ms$mean
          band_stats[[paste0(b, "_sd")]]   <- ms$sd
        }
        
        as.data.frame(c(
          list(polygon_id = pid, Index = dt),
          band_stats
        ))
        
      }, error = function(e) {
        cat("  -> Polygon", pid, "skipped on", dt, ":", conditionMessage(e), "\n")
        NULL
      })
      
      if (!is.null(row_result)) {
        stats_records[[record_counter]] <- row_result
        record_counter <- record_counter + 1
      }
    }
  }
  
  polygon_summary_stats <- dplyr::bind_rows(stats_records)
  return(polygon_summary_stats)
}

# ─────────────────────────────────────────────
# FUNCTION 4: Extract and save RGB image patches per polygon per date
#   - only pixels entirely within the polygon
#   - only pixels classified as "clear" by the SCL cloud mask
# ─────────────────────────────────────────────

extract_rgb_patches <- function(satellite_images_reg, my_polygons, base_img_dir,
                                scl_clear_values = c(4, 5, 6, 7)) {
  if (!dir.exists(base_img_dir)) dir.create(base_img_dir, recursive = TRUE)
  
  cube_files       <- dplyr::bind_rows(satellite_images_reg$file_info)
  my_polygons_spat <- terra::vect(my_polygons)
  unique_dates     <- unique(cube_files$date)
  
  cat("Total file entries across all tiles:", nrow(cube_files), "\n")
  cat("Unique tiles:", length(satellite_images_reg$file_info), "\n")
  cat("Starting RGB patch extraction...\n")
  
  for (dt in as.character(unique_dates)) {
    cat("\nProcessing date:", dt, "...\n")
    
    paths_R <- cube_files %>% filter(as.character(date) == dt, band == "B04") %>% pull(path)
    paths_G <- cube_files %>% filter(as.character(date) == dt, band == "B03") %>% pull(path)
    paths_B <- cube_files %>% filter(as.character(date) == dt, band == "B02") %>% pull(path)
    
    cat("  Tiles found -> R:", length(paths_R),
        "G:", length(paths_G),
        "B:", length(paths_B), "\n")
    
    if (length(paths_R) >= 1 && length(paths_G) >= 1 && length(paths_B) >= 1) {
      
      scene_RGB <- c(build_band(paths_R), build_band(paths_G), build_band(paths_B))
      names(scene_RGB) <- c("Red", "Green", "Blue")
      
      # Cloud mask for this date
      cloud_paths  <- cube_files %>% filter(as.character(date) == dt, band == "CLOUD") %>% pull(path)
      cloud_raster <- if (length(cloud_paths) > 0) build_band(cloud_paths) else NULL
      
      poly_spat_proj <- terra::project(my_polygons_spat, terra::crs(scene_RGB))
      
      for (i in 1:nrow(poly_spat_proj)) {
        poly_single <- poly_spat_proj[i, ]
        pid         <- poly_single$polygon_id
        
        plot_dir <- file.path(base_img_dir, paste0("plot_", pid))
        if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
        
        out_tiff <- file.path(plot_dir, paste0("RGB_", dt, ".tif"))
        out_png  <- file.path(plot_dir, paste0("RGB_", dt, ".png"))
        
        tryCatch({
          plot_cropped  <- terra::crop(scene_RGB, poly_single)
          combined_mask <- get_combined_mask(plot_cropped, poly_single, cloud_raster, scl_clear_values)
          plot_RGB      <- terra::mask(plot_cropped, combined_mask)
          
          terra::writeRaster(plot_RGB, out_tiff, overwrite = TRUE)
          
          plot_png_scaled <- terra::clamp(plot_RGB, 0, 3000)
          plot_png_scaled <- (plot_png_scaled / 3000) * 255
          plot_png_large  <- terra::disagg(plot_png_scaled, fact = 20, method = "near")
          terra::writeRaster(plot_png_large, out_png, overwrite = TRUE, datatype = "INT1U")
          
        }, error = function(e) {
          cat("  -> Plot", pid, "skipped on", dt, ":", conditionMessage(e), "\n")
        })
      }
      
    } else {
      warning(paste("Could not find complete RGB paths for date:", dt))
    }
  }
  cat("\nRGB patch extraction complete!\n")
}

# ─────────────────────────────────────────────
# FUNCTION 5: Generate parcel metadata CSV
# ─────────────────────────────────────────────

generate_parcel_metadata <- function(vector_file, output_path) {
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  my_polygons_meta <- st_read(vector_file) %>%
    st_drop_geometry() %>%
    mutate(feature_id = row_number())
  
  cat("Available columns in geopackage:\n")
  print(names(my_polygons_meta))
  
  parcel_metadata <- my_polygons_meta %>%
    mutate(
      folder_name = paste0("plot_", feature_id),
      glyphosate  = NA,
      discarded   = NA
    ) %>%
    rename(brp_id = parcel_id) %>%
    select(parcel_id = feature_id, brp_id, folder_name, glyphosate, discarded)
  
  write.csv(parcel_metadata, output_path, row.names = FALSE, na = "")
  cat("parcel_metadata.csv written:", nrow(parcel_metadata), "parcels\n")
  
  return(parcel_metadata)
}

# ─────────────────────────────────────────────
# FUNCTION 6: Generate image metadata CSV with NDVI and band stats
# ─────────────────────────────────────────────

generate_image_metadata <- function(satellite_images_reg, vector_file,
                                    polygon_summary_stats, output_path,
                                    scl_clear_values = c(4, 5, 6, 7)) {
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  cube_files   <- dplyr::bind_rows(satellite_images_reg$file_info)
  unique_dates <- as.character(unique(cube_files$date))
  
  my_polygons_spat <- terra::vect(
    st_read(vector_file) %>% mutate(parcel_id = row_number())
  )
  
  image_records    <- list()
  image_id_counter <- 1
  
  for (dt in unique_dates) {
    paths_NIR <- cube_files %>% filter(as.character(date) == dt, band == "B08") %>% pull(path)
    paths_RED <- cube_files %>% filter(as.character(date) == dt, band == "B04") %>% pull(path)
    
    if (length(paths_NIR) == 0 || length(paths_RED) == 0) {
      cat("Skipping NDVI for date", dt, "- missing B08 or B04\n")
      next
    }
    
    ndvi_scene <- (build_band(paths_NIR) - build_band(paths_RED)) /
      (build_band(paths_NIR) + build_band(paths_RED))
    
    # Cloud mask for this date
    cloud_paths  <- cube_files %>% filter(as.character(date) == dt, band == "CLOUD") %>% pull(path)
    cloud_raster <- if (length(cloud_paths) > 0) build_band(cloud_paths) else NULL
    
    poly_proj <- terra::project(my_polygons_spat, terra::crs(ndvi_scene))
    
    for (i in 1:nrow(poly_proj)) {
      poly_single <- poly_proj[i, ]
      pid         <- poly_single$parcel_id
      
      result <- tryCatch({
        cropped       <- terra::crop(ndvi_scene, poly_single)
        combined_mask <- get_combined_mask(cropped, poly_single, cloud_raster, scl_clear_values)
        masked        <- terra::mask(cropped, combined_mask)
        vals          <- terra::values(masked, na.rm = TRUE)
        
        c(safe_mean_sd(vals), list(n_valid = length(vals)))
      }, error = function(e) list(mean = NA_real_, sd = NA_real_, n_valid = 0))
      
      ndvi_vals <- list(mean = result$mean, sd = result$sd)
      
      # Suggested class label based on NDVI thresholds
      suggested_label <- dplyr::case_when(
        is.na(ndvi_vals$mean) ~ "no_data",
        ndvi_vals$mean > 0.7  ~ "green",
        ndvi_vals$mean > 0.6  ~ "slightly_yellow",
        ndvi_vals$mean > 0.3  ~ "yellow",
        TRUE                  ~ "ploughed"
      )
      
      # Mark as discarded if no valid (fully-covered, cloud-free) pixels exist
      discarded_flag <- if (result$n_valid == 0) "yes" else NA_character_
      
      image_records[[image_id_counter]] <- data.frame(
        image_id              = paste0("IMG_", sprintf("%05d", image_id_counter)),
        parcel_id             = pid,
        image_date            = dt,
        file_path             = file.path("data", paste0("plot_", pid),
                                          paste0("RGB_", dt, ".tif")),
        ndvi_mean             = round(ndvi_vals$mean, 4),
        ndvi_sd               = round(ndvi_vals$sd,   4),
        suggested_class_label = suggested_label,
        class_label           = NA_character_,
        discarded             = discarded_flag,
        stringsAsFactors      = FALSE
      )
      image_id_counter <- image_id_counter + 1
    }
    cat("NDVI computed for date:", dt, "\n")
  }
  
  image_metadata <- dplyr::bind_rows(image_records)
  
  poly_stats_renamed <- polygon_summary_stats %>%
    rename(image_date = Index,
           parcel_id  = polygon_id) %>%
    mutate(image_date = as.character(image_date))
  
  image_metadata_full <- image_metadata %>%
    left_join(poly_stats_renamed, by = c("parcel_id", "image_date"))
  
  write.csv(image_metadata_full, output_path, row.names = FALSE, na = "")
  cat("image_metadata.csv written:", nrow(image_metadata_full), "rows\n")
  cat("Columns:", paste(names(image_metadata_full), collapse = ", "), "\n")
  
  return(image_metadata_full)
}

# ═════════════════════════════════════════════
# MAIN: Call all functions in order for each year
# ═════════════════════════════════════════════

years    <- c(2020, 2025)
data_dir <- "../sampling/samples"

parcel_files <- file.path(data_dir, paste0("sampled_parcels_", years, ".gpkg"))
start_dates  <- paste0(years, "-03-23")
end_dates    <- paste0(years, "-05-07")

for (idx in seq_along(years)) {
  yr <- years[idx]
  
  input_vector_file <- parcel_files[idx]
  base_img_dir       <- paste0("images_", yr)
  start_date         <- start_dates[idx]
  end_date           <- end_dates[idx]
  raw_data_dir       <- "data/raw-data"
  
  cat("\n===== Starting pipeline for year", yr, "=====\n")
  
  # 1. Check for existing raw data first; reload it if found, otherwise download
  existing_cube <- check_existing_raw_data(raw_data_dir, yr)
  
  if (!is.null(existing_cube)) {
    satellite_images <- existing_cube
  } else {
    satellite_images <- download_images_and_prepare_data(
      input_vector_file, start_date, end_date
    )
  }
  
  # 2. Regularize (always runs, regardless of source)
  satellite_images_reg <- regularize_cube(satellite_images)
  
  # 3. Read polygons and compute band statistics
  my_polygons           <- st_read(input_vector_file)
  polygon_summary_stats <- compute_polygon_stats(satellite_images_reg, my_polygons)
  
  # 4. Extract RGB image patches
  my_polygons_with_id <- st_read(input_vector_file) %>% mutate(polygon_id = row_number())
  extract_rgb_patches(satellite_images_reg, my_polygons_with_id, base_img_dir)
  
  # 5. Generate parcel metadata CSV
  parcel_metadata <- generate_parcel_metadata(
    input_vector_file,
    file.path("metadata", paste0("parcel_metadata", yr, ".csv"))
  )
  
  # 6. Generate image metadata CSV
  image_metadata <- generate_image_metadata(
    satellite_images_reg,
    input_vector_file,
    polygon_summary_stats,
    file.path("metadata", paste0("image_metadata", yr, ".csv"))
  )
  
  cat("\nAll", yr, "operations complete!\n")
}

cat("\n===== All years complete! =====\n")