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

suggest_label <- function(ndvi_mean) {
  dplyr::case_when(
    is.na(ndvi_mean) ~ "no_data",
    ndvi_mean > 0.7  ~ "green",
    ndvi_mean > 0.6  ~ "slightly_yellow",
    ndvi_mean > 0.3  ~ "yellow",
    TRUE             ~ "ploughed"
  )
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
    bands      = c("B02", "B03", "B04", "B05", "B08"),
    start_date = start_date,
    end_date   = end_date
  )
  
  data_cube <- sits_cube_copy(
    cube,
    roi        = roi_bbox,
    output_dir = "C:/remote_sensing_Advanced_EO/data"
  )
  
  return(data_cube)
}

# ─────────────────────────────────────────────
# FUNCTION 2: Regularize the satellite data cube
# ─────────────────────────────────────────────

regularize_cube <- function(satellite_images) {
  sits_regularize(
    satellite_images,
    period     = "P5D",
    res        = 10,
    output_dir = "C:/remote_sensing_Advanced_EO/regularized"
  )
}

# ─────────────────────────────────────────────
# FUNCTION 3: Compute per-polygon band statistics
# ─────────────────────────────────────────────

compute_polygon_stats <- function(satellite_images_reg, my_polygons) {
  polygon_time_series <- sits_get_data(
    cube    = satellite_images_reg,
    samples = my_polygons
  )
  
  flat_time_series <- polygon_time_series %>%
    tidyr::unnest(cols = time_series)
  
  polygon_summary_stats <- flat_time_series %>%
    group_by(polygon_id, Index) %>%
    summarise(
      B02_mean = mean(B02, na.rm = TRUE), B02_sd = sd(B02, na.rm = TRUE),
      B03_mean = mean(B03, na.rm = TRUE), B03_sd = sd(B03, na.rm = TRUE),
      B04_mean = mean(B04, na.rm = TRUE), B04_sd = sd(B04, na.rm = TRUE),
      B05_mean = mean(B05, na.rm = TRUE), B05_sd = sd(B05, na.rm = TRUE),
      B08_mean = mean(B08, na.rm = TRUE), B08_sd = sd(B08, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(polygon_summary_stats)
}

# ─────────────────────────────────────────────
# FUNCTION 4: Extract and save RGB image patches per polygon per date
# ─────────────────────────────────────────────

extract_rgb_patches <- function(satellite_images_reg, my_polygons, base_img_dir) {
  if (!dir.exists(base_img_dir)) dir.create(base_img_dir, recursive = TRUE)
  
  cube_files      <- dplyr::bind_rows(satellite_images_reg$file_info)
  my_polygons_spat <- terra::vect(my_polygons)
  unique_dates    <- unique(cube_files$date)
  
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
      
      poly_spat_proj <- terra::project(my_polygons_spat, terra::crs(scene_RGB))
      
      for (i in 1:nrow(poly_spat_proj)) {
        poly_single <- poly_spat_proj[i, ]
        pid         <- poly_single$polygon_id
        
        plot_dir <- file.path(base_img_dir, paste0("plot_", pid))
        if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
        
        out_tiff <- file.path(plot_dir, paste0("RGB_", dt, ".tif"))
        out_png  <- file.path(plot_dir, paste0("RGB_", dt, ".png"))
        
        tryCatch({
          plot_cropped    <- terra::crop(scene_RGB, poly_single)
          plot_RGB        <- terra::mask(plot_cropped, poly_single)
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
                                    polygon_summary_stats, output_path) {
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
    
    poly_proj <- terra::project(my_polygons_spat, terra::crs(ndvi_scene))
    
    for (i in 1:nrow(poly_proj)) {
      poly_single <- poly_proj[i, ]
      pid         <- poly_single$parcel_id
      
      ndvi_vals <- tryCatch({
        cropped <- terra::crop(ndvi_scene, poly_single)
        masked  <- terra::mask(cropped, poly_single)
        vals    <- terra::values(masked, na.rm = TRUE)
        list(mean = mean(vals), sd = sd(vals))
      }, error = function(e) list(mean = NA_real_, sd = NA_real_))
      
      image_records[[image_id_counter]] <- data.frame(
        image_id              = paste0("IMG_", sprintf("%05d", image_id_counter)),
        parcel_id             = pid,
        image_date            = dt,
        file_path             = file.path("data", paste0("plot_", pid),
                                          paste0("RGB_", dt, ".tif")),
        ndvi_mean             = round(ndvi_vals$mean, 4),
        ndvi_sd               = round(ndvi_vals$sd,   4),
        suggested_class_label = suggest_label(ndvi_vals$mean),
        class_label           = NA_character_,
        discarded             = NA_character_,
        stringsAsFactors      = FALSE
      )
      image_id_counter <- image_id_counter + 1
    }
    cat("NDVI computed for date:", dt, "\n")
  }
  
  image_metadata <- dplyr::bind_rows(image_records)
  
  # Merge with polygon band summary stats
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
# MAIN: Call all functions in order
# ═════════════════════════════════════════════

input_vector_file <- "C:/remote_sensing_Advanced_EO/dummyData.gpkg"
base_img_dir      <- "C:/remote_sensing_Advanced_EO/images"
start_date        <- "2020-03-23"
end_date          <- "2020-05-07"

# 1. Download
satellite_images <- download_images_and_prepare_data(
  input_vector_file, start_date, end_date
)

# 2. Regularize
satellite_images_reg <- regularize_cube(satellite_images)

# 3. Read polygons and compute band statistics
my_polygons          <- st_read(input_vector_file)
polygon_summary_stats <- compute_polygon_stats(satellite_images_reg, my_polygons)

# 4. Extract RGB image patches
my_polygons_with_id <- st_read(input_vector_file) %>% mutate(polygon_id = row_number())
extract_rgb_patches(satellite_images_reg, my_polygons_with_id, base_img_dir)

# 5. Generate parcel metadata CSV
parcel_metadata <- generate_parcel_metadata(
  input_vector_file,
  "C:/remote_sensing_Advanced_EO/parcel_metadata.csv"
)

# 6. Generate image metadata CSV
image_metadata <- generate_image_metadata(
  satellite_images_reg,
  input_vector_file,
  polygon_summary_stats,
  "C:/remote_sensing_Advanced_EO/image_metadata.csv"
)

cat("\nAll operations complete!\n")