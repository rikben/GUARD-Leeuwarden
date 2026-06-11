library(sits)
library(sf)
library(httr2)
library(tidyverse) 

download_images_and_prepare_data <- function(vector_file, start_date, end_date) {
  # 1. Read the input vector file containing the 300 polygons
  roi_native <- st_read(vector_file)
  
  # 2. Transform the entire collection to WGS84
  roi_wgs84 <- st_transform(roi_native, 4326)
  
  # 3. Collapse 300 rows into 1 single bounding box polygon.
  # This provides a clean, single-geometry footprint for the STAC API.
  roi_bbox <- st_as_sfc(st_bbox(roi_wgs84))
  
  # 4. Define the data cube using the unified bounding box
  cube <- sits_cube(
    source = "AWS",
    collection = "SENTINEL-2-L2A",
    roi = roi_bbox, 
    bands = c("B02", "B03", "B04", "B05", "B08"), #RGB NIR and red edge band (5)
    start_date = start_date,
    end_date = end_date
  )
  
  # 5. Download the imagery covering the total bounding box area
  data_cube <- sits_cube_copy(
    cube, 
    roi = roi_bbox, 
    output_dir = "C:/remote_sensing_Advanced_EO/data"
  )
  
  return(data_cube)
}

# Define your parameters
input_vector_file <- "C:/remote_sensing_Advanced_EO/data.gpkg" 
start_date <- "2020-03-23"
end_date <- "2020-05-07"

# Run the function
satellite_images <- download_images_and_prepare_data(input_vector_file, start_date, end_date)




#Read your 300 polygons back into R
my_polygons <- st_read("C:/remote_sensing_Advanced_EO/data.gpkg")

satellite_images_reg <- sits_regularize(
  satellite_images,
  period = "P10D", # 5-day Sentinel-2 revisit
  res = 10,
  output_dir = "C:/remote_sensing_Advanced_EO/regularized"
)


# 1. Extract the raw pixel data (No extra agg_ arguments needed!)
polygon_time_series <- sits_get_data(
  cube    = satellite_images_reg,  
  samples = my_polygons
)

# 2. Flatten and calculate the Mean and SD manually
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

# 3. Export
write.csv(polygon_summary_stats, "C:/remote_sensing_Advanced_EO/polygon_stats_final.csv", row.names = FALSE)

#############
#Read your 300 polygons back into R
my_polygons <- st_read("C:/remote_sensing_Advanced_EO/data.gpkg")

satellite_images_reg <- sits_regularize(
  satellite_images,
  period = "P5D", # 5-day Sentinel-2 revisit
  res = 10,
  output_dir = "C:/remote_sensing_Advanced_EO/regularized"
)


# 1. Extract the raw pixel data (No extra agg_ arguments needed!)
polygon_time_series <- sits_get_data(
  cube    = satellite_images_reg,  
  samples = my_polygons
)

# 2. Flatten and calculate the Mean and SD manually
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

# 3. Export
write.csv(polygon_summary_stats, "C:/remote_sensing_Advanced_EO/polygon_stats_final5.csv", row.names = FALSE)


############################
# 1. Define the base directory for your images
base_img_dir <- "C:/remote_sensing_Advanced_EO/images"
if(!dir.exists(base_img_dir)) dir.create(base_img_dir, recursive = TRUE)

# 2. Combine file_info from ALL tiles in the cube (not just [[1]])
cube_files <- dplyr::bind_rows(satellite_images_reg$file_info)

# Quick sanity check — should now show multiple tiles per band/date
cat("Total file entries across all tiles:", nrow(cube_files), "\n")
cat("Unique tiles:", length(satellite_images_reg$file_info), "\n")

# 3. Read your 300 polygons natively
cat("Reading polygons...\n")
my_polygons <- st_read("C:/remote_sensing_Advanced_EO/data.gpkg") %>%
  mutate(polygon_id = row_number())

my_polygons_spat <- terra::vect(my_polygons)

unique_dates <- unique(cube_files$date)
cat("Starting RGB patch extraction for 300 plots...\n")

# 4. Helper function to mosaic ANY number of tiles for a single band/date
build_band <- function(paths) {
  paths <- paths[!is.na(paths) & nchar(paths) > 0]
  if (length(paths) == 0) return(NULL)
  if (length(paths) == 1) return(terra::rast(paths))
  rast_list <- lapply(paths, terra::rast)
  return(terra::mosaic(terra::sprc(rast_list)))
}

# 5. Loop through each individual date
for (dt in as.character(unique_dates)) {
  cat("\nProcessing date:", dt, "...\n")
  
  paths_R <- cube_files %>% filter(as.character(date) == dt, band == "B04") %>% pull(path)
  paths_G <- cube_files %>% filter(as.character(date) == dt, band == "B03") %>% pull(path)
  paths_B <- cube_files %>% filter(as.character(date) == dt, band == "B02") %>% pull(path)
  
  cat("  Tiles found -> R:", length(paths_R),
      "G:", length(paths_G),
      "B:", length(paths_B), "\n")
  
  if (length(paths_R) >= 1 && length(paths_G) >= 1 && length(paths_B) >= 1) {
    
    band_R <- build_band(paths_R)
    band_G <- build_band(paths_G)
    band_B <- build_band(paths_B)
    
    scene_RGB <- c(band_R, band_G, band_B)
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
        plot_cropped <- terra::crop(scene_RGB, poly_single)
        plot_RGB     <- terra::mask(plot_cropped, poly_single)
        
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

cat("\nAll operations complete!\n")
