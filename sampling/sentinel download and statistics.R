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