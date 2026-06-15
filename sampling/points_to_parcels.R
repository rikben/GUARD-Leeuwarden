# Script that takes all BRP parcels and links them to all Waarnemingen observations, 2020 and 2025.

# Because the observers sometimes log the fields from the road (even though they
# mostly drag the marker to middle of correct field); this script runs an intersection 
# of glyphosate observations with parcels and only keep parcels which have an observation directly 
# on them, thus losing a few parcels for training, but avoiding having to figure out which 
# field was meant by a user who was stationary on road between multiple fields.

# Output: 
# 1) GPKG with only those parcels that had an observation (intersect) directly on them (named as "parcels_2020_observations"), also filtered by area (smallest 10% out) 
# 2) new GPKG version of all BRP parcels with added glyphosate 0/1 column (named as "brp_parcels_2020")

# assumes all parcels and observations files are in "data" dir, if not, we need some function here that sources those
# from the other scripts (which are Waarnemingen_obs_download.R and prepare_soil_brp.R)

library(sf)
library(dplyr)

#user input
year <- 2025

# Setup
out_dir <- "data"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

#function to call file paths
get_paths <- function(year) {
  list(
    parcels = paste0("data/brp_dominant_soil_", year, "_simplified.gpkg"),
    obs     = paste0("data/obs_", year, ".gpkg"),
    
    out_full_parcels = paste0("data/brp_parcels_", year, "_filtered.gpkg"),
    
    out_parcels = paste0("data/parcels_", year, "_intersect.gpkg"),
    out_obs     = paste0("data/obs_", year, "_intersect.gpkg")
  )
}

#main pipeline function
run_pipeline <- function(year) {
  
  paths <- get_paths(year)
  
  # Load data 
  parcels <- st_read(paths$parcels, quiet = TRUE)
  obs     <- st_read(paths$obs, quiet = TRUE)
  
  #task 1
  
  parcels <- st_read(paths$parcels, quiet = TRUE)
  obs     <- st_read(paths$obs, quiet = TRUE)
  
  # fix CRS mismatch
  if (st_crs(parcels) != st_crs(obs)) {
    obs <- st_transform(obs, st_crs(parcels))
  }
  
  idx <- st_intersects(parcels, obs)
  
  parcels$glyphosate <- as.integer(lengths(idx) > 0)
  
  parcels_g <- parcels[parcels$glyphosate == 1, ]
  parcels_n <- parcels[parcels$glyphosate == 0, ]
  
  thr_g <- quantile(parcels_g$parcel_area_m2, 0.10, na.rm = TRUE)
  thr_n <- quantile(parcels_n$parcel_area_m2, 0.10, na.rm = TRUE)
  
  parcels_g_f <- parcels_g[parcels_g$parcel_area_m2 >= thr_g, ]
  parcels_n_f <- parcels_n[parcels_n$parcel_area_m2 >= thr_n, ]
  
  parcels_final <- rbind(parcels_g_f, parcels_n_f)
  
  if (file.exists(paths$out_parcels)) {
    file.remove(paths$out_parcels)
  }
  
  
  st_write(parcels_final, paths$out_full_parcels, delete_dsn = TRUE)
  
}

run_pipeline(year)




