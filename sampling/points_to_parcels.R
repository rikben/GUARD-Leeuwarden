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

# Setup
out_dir <- "data"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

#reading data for parcels, points, and the spatial grid
brp_parcels_2025 <- st_read("data/brp_dominant_soil_2025_simplified.gpkg")
brp_parcels_2020 <- st_read("data/brp_dominant_soil_2020_simplified.gpkg")

obs_2020 <- st_read("data/obs_2020.gpkg")
obs_2025 <- st_read("data/obs_2025.gpkg")

### TASK 1: Create version of BRP with "glyphosate" 0/1 column (for later work) ###
# Spatial intersection: for each parcel, list matching observation indices
idx_2020 <- st_intersects(brp_parcels_2020, obs_2020)
idx_2025 <- st_intersects(brp_parcels_2025, obs_2025)

# Create binary presence/absence flag
brp_parcels_2020$glyphosate <- as.integer(lengths(idx_2020) > 0)
brp_parcels_2025$glyphosate <- as.integer(lengths(idx_2025) > 0)

# writing output of BRP parcels, now WITH glyphosate column
st_write(brp_parcels_2020,
         "data/brp_parcels_2020.gpkg",
         delete_dsn = TRUE)

st_write(brp_parcels_2025,
         "data/brp_parcels_2025.gpkg",
         delete_dsn = TRUE)
#----------------------------------------------------------------------------------

### TASK 2: Make a subset of brp parcels of only those that had observation ###
parcels_2020_intersect <- brp_parcels_2020[
  brp_parcels_2020$glyphosate == 1,
]

parcels_2025_intersect <- brp_parcels_2025[
  brp_parcels_2025$glyphosate == 1,
]
#----------------------------------------------------------------------------------

### TASK 3: Remove the 10% of smallest parcels to remove odd geometries and insufficiently small parcels ###

#compute 10th percentile threshold
# (ONLY on intersected parcels)
area_threshold_2020 <- quantile(
  parcels_2020_intersect$parcel_area_m2,
  probs = 0.10,
  na.rm = TRUE
)

area_threshold_2025 <- quantile(
  parcels_2025_intersect$parcel_area_m2,
  probs = 0.10,
  na.rm = TRUE
)
#remove smallest 10% parcels
parcels_2020_observations <- parcels_2020_intersect[
  parcels_2020_intersect$parcel_area_m2 >= area_threshold_2020,
]

parcels_2025_observations <- parcels_2025_intersect[
  parcels_2025_intersect$parcel_area_m2 >= area_threshold_2025,
]

#write outputs (parcels that had observation)
st_write(parcels_2025_observations,
         "data/parcels_2025_observations.gpkg",
         delete_dsn = TRUE)

st_write(parcels_2020_observations,
         "data/parcels_2020_observations.gpkg",
         delete_dsn = TRUE)


#analysis function
analyze_parcels <- function(parcels, year) {
  
  q10 <- quantile(parcels$parcel_area_m2, 0.10, na.rm = TRUE)
  
  cat("\n---", year, "---\n")
  cat("Number of parcels:", nrow(parcels), "\n")
  cat("Mean area (m²):", round(mean(parcels$parcel_area_m2, na.rm = TRUE), 1), "\n")
  cat("Median area (m²):", round(median(parcels$parcel_area_m2, na.rm = TRUE), 1), "\n")
  cat("10th percentile (m²):", round(q10, 1), "\n")
  cat("Min area (m²):", min(parcels$parcel_area_m2, na.rm = TRUE), "\n")
  
  # plot
  hist(
    parcels$parcel_area_m2,
    breaks = 50,
    main = paste("Parcel area distribution", year),
    xlab = "Area (m²)"
  )
  
  return(q10)
}

# run analysis
q10_2020 <- analyze_parcels(parcels_2020_observations, 2020)
q10_2025 <- analyze_parcels(parcels_2025_observations, 2025)


