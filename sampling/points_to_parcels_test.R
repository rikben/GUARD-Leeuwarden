library(sf)
library(dplyr)


# Setup
out_dir <- "data"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Read data
brp_parcels_2025 <- st_read("data/brp_dominant_soil_2025_simplified.gpkg")
brp_parcels_2020 <- st_read("data/brp_dominant_soil_2020_simplified.gpkg")

obs_2025 <- st_read("data/obs_2025.gpkg")
obs_2020 <- st_read("data/obs_2020.gpkg")

#keep only points that intersect parcels
# (cheap operation → reduces data early)
obs_2025_intersect <- st_filter(
  obs_2025,
  brp_parcels_2025,
  .predicate = st_intersects
)

obs_2020_intersect <- st_filter(
  obs_2020,
  brp_parcels_2020,
  .predicate = st_intersects
)

#keep only parcels that intersect remaining points
parcels_2025_intersect <- st_filter(
  brp_parcels_2025,
  obs_2025_intersect,
  .predicate = st_intersects
)

parcels_2020_intersect <- st_filter(
  brp_parcels_2020,
  obs_2020_intersect,
  .predicate = st_intersects
)

#compute 20th percentile threshold
# (ONLY on intersected parcels)
area_threshold_2025 <- quantile(
  parcels_2025_intersect$parcel_area_m2,
  probs = 0.20,
  na.rm = TRUE
)

area_threshold_2020 <- quantile(
  parcels_2020_intersect$parcel_area_m2,
  probs = 0.20,
  na.rm = TRUE
)


#remove smallest 20% parcels
parcels_2025_intersect <- parcels_2025_intersect[
  parcels_2025_intersect$parcel_area_m2 >= area_threshold_2025,
]

parcels_2020_intersect <- parcels_2020_intersect[
  parcels_2020_intersect$parcel_area_m2 >= area_threshold_2020,
]


#re-filter observations
# (ensures no orphan points remain)

obs_2025_intersect <- st_filter(
  obs_2025_intersect,
  parcels_2025_intersect,
  .predicate = st_intersects
)

obs_2020_intersect <- st_filter(
  obs_2020_intersect,
  parcels_2020_intersect,
  .predicate = st_intersects
)

#write outputs
st_write(obs_2025_intersect,
         "data/obs_2025_intersect.gpkg",
         delete_dsn = TRUE)

st_write(parcels_2025_intersect,
         "data/parcels_2025_intersect.gpkg",
         delete_dsn = TRUE)

st_write(obs_2020_intersect,
         "data/obs_2020_intersect.gpkg",
         delete_dsn = TRUE)

st_write(parcels_2020_intersect,
         "data/parcels_2020_intersect.gpkg",
         delete_dsn = TRUE)


#analysis function
analyze_parcels <- function(parcels, year) {
  
  q20 <- quantile(parcels$parcel_area_m2, 0.20, na.rm = TRUE)
  
  cat("\n---", year, "---\n")
  cat("Number of parcels:", nrow(parcels), "\n")
  cat("Mean area (m²):", round(mean(parcels$parcel_area_m2, na.rm = TRUE), 1), "\n")
  cat("Median area (m²):", round(median(parcels$parcel_area_m2, na.rm = TRUE), 1), "\n")
  cat("20th percentile (m²):", round(q20, 1), "\n")
  cat("Min area (m²):", min(parcels$parcel_area_m2, na.rm = TRUE), "\n")
  
  hist(
    parcels$parcel_area_m2,
    breaks = 50,
    main = paste("Parcel area distribution", year),
    xlab = "Area (m²)"
  )
  
  return(q20)
}

# run analysis
q20_2020 <- analyze_parcels(parcels_2020_intersect, 2020)
q20_2025 <- analyze_parcels(parcels_2025_intersect, 2025)


