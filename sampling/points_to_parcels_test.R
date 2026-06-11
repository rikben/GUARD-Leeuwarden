library(sf)
library(dplyr)

#for now hardcoded, but its going to be called from the other R scripts later

#reading data for parcels, points, and the spatial grid

#reading data for parcels, points, and the spatial grid 
brp_parcels_2025 <- st_read("C:/Users/dirke/projects/brp_dominant_soil_2025_simplified.gpkg") 
brp_parcels_2020 <- st_read("C:/Users/dirke/projects/brp_dominant_soil_2020_simplified.gpkg") 

obs_2025 <- st_read("C:/Users/dirke/projects/data/obs_2025.gpkg") 
obs_2020 <- st_read("C:/Users/dirke/projects/data/obs_2020.gpkg") 

#national_grid <- st_read("C:/Users/dirke/OneDrive/Documenten/data/nl_grid_40000m.geojson") 

#keep only the points which intersect with parcels 
obs_2025_intersect <- st_filter(obs_2025, brp_parcels_2025, .predicate = st_intersects) 
obs_2020_intersect <- st_filter(obs_2020, brp_parcels_2020, .predicate = st_intersects) 

#keep only the parcels which intersect with points 
parcels_2025_intersect <- st_filter(brp_parcels_2025, obs_2025, .predicate = st_intersects) 
parcels_2020_intersect <- st_filter(brp_parcels_2020, obs_2020, .predicate = st_intersects) 

#writing the intersect files to new gpkgs 
st_write(obs_2025_intersect, "obs_2025_intersect.gpkg", delete_dsn = TRUE) 
st_write(parcels_2025_intersect, "parcels_2025_intersect.gpkg", delete_dsn = TRUE) 

st_write(obs_2020_intersect, "obs_2020_intersect.gpkg", delete_dsn = TRUE) 
st_write(parcels_2020_intersect, "parcels_2020_intersect.gpkg", delete_dsn = TRUE)

names(parcels_2020_intersect)

analyze_parcels <- function(parcels, year) {
  
  Small_areas <- quantile(parcels$parcel_area_m2, 0.25, na.rm = TRUE)
  
  cat("\n---", year, "---\n")
  cat("Number of parcels:", nrow(parcels), "\n")
  cat("Mean area (m²):", round(mean(parcels$parcel_area_m2, na.rm = TRUE), 1), "\n")
  cat("Median area (m²):", round(median(parcels$parcel_area_m2, na.rm = TRUE), 1), "\n")
  cat("25th percentile (m²):", round(Small_areas, 1), "\n")
  cat(
    "Parcels below 25th percentile:",
    sum(parcels$parcel_area_m2 < Small_areas, na.rm = TRUE),
    "\n"
  )
  
  hist(
    parcels$parcel_area_m2,
    breaks = 50,
    main = paste("Parcel area distribution", year),
    xlab = "Area (m²)"
  )
  
  return(Small_areas)
}

q25_2020 <- analyze_parcels(parcels_2020_intersect, 2020)
q25_2025 <- analyze_parcels(parcels_2025_intersect, 2025)

