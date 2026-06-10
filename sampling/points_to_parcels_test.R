library(sf)

#for now hardcoded, but its going to be called from the other R scripts later
brp_parcels_2025 <- st_read("C:/Users/dirke/projects/brp_dominant_soil_2025_simplified.gpkg")

obs_2025 <- st_read("C:/Users/dirke/projects/obs_2025.gpkg")

national_grid <- st_read("C:/Users/dirke/projects/GUARD-Leeuwarden/sampling/create_country_grid")