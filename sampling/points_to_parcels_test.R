library(sf)
library(dplyr)

#for now hardcoded, but its going to be called from the other R scripts later

#reading data for parcels, points, and the spatial grid
brp_parcels_2025 <- st_read("C:/Users/dirke/projects/brp_dominant_soil_2025_simplified.gpkg")
obs_2025 <- st_read("C:/Users/dirke/projects/data/obs_2025.gpkg")
national_grid <- st_read("C:/Users/dirke/OneDrive/Documenten/data/nl_grid_40000m.geojson")

#keep only the points which intersect with parcels
obs_2025_intersect <- st_filter(obs_2025, brp_parcels_2025, .predicate = st_intersects)

#keep only the parcels which intersect with points
parcels_2025_intersect <- st_filter(brp_parcels_2025, obs_2025, .predicate = st_intersects)

#writing the intersect files to new gpkgs
st_write(obs_2025_intersect, "obs_2025_intersect.gpkg", delete_dsn = TRUE)
st_write(parcels_2025_intersect, "parcels_2025_intersect.gpkg", delete_dsn = TRUE)

#Notes for continuing tomorrow -- 

#download brp from 2020
#Repeat the same steps for 2020
#remove parcels that are too small for sentinel to even pick them up 
#(less than 20m in width and length, probably doable by checking if R can fit a circle of 80m diameter 
#into the parcel and if yes, it keeps it)