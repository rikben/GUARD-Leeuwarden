### Script that will link to Waarnemingen.nl observations using an API and
# it will retrieve all observations from defined time period (spring of 2020, 2025).
# It will then extract geo information from them to make true poitns, and
# those will then be linked to specific parcels. Because the observers usually log
# fields from the road and not the field itself,it is challenging to match the correct
# parcel to the observations. Two methods can be applied:
# 1) A buffer of ~20m around the point, intersected parcel with largest shared area get chosen
# 2) closest parcel from point by (euclidean) distance gets selected

# installing packages
#install.packages("httr2")

# libraries
#library(sf)
#library(httr2)
#library(jsonlite)


# Call the Waarnemningen API to fetch all observations from 2020
url_2020 <- paste0(
  "https://waarneming.nl/api/v1/observations/",
  "?species_group=30",
  "&search=glyphosate+sprayed",
  "&date_after=2020-03-10",
  "&date_before=2020-06-15",
  "&limit=50000"
)
# and for the 2025 observations as well...
url_2025 <- paste0(
  "https://waarneming.nl/api/v1/observations/",
  "?species_group=30",
  "&search=glyphosate+sprayed",
  "&date_after=2025-03-10",
  "&date_before=2025-06-15",
  "&limit=50000"
)

# create objects from observations for both years
obs_2020_raw <- fromJSON(url_2020, simplifyVector = FALSE) # F so that it keeps spatial info
obs_2025_raw <- fromJSON(url_2025, simplifyVector = FALSE)

### convert to sf objects to extract the real geometry from attributes ###
ids <- vapply(obs_2020_raw$results, function(x) x$id, integer(1))

accuracy <- vapply(obs_2020_raw$results, function(x) {
  if (is.null(x$accuracy)) NA_real_ else as.numeric(x$accuracy)
}, numeric(1))

# only now extract coordinates
coords_2020 <- t(vapply(obs_2020_raw$results, function(x) {
  
  if (is.null(x$point) || is.null(x$point$coordinates)) {
    return(c(NA_real_, NA_real_))
  }
  
  c(
    x$point$coordinates[[1]],
    x$point$coordinates[[2]]
  )
  
}, numeric(2)))

# construct a dataframe cleanly..
obs_df_2020 <- data.frame(
  id = ids,
  accuracy = accuracy,
  lon = coords_2020[,1],
  lat = coords_2020[,2]
)
# finally convert to sf object
obs_2020_sf <- st_as_sf(
  obs_df_2020,
  coords = c("lon", "lat"),
  crs = 4326
)

# REPEAT for 2025 #
# coordinates extraction (this time i am using vapply instead of sapply because
# there are some NULL accuracy fields and the results would no longer match coords_2025)
ids <- vapply(obs_2025_raw$results, function(x) x$id, integer(1))

accuracy <- vapply(obs_2025_raw$results, function(x) {
  if (is.null(x$accuracy)) NA_real_ else as.numeric(x$accuracy)
}, numeric(1))

# only now extract coordinates
coords_2025 <- t(vapply(obs_2025_raw$results, function(x) {
  
  if (is.null(x$point) || is.null(x$point$coordinates)) {
    return(c(NA_real_, NA_real_))
  }
  
  c(
    x$point$coordinates[[1]],
    x$point$coordinates[[2]]
  )
  
}, numeric(2)))

# dataframe build
obs_df_2025 <- data.frame(
  id = ids,
  accuracy = accuracy,
  lon = coords_2025[,1],
  lat = coords_2025[,2]
)
# sf conversion
obs_2025_sf <- st_as_sf(
  obs_df_2025,
  coords = c("lon", "lat"),
  crs = 4326
)

### ACCURACY - filtering or not filtering observations ###
### WE DECIDED NOT TO DO THIS - THE FIELD OBSERVER_LOCATION IS NA ###
# remove sightings which were NOT moved (marker not adjusted by user) and their accuracy is worse than 25 m
# fields to use: compare "observer_location" and "location" to see if they moved marker
# If they differ, do not do anything (assuming they moved the marker to field correctly)
# If they are the same, proceed to field "accuracy"
# If accuracy was <= 25 m, keep this observation (if it was 26m or more, discard it)

## reproject both to RD new for later AND plot them to check if success
obs_2020_sf <- st_transform(obs_2020_sf, 28992)
obs_2025_sf <-  st_transform(obs_2025_sf, 28992)
plot(st_geometry(obs_2020_sf))
plot(st_geometry(obs_2025_sf))

# create dir
out_dir <- "data"
year <- c("2020", "2025")

# ---- Create output folder if needed ----
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

#optionally, export the final shapefiles (they will have id, accuracy, and geometry)
write_to_gpkg <- function(shapefile, year, out_dir) {
  st_write(
    shapefile,
    file.path(out_dir, paste0("obs_", year, ".gpkg")),
    delete_dsn = TRUE
  )
}

write_to_gpkg(obs_2020_sf, year[1], out_dir)
write_to_gpkg(obs_2025_sf, year[2], out_dir)

# NOTES FOR SELF:
# make the logic better through a function which does all of this based on a year you give it
# meaning you want RD_new sf object with observations for 2020? just give the function
# that year and it should do the rest:)


### CLEANUP ### potentially..
#rm(obs_2020_raw, obs_2025_raw, obs_df_2020, obs_df_2025)
