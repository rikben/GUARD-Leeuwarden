### Script that will link to Waarnemingen.nl observations using an API and
# it will retrieve all observations from defined time period (spring of 2020, 2025).
# Extracts geo information to create sf point objects.

# PACKAGES #
#install.packages("httr2")

library(sf)
library(httr2)
library(jsonlite)

# SETUP #
out_dir <- "data"
year <- c("2020", "2025")

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}


# Call the Waarnemningen API to fetch all observations from 2020
# Dates were reduced from 10.3. - 15.6. to 23.3. - 7.5
url_2020 <- paste0(
  "https://waarneming.nl/api/v1/observations/",
  "?species_group=30",
  "&search=glyphosate+sprayed",
  "&date_after=2020-03-23",
  "&date_before=2020-05-07",
  "&limit=50000"
)
# and for the 2025 observations as well...
url_2025 <- paste0(
  "https://waarneming.nl/api/v1/observations/",
  "?species_group=30",
  "&search=glyphosate+sprayed",
  "&date_after=2025-03-23",
  "&date_before=2025-05-07",
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

## reproject both to RD new for later AND plot them to check if success
obs_2020_sf <- st_transform(obs_2020_sf, 28992)
obs_2025_sf <-  st_transform(obs_2025_sf, 28992)


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
# make the logic of this script better through a function which does all of this 
# based on a year you give it meaning you want RD_new sf object with observations 
# for 2020? just give the function that year and it should do the rest:)
