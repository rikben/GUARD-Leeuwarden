### Script that will link to Waarnemingen.nl observations using an API and
# it will retrieve all observations from defined time period (spring of 2020, 2025).
# Extracts geo information to create sf point objects.

# PACKAGES #
#install.packages("httr2")

required_packages <- c(
  "sf",
  "httr2",
  "jsonlite"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing missing package: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# SETUP #
out_dir <- "data"
if (!exists("years")) {
  years <- c(2020, 2025)
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

## Core function that will fetch per variable from Waarnemingen API ##
fetch_observations <- function(year,
                               date_start = "03-01",
                               date_end   = "05-15",
                               limit      = 50000,
                               out_dir    = NULL) {
  year <- as.character(year)
  
  # 1: URL build
  url <- paste0(
    "https://waarneming.nl/api/v1/observations/",
    "?species_group=30",
    "&search=glyphosate+sprayed",
    "&date_after=",  year, "-", date_start,
    "&date_before=", year, "-", date_end,
    "&limit=", limit
  )
  
  message("Fetching observations for ", year, " ...")
  raw     <- fromJSON(url, simplifyVector = FALSE)
  results <- raw$results
  
  # 2: Extract attribute fields using vapply (for type safety)
  ids <- vapply(results, function(x) x$id, integer(1))
  accuracy <- vapply(results, function(x) {
    if (is.null(x$accuracy)) NA_real_ else as.numeric(x$accuracy)
  }, numeric(1))
  coords <- t(vapply(results, function(x) {
    if (is.null(x$point) || is.null(x$point$coordinates))
      return(c(NA_real_, NA_real_))
    c(x$point$coordinates[[1]], x$point$coordinates[[2]])
  }, numeric(2)))
  
  # 3: Build sf object and transform to RD New
  obs_sf <- st_as_sf(
    data.frame(id = ids, accuracy = accuracy,
               lon = coords[, 1], lat = coords[, 2]),
    coords = c("lon", "lat"),
    crs    = 4326
  ) |>
    st_transform(28992)
  
  # 4: Write to GPKG
  if (!is.null(out_dir)) {
    path <- file.path(out_dir, paste0("obs_", year, ".gpkg"))
    st_write(obs_sf, path, delete_dsn = TRUE)
  }
  
  obs_sf
}

## Run for all years from "years" list
obs_list <- lapply(
  setNames(years, paste0("obs_", years, "_sf")),
  fetch_observations,
  out_dir = out_dir
)

# Unpack to named objects in global environment (obs_2020_sf, obs_2025_sf)
list2env(obs_list, envir = .GlobalEnv)
