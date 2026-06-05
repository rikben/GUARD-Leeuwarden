# create_country_grid.R

library(sf)
library(dplyr)

# ---- Settings ----
grid_size_m <- 50000

out_dir <- "data"
out_file <- file.path(out_dir, paste0("nl_grid_", grid_size_m, "m.geojson"))

boundary_url <- paste0(
  "https://service.pdok.nl/kadaster/bestuurlijkegebieden/wfs/v1_0?",
  "request=GetFeature&service=WFS&version=1.1.0&",
  "outputFormat=application%2Fjson&",
  "typeName=bestuurlijkegebieden:Landgebied"
)

# ---- Create output folder if needed ----
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ---- Read national boundary ----
nl_boundary <- st_read(boundary_url, quiet = FALSE) |>
  st_transform(28992) |>
  st_make_valid()

# Safety check
nl_boundary <- nl_boundary |>
  filter(naam == "Nederland")

# ---- Create grid over national boundary extent ----
grid <- st_make_grid(
  nl_boundary,
  cellsize = grid_size_m,
  square = TRUE
) |>
  st_as_sf() |>
  mutate(grid_id = row_number())

# ---- Clip grid to national boundary ----
grid_clipped <- st_intersection(grid, nl_boundary) |>
  st_make_valid() |>
  st_geometry() |>
  st_as_sf()

grid_clipped <- grid_clipped |>
  mutate(
    grid_id = row_number(),
    grid_size_m = grid_size_m,
    area_m2 = as.numeric(st_area(grid_clipped))
  )

# ---- Save as GeoJSON ----
if (file.exists(out_file)) {
  file.remove(out_file)
}

st_write(
  grid_clipped,
  out_file,
  driver = "GeoJSON",
  quiet = FALSE
)

message("Grid written to: ", out_file)
message("Number of grid cells: ", nrow(grid_clipped))