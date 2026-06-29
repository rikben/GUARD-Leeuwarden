# create_country_grid.R

required_packages <- c(
  "sf",
  "dplyr"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing missing package: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_packages, install_if_missing))
invisible(lapply(required_packages, function(pkg) {
  message("Loading package: ", pkg)
  library(pkg, character.only = TRUE)
}))

# ---- Settings ----
if (!exists("grid_size_m")) {
  grid_size_m <- 40000
}

if (!exists("min_area_fraction")) {
  min_area_fraction <- 0.25
}

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

nl_boundary <- nl_boundary |>
  filter(naam == "Nederland")

# ---- Create grid ----
grid <- st_make_grid(
  nl_boundary,
  cellsize = grid_size_m,
  square = TRUE
) |>
  st_as_sf() |>
  mutate(grid_id = row_number())

# ---- Clip grid to Netherlands ----
grid_clipped <- st_intersection(grid, nl_boundary) |>
  st_make_valid() |>
  st_as_sf()

# ---- Calculate areas ----
grid_clipped <- grid_clipped |>
  mutate(
    grid_id = row_number(),
    grid_size_m = grid_size_m,
    area_m2 = as.numeric(st_area(grid_clipped))
  )

# ---- Merge small edge cells ----

full_cell_area <- grid_size_m^2
min_area <- min_area_fraction * full_cell_area

message("Minimum allowed area: ", format(min_area, scientific = FALSE), " m²")

grid_sf <- grid_clipped

repeat {
  
  grid_sf$area_m2 <- as.numeric(st_area(grid_sf))
  
  small_cells <- which(grid_sf$area_m2 < min_area)
  
  if (length(small_cells) == 0) {
    break
  }
  
  # Each polygon initially keeps itself
  grid_sf$merge_to <- seq_len(nrow(grid_sf))
  
  for (i in small_cells) {
    
    cell_boundary <- st_boundary(grid_sf[i, ])
    
    neighbors <- st_intersects(
      grid_sf[i, ],
      grid_sf,
      sparse = TRUE
    )[[1]]
    
    neighbors <- neighbors[neighbors != i]
    
    # Keep only neighbors sharing a line segment
    neighbors <- neighbors[
      sapply(neighbors, function(j) {
        
        shared <- st_intersection(
          st_boundary(grid_sf[i, ]),
          st_boundary(grid_sf[j, ])
        )
        
        if (length(shared) == 0) {
          return(FALSE)
        }
        
        as.numeric(st_length(shared)) > 0
      })
    ]
    
    if (length(neighbors) == 0) {
      next
    }
    
    neighbor_areas <- grid_sf$area_m2[neighbors]
    
    # Merge into largest touching neighbor
    target <- neighbors[which.max(neighbor_areas)]
    
    grid_sf$merge_to[i] <- target
  }
  
  old_n <- nrow(grid_sf)
  
  grid_sf <- grid_sf |>
    group_by(merge_to) |>
    summarise(.groups = "drop") |>
    st_make_valid()
  
  new_n <- nrow(grid_sf)
  
  if (new_n == old_n) {
    warning(
      "No further merges possible, but small cells remain."
    )
    break
  }
  
  message(
    "Merged ",
    old_n - new_n,
    " cell(s). Remaining: ",
    new_n
  )
}

# ---- Final attributes ----
grid_clean <- grid_sf |>
  mutate(
    grid_id = row_number(),
    grid_size_m = grid_size_m
  )

grid_clean$area_m2 <- as.numeric(st_area(grid_clean))

# ---- Diagnostics ----
message("Final number of cells: ", nrow(grid_clean))
message(
  "Smallest cell area: ",
  round(min(grid_clean$area_m2)),
  " m²"
)

message(
  "Cells below threshold: ",
  sum(grid_clean$area_m2 < min_area)
)

# ---- Save GeoJSON ----
if (file.exists(out_file)) {
  file.remove(out_file)
}

st_write(
  grid_clean,
  out_file,
  driver = "GeoJSON",
  quiet = FALSE
)

message("Grid written to: ", out_file)