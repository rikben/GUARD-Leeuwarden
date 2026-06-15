# sample_brp_parcels.R

library(sf)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# ---- Settings ----

data_dir <- "data"
out_dir  <- "samples"

years <- c(2020, 2025)
grid_size <- 40000
target_categories <- c("Bouwland", "Grasland")

# Number of samples per glyphosate class per year
n_per_year_glyphosate <- c(
  "0" = 100,  # non-glyphosate parcels per year
  "1" = 300   # glyphosate parcels per year
)

out_summary_csv <- file.path(out_dir, "sample_summary.csv")
out_concise_summary_csv <- file.path(out_dir, "sample_summary_concise.csv")

set.seed(123)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ---- Check parcel files ----

parcel_files <- file.path(data_dir, paste0("brp_parcels_", years, ".gpkg"))

missing_files <- parcel_files[!file.exists(parcel_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing parcel file(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

message("Found parcel files:")
print(parcel_files)

# ---- Select grid file ----
grid_file <- file.path(
  data_dir,
  paste0("nl_grid_", grid_size, "m.geojson")
)

if (!file.exists(grid_file)) {
  stop("Grid file not found: ", grid_file)
}

message("Using grid file: ", grid_file)

# ---- Read grid ----

grid <- st_read(grid_file, quiet = TRUE)

if (!"grid_id" %in% names(grid)) {
  grid <- grid %>%
    mutate(grid_id = row_number())
}

grid <- grid %>%
  select(grid_id, geometry)

# ---- Read and combine parcels ----

read_parcels <- function(file) {
  
  parcels <- st_read(file, quiet = TRUE)
  
  required_cols <- c(
    "jaar",
    "simplified_soil_type",
    "category",
    "glyphosate"
  )
  
  missing_cols <- setdiff(required_cols, names(parcels))
  
  if (length(missing_cols) > 0) {
    stop(
      "File ", file, " is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  parcels
}

parcels <- map_dfr(parcel_files, read_parcels)

message("Unique categories before filtering:")
print(sort(unique(parcels$category)))

parcels <- parcels %>%
  filter(category %in% target_categories)

message("Keeping only categories:")
print(target_categories)

message("Unique simplified soil types:")
print(sort(unique(parcels$simplified_soil_type)))

message("Unique categories:")
print(sort(unique(parcels$category)))

message("Glyphosate classes:")
print(sort(unique(parcels$glyphosate)))

# ---- Match CRS ----

if (st_crs(parcels) != st_crs(grid)) {
  grid <- st_transform(grid, st_crs(parcels))
}

# ---- Assign parcels to grid cells ----
# Uses one representative point per parcel to assign each parcel to one grid cell.

parcel_points <- st_point_on_surface(parcels)

grid_match <- st_join(
  parcel_points,
  grid,
  join = st_within,
  left = TRUE
) %>%
  st_drop_geometry() %>%
  select(grid_id)

parcels$grid_id <- grid_match$grid_id

parcels <- parcels %>%
  filter(!is.na(grid_id))

# ---- Prepare strata ----

parcels <- parcels %>%
  mutate(
    simplified_soil_type = as.character(simplified_soil_type),
    category = as.character(category),
    glyphosate = case_when(
      glyphosate %in% c(1, "1", TRUE, "TRUE", "true") ~ "1",
      glyphosate %in% c(0, "0", FALSE, "FALSE", "false") ~ "0",
      TRUE ~ as.character(glyphosate)
    ),
    grid_id = as.character(grid_id)
  )

available_summary <- parcels %>%
  st_drop_geometry() %>%
  count(
    jaar,
    simplified_soil_type,
    category,
    grid_id,
    glyphosate,
    name = "n_available"
  )

# ---- Stratified sampling with yearly glyphosate targets ----

sample_one_year_glyph <- function(df, target_n) {
  
  if (nrow(df) <= target_n) {
    return(df)
  }
  
  stratum_counts <- df %>%
    st_drop_geometry() %>%
    count(
      simplified_soil_type,
      category,
      grid_id,
      name = "n_available"
    ) %>%
    mutate(
      prop = n_available / sum(n_available),
      n_target_raw = prop * target_n,
      n_target = floor(n_target_raw),
      remainder = n_target_raw - n_target
    )
  
  remaining <- target_n - sum(stratum_counts$n_target)
  
  if (remaining > 0) {
    stratum_counts <- stratum_counts %>%
      arrange(desc(remainder)) %>%
      mutate(
        n_target = n_target + ifelse(row_number() <= remaining, 1, 0)
      )
  }
  
  df_with_targets <- df %>%
    left_join(
      stratum_counts %>%
        select(simplified_soil_type, category, grid_id, n_target),
      by = c("simplified_soil_type", "category", "grid_id")
    )
  
  df_with_targets %>%
    group_by(simplified_soil_type, category, grid_id) %>%
    group_modify(function(.x, .y) {
      
      n_draw <- unique(.x$n_target)
      
      if (length(n_draw) != 1 || is.na(n_draw) || n_draw == 0) {
        return(.x[0, ])
      }
      
      slice_sample(.x, n = n_draw)
    }) %>%
    ungroup() %>%
    select(-n_target)
}

sampled <- parcels %>%
  group_by(jaar, glyphosate) %>%
  group_modify(function(.x, .y) {
    
    glyph_class <- as.character(.y$glyphosate)
    target_n <- n_per_year_glyphosate[[glyph_class]]
    
    if (is.null(target_n) || is.na(target_n)) {
      return(.x[0, ])
    }
    
    sample_one_year_glyph(.x, target_n)
  }) %>%
  ungroup()

# ---- Summary of selected samples ----

sample_summary <- sampled %>%
  st_drop_geometry() %>%
  count(
    jaar,
    simplified_soil_type,
    category,
    grid_id,
    glyphosate,
    name = "n_sampled"
  ) %>%
  full_join(
    available_summary,
    by = c(
      "jaar",
      "simplified_soil_type",
      "category",
      "grid_id",
      "glyphosate"
    )
  ) %>%
  mutate(
    n_sampled = ifelse(is.na(n_sampled), 0, n_sampled),
    n_available = ifelse(is.na(n_available), 0, n_available)
  ) %>%
  arrange(
    jaar,
    simplified_soil_type,
    category,
    grid_id,
    glyphosate
  )

# ---- Concise summary ----

sample_summary_concise <- bind_rows(
  
  sample_summary %>%
    group_by(jaar, glyphosate) %>%
    summarise(
      n_available = sum(n_available),
      n_sampled = sum(n_sampled),
      .groups = "drop"
    ) %>%
    mutate(summary_level = "year_glyphosate"),
  
  sample_summary %>%
    group_by(jaar, glyphosate, category) %>%
    summarise(
      n_available = sum(n_available),
      n_sampled = sum(n_sampled),
      .groups = "drop"
    ) %>%
    mutate(summary_level = "year_glyphosate_category"),
  
  sample_summary %>%
    group_by(jaar, glyphosate, simplified_soil_type) %>%
    summarise(
      n_available = sum(n_available),
      n_sampled = sum(n_sampled),
      .groups = "drop"
    ) %>%
    mutate(summary_level = "year_glyphosate_soil"),
  
  sample_summary %>%
    group_by(jaar, glyphosate) %>%
    summarise(
      n_grid_cells_available = n_distinct(grid_id[n_available > 0]),
      n_grid_cells_sampled = n_distinct(grid_id[n_sampled > 0]),
      .groups = "drop"
    ) %>%
    mutate(
      summary_level = "year_glyphosate_grid_coverage",
      n_available = n_grid_cells_available,
      n_sampled = n_grid_cells_sampled
    ) %>%
    select(-n_grid_cells_available, -n_grid_cells_sampled)
) %>%
  arrange(summary_level, jaar, glyphosate)

# ---- Export ----

sampled_years <- sort(unique(sampled$jaar))

for (yr in sampled_years) {
  
  out_file <- file.path(
    out_dir,
    paste0("sampled_parcels_", yr, ".gpkg")
  )
  
  st_write(
    sampled %>% filter(jaar == yr),
    out_file,
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message("Written: ", out_file)
}

write_csv(sample_summary, out_summary_csv)
write_csv(sample_summary_concise, out_concise_summary_csv)

message("Done.")
message("Exported sampled parcels per year to folder: ", out_dir)
message("Exported detailed sample summary to: ", out_summary_csv)
message("Exported concise sample summary to: ", out_concise_summary_csv)