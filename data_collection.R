# 1 grid

grid_size_m <- 30000
min_area_fraction <- 0.25

source("sampling/create_country_grid.R")

# 2 prepare soil brp

# Choose the years for this run
brp_years <- c(2020, 2025)

source("sampling/prepare_soil_brp.R")

# 3 summarise soils

# Choose the years for this run
brp_years <- c(2020, 2025)

source("sampling/summarise_soils.R")

# 4 waarneming observation

# Choose the years for this run
years <- c(2020, 2025)

source("sampling/waarneming_obs.R")

# 5 points to parcels 

# Choose the years for this run
years <- c(2020, 2025)

source("sampling/points_to_parcels.R")

# 6 Sample BRP parcels 

years <- c(2020, 2025)
grid_size <- 40000
target_categories <- c("Grasland", "Bouwland")
n_per_year_glyphosate <- c("0" = 100, "1" = 300)

source("sampling/sample_brp_parcels.R")

# 7 sentinel download 

years <- c(2020, 2025)

source("downloading/sentinel_download_and_statistics.R")

