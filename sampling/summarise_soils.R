# summarise_soils.R

library(sf)
library(dplyr)
library(stringr)

# ---- Settings ----

data_dir <- "data"
brp_years <- c(2020, 2025)

# ---- Process each year ----

for (year in brp_years) {
  
  message("\n---- Summarising soils for BRP year: ", year, " ----")
  
  gpkg_file <- file.path(data_dir, paste0("brp_dominant_soil_", year, ".gpkg"))
  out_file <- file.path(data_dir, paste0("brp_dominant_soil_", year, "_simplified.gpkg"))
  out_layer <- paste0("brp_dominant_soil_", year, "_simplified")
  
  if (!file.exists(gpkg_file)) {
    stop("Input file not found: ", gpkg_file)
  }
  
  soil_data <- st_read(gpkg_file, quiet = TRUE)
  
  soil_data_simplified <- soil_data |>
    mutate(
      soil_text = str_to_lower(paste(
        dominant_mainsoilclassification,
        dominant_soilclassification,
        dominant_topsoil_description,
        sep = " "
      )),
      
      simplified_soil_type = case_when(
        str_detect(soil_text, "veen|moerig|moerige") ~ "Peat",
        str_detect(soil_text, "löss|loess|siltige leem|zandige leem|keileem|leemgronden") ~ "Loess",
        str_detect(soil_text, "klei|zavel|kleidek|klei-dek|kleiig") ~ "Clay",
        str_detect(soil_text, "lemig.*zand|zwak lemig|sterk lemig|zeer sterk lemig") ~ "Loamy Sand",
        str_detect(soil_text, "zand|podzol|duinvaag|vlakvaag|beekeerd|gooreerd|enkeerd") ~ "Sand",
        TRUE ~ "Other"
      )
    ) |>
    select(-soil_text)
  
  # ---- Save updated GPKG ----
  
  if (file.exists(out_file)) {
    file.remove(out_file)
  }
  
  st_write(
    soil_data_simplified,
    out_file,
    layer = out_layer,
    driver = "GPKG",
    quiet = FALSE
  )
  
  cat("\nWritten updated file to:\n")
  cat(out_file, "\n")
  
  # ---- Parcel-count summary ----
  
  parcel_summary <- soil_data_simplified |>
    st_drop_geometry() |>
    count(
      simplified_soil_type,
      sort = TRUE,
      name = "n_parcels"
    ) |>
    mutate(
      parcel_percentage = round(100 * n_parcels / sum(n_parcels), 2)
    )
  
  cat("\nSimplified soil type summary by parcel count for ", year, ":\n", sep = "")
  print(parcel_summary)
  
  # ---- Area-weighted summary ----
  
  area_summary <- soil_data_simplified |>
    st_drop_geometry() |>
    group_by(simplified_soil_type) |>
    summarise(
      n_parcels = n(),
      area_ha = sum(parcel_area_m2, na.rm = TRUE) / 10000,
      .groups = "drop"
    ) |>
    mutate(
      area_percentage = round(100 * area_ha / sum(area_ha), 2),
      area_ha = round(area_ha, 1)
    ) |>
    arrange(desc(area_ha))
  
  cat("\nSimplified soil type summary by area for ", year, ":\n", sep = "")
  print(area_summary)
  
  # ---- Check examples per class ----
  
  examples <- soil_data_simplified |>
    st_drop_geometry() |>
    group_by(simplified_soil_type) |>
    summarise(
      example_mainsoil = first(dominant_mainsoilclassification),
      example_detail = first(dominant_soilclassification),
      example_topsoil = first(dominant_topsoil_description),
      .groups = "drop"
    )
  
  cat("\nExample classification per simplified soil type for ", year, ":\n", sep = "")
  print(examples)
}