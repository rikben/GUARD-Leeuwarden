# finalise_metadata.R

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

metadata_dir <- "../downloading/metadata"
out_dir <- "output"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ---- Helpers ----

normalise_discarded <- function(x) {
  case_when(
    is.na(x) ~ FALSE,
    as.character(x) %in% c("TRUE", "true", "True", "yes", "Yes", "1") ~ TRUE,
    TRUE ~ FALSE
  )
}

normalise_label <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  ifelse(is.na(x) | x == "", NA_character_, x)
}

extract_year <- function(path) {
  str_extract(basename(path), "\\d{4}")
}

# ---- Find metadata files ----

all_csv <- list.files(
  metadata_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

image_files <- all_csv |>
  keep(~ str_detect(basename(.x), regex("image", ignore_case = TRUE))) |>
  keep(~ !is.na(extract_year(.x)))

parcel_files <- all_csv |>
  keep(~ str_detect(basename(.x), regex("parcel", ignore_case = TRUE))) |>
  keep(~ !is.na(extract_year(.x)))

years <- sort(unique(c(
  map_chr(image_files, extract_year),
  map_chr(parcel_files, extract_year)
)))

# ---- Merge functions ----

merge_image_year <- function(year) {
  files <- image_files[map_chr(image_files, extract_year) == year]
  
  if (length(files) == 0) return(NULL)
  
  message("Merging image metadata for ", year)
  
  dat <- map_dfr(files, function(f) {
    read_csv(f, show_col_types = FALSE) |>
      mutate(
        source_file = basename(f),
        discarded = normalise_discarded(discarded),
        class_label = normalise_label(class_label)
      )
  })
  
  base_rows <- dat |>
    group_by(image_id) |>
    slice(1) |>
    ungroup() |>
    select(-source_file)
  
  merged_labels <- dat |>
    group_by(image_id) |>
    summarise(
      discarded = any(discarded, na.rm = TRUE),
      labels_found = paste(sort(unique(na.omit(class_label))), collapse = "; "),
      n_labels_found = n_distinct(na.omit(class_label)),
      class_label_conflict = n_labels_found > 1,
      class_label_merged = case_when(
        n_labels_found == 0 ~ NA_character_,
        n_labels_found == 1 ~ first(na.omit(class_label)),
        TRUE ~ NA_character_
      ),
      label_source_files = paste(
        unique(source_file[!is.na(class_label)]),
        collapse = "; "
      ),
      .groups = "drop"
    )
  
  final <- base_rows |>
    select(-discarded, -class_label) |>
    left_join(merged_labels, by = "image_id") |>
    rename(class_label = class_label_merged)
  
  conflicts <- final |>
    filter(class_label_conflict) |>
    select(
      image_id,
      parcel_id,
      image_date,
      labels_found,
      label_source_files,
      everything()
    )
  
  out_file <- file.path(out_dir, paste0("image_metadata", year, "_final.csv"))
  conflict_file <- file.path(out_dir, paste0("image_label_conflicts", year, ".csv"))
  
  write_csv(final, out_file)
  write_csv(conflicts, conflict_file)
  
  tibble(
    year = year,
    type = "image",
    files_merged = length(files),
    rows_out = nrow(final),
    discarded = sum(final$discarded, na.rm = TRUE),
    labelled = sum(!is.na(final$class_label)),
    conflicts = nrow(conflicts),
    output_file = out_file,
    conflict_file = conflict_file
  )
}

merge_parcel_year <- function(year) {
  files <- parcel_files[map_chr(parcel_files, extract_year) == year]
  
  if (length(files) == 0) return(NULL)
  
  message("Merging parcel metadata for ", year)
  
  dat <- map_dfr(files, function(f) {
    read_csv(f, show_col_types = FALSE) |>
      mutate(
        source_file = basename(f),
        discarded = normalise_discarded(discarded)
      )
  })
  
  base_rows <- dat |>
    group_by(parcel_id) |>
    slice(1) |>
    ungroup() |>
    select(-source_file, -discarded)
  
  merged_discarded <- dat |>
    group_by(parcel_id) |>
    summarise(
      discarded = any(discarded, na.rm = TRUE),
      discard_source_files = paste(
        unique(source_file[discarded]),
        collapse = "; "
      ),
      .groups = "drop"
    )
  
  final <- base_rows |>
    left_join(merged_discarded, by = "parcel_id")
  
  out_file <- file.path(out_dir, paste0("parcel_metadata", year, "_final.csv"))
  
  write_csv(final, out_file)
  
  tibble(
    year = year,
    type = "parcel",
    files_merged = length(files),
    rows_out = nrow(final),
    discarded = sum(final$discarded, na.rm = TRUE),
    labelled = NA_integer_,
    conflicts = NA_integer_,
    output_file = out_file,
    conflict_file = NA_character_
  )
}

# ---- Run ----

summary <- map_dfr(years, function(y) {
  bind_rows(
    merge_image_year(y),
    merge_parcel_year(y)
  )
})

summary_file <- file.path(out_dir, "metadata_merge_summary.csv")
write_csv(summary, summary_file)

message("Done.")
message("Summary written to: ", summary_file)
