required_packages <- c("dplyr", "readr", "sf")

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

# ─────────────────────────────────────────────
# TEMPORAL PATTERN FUNCTIONS
# ─────────────────────────────────────────────

remove_consecutive_duplicates <- function(df) {
  parcels <- split(df, df$parcel_id)
  out <- list()
  
  for (pid in names(parcels)) {
    d <- parcels[[pid]]
    
    run_id <- cumsum(
      c(TRUE,
        d$predicted_class[-1] != d$predicted_class[-nrow(d)])
    )
    
    cleaned <- lapply(split(d, run_id), function(x) {
      x[which.max(x$predicted_probability), ]
    })
    
    out[[pid]] <- bind_rows(cleaned)
  }
  
  bind_rows(out) %>%
    arrange(parcel_id, image_date)
}

compress_seq <- function(x) rle(x)$values

is_valid_window <- function(seq, allowed_patterns) {
  seq <- as.character(seq)
  for (p in allowed_patterns) {
    if (identical(seq, p)) return(TRUE)
  }
  return(FALSE)
}

process_parcels <- function(df, window_days = 25) {
  
  allowed_patterns <- list(
    c("green","slightly_yellow","yellow","ploughed"),
    c("green","yellow","ploughed"),
    c("green","slightly_yellow","ploughed"),
    c("slightly_yellow","yellow","ploughed")
  )
  
  parcels <- split(df, df$parcel_id)
  results <- list()
  
  for (pid in names(parcels)) {
    
    d <- parcels[[pid]] %>%
      arrange(image_date)
    
    d <- remove_consecutive_duplicates(d)
    
    n <- nrow(d)
    
    glyphosate <- FALSE
    valid_window_probs <- NULL
    
    for (i in seq_len(n)) {
      
      start_date <- d$image_date[i]
      
      window <- d %>%
        filter(image_date >= start_date &
                 image_date <= start_date + window_days) %>%
        arrange(image_date)
      
      if (nrow(window) < 2) next
      
      seq <- compress_seq(window$predicted_class)
      
      if (is_valid_window(seq, allowed_patterns)) {
        glyphosate <- TRUE
        valid_window_probs <- window$predicted_probability
        break
      }
    }
    
    avg_probability <- if (!is.null(valid_window_probs)) {
      mean(valid_window_probs, na.rm = TRUE)
    } else {
      mean(d$predicted_probability, na.rm = TRUE)
    }
    
    avg_probability <- round(avg_probability, 3)
    
    results[[pid]] <- data.frame(
      parcel_id = pid,
      glyphosate = ifelse(glyphosate, "yes", "no"),
      avg_probability = avg_probability
    )
  }
  
  bind_rows(results)
}

# ─────────────────────────────────────────────
# RUN PART 3: Execute temporal pattern search on final_predictions
#   Expects `final_predictions` (from 02_rf_prediction.R) and `yr`
#   to be defined in the calling environment.
#   Returns `final_parcel_results` (data.frame).
# ─────────────────────────────────────────────

run_temporal_pattern_search <- function(final_predictions, yr, window_days = 25) {
  cat("\n===== 3. Executing Temporal Pattern Search =====\n")
  
  df <- final_predictions %>%
    select(parcel_id, image_date, predicted_class, predicted_probability) %>%
    mutate(
      image_date = as.Date(image_date),
      predicted_class = as.character(predicted_class),
      predicted_probability = as.numeric(predicted_probability)
    ) %>%
    filter(!is.na(predicted_class)) %>%
    arrange(parcel_id, image_date)
  
  # Remove duplicates (keep highest-confidence image per run)
  df_clean <- remove_consecutive_duplicates(df)
  
  # Process parcels (window search)
  final_parcel_results <- process_parcels(df_clean, window_days = window_days)
  
  # Export final aggregated results
  out_dir <- "output"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_parcel_path <- file.path(out_dir, paste0("parcel_predictions_", yr, ".csv"))
  write_csv(final_parcel_results, out_parcel_path)
  cat("Final parcel classifications saved to:", out_parcel_path, "\n")
  
  return(final_parcel_results)
}

# ─────────────────────────────────────────────
# RUN PART 4: Join temporal pattern search results to the original
#   parcel geometries and write out a new GeoPackage.
#   Expects `final_parcel_results`, `input_vector_file`, and `yr`
#   to be defined in the calling environment.
# ─────────────────────────────────────────────

run_join_to_geopackage <- function(final_parcel_results, input_vector_file, yr) {
  cat("\n===== 4. Joining Results to Spatial File =====\n")
  
  # Load the original geometry
  parcels_sf <- st_read(input_vector_file) %>%
    mutate(parcel_id = row_number())
  
  # Join the data
  parcels_with_results <- parcels_sf %>%
    mutate(parcel_id = as.integer(parcel_id)) %>%
    left_join(
      final_parcel_results %>% mutate(parcel_id = as.integer(parcel_id)),
      by = "parcel_id"
    )
  
  # Save the new Geopackage
  out_dir <- "output"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  output_gpkg <- file.path(out_dir, paste0("Leeuwarden_Results_", yr, ".gpkg"))
  st_write(parcels_with_results, output_gpkg, append = FALSE)
  
  cat("Successfully exported final spatial results to:", output_gpkg, "\n")
  
  return(parcels_with_results)
}