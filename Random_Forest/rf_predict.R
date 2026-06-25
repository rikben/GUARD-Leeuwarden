# Main function of this script
# Read RF model -> take information from attributes date, class_label, uncertainty, group_id
# -> create a moving window that iterates over time and keeps the accepted class_label patterns
# -> removes duplicates (based on following rules: 'latest' 1 and 'earliest' 4, and the image of the classes 
# between 1-4 which have the highest certainty are kept)

# ------------------------------------------------------------
# LOAD PACKAGES
# ------------------------------------------------------------
library(readr)
library(dplyr)
library(here)

# ------------------------------------------------------------
# OUTPUT DIR
# ------------------------------------------------------------
out_dir <- "data"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# 1. READ DATA
# ------------------------------------------------------------
file_path <- here("downloading/results", "predictions_2026.csv")

df <- read_csv(file_path, show_col_types = FALSE) %>%
  select(parcel_id, image_date, predicted_class, predicted_probability) %>%
  mutate(
    image_date = as.Date(image_date),
    predicted_class = as.character(predicted_class),
    predicted_probability = as.numeric(predicted_probability)
  ) %>%
  arrange(parcel_id, image_date)

# ------------------------------------------------------------
# CLEANING
# ------------------------------------------------------------
clean_and_sort_metadata <- function(df) {
  df %>%
    filter(!is.na(predicted_class)) %>%
    arrange(parcel_id, image_date)
}

df <- clean_and_sort_metadata(df)

# ------------------------------------------------------------
# DUPLICATE REMOVAL (your rule-based logic preserved)
# ------------------------------------------------------------
remove_consecutive_duplicates <- function(df) {
  
  df <- df %>% arrange(parcel_id, image_date)
  parcels <- split(df, df$parcel_id)
  
  cleaned_list <- list()
  
  for (pid in names(parcels)) {
    
    d <- parcels[[pid]]
    
    run_id <- cumsum(
      c(TRUE, d$predicted_class[-1] != d$predicted_class[-nrow(d)])
    )
    
    cleaned_runs <- lapply(split(d, run_id), function(x) {
      
      cls <- x$predicted_class[1]
      
      if (cls == "green") {
        x[nrow(x), ]          # latest green
        
      } else if (cls == "ploughed") {
        x[1, ]                # earliest ploughed
        
      } else {
        x[which.max(x$predicted_probability), ]
      }
    })
    
    cleaned_list[[pid]] <- bind_rows(cleaned_runs)
  }
  
  bind_rows(cleaned_list) %>%
    arrange(parcel_id, image_date)
}

df <- remove_consecutive_duplicates(df)

# ------------------------------------------------------------
# ALLOWED FULL TRAJECTORIES
# ------------------------------------------------------------
allowed_patterns <- list(
  c("green","slightly_yellow","yellow","ploughed"),
  c("green","yellow","ploughed"),
  c("green","slightly_yellow","ploughed"),
  c("slightly_yellow","yellow","ploughed")
)

# ------------------------------------------------------------
# COMPRESS SEQUENCE (CRITICAL STEP)
# ------------------------------------------------------------
compress_sequence <- function(x) {
  r <- rle(x)
  r$values
}

# ------------------------------------------------------------
# VALIDATE FULL SEQUENCE (STRICT)
# ------------------------------------------------------------
is_valid_sequence <- function(seq, allowed_patterns) {
  
  seq <- as.character(seq)
  
  for (p in allowed_patterns) {
    if (identical(seq, p)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# ------------------------------------------------------------
# MOVING WINDOW FILTER
# ------------------------------------------------------------
moving_window_30days_filtered <- function(df, window_days = 30) {
  
  results <- list()
  parcels <- split(df, df$parcel_id)
  
  for (pid in names(parcels)) {
    
    parcel_data <- parcels[[pid]] %>%
      arrange(image_date)
    
    dates <- unique(parcel_data$image_date)
    
    best_window <- NULL
    found <- FALSE
    
    for (start_date in dates) {
      
      if (found) break
      
      end_date <- start_date + window_days
      
      window_data <- parcel_data %>%
        filter(image_date >= start_date & image_date <= end_date) %>%
        arrange(image_date)
      
      if (nrow(window_data) < 2) next
      
      # --------------------------------------------------------
      # KEY FIX: compress first
      # --------------------------------------------------------
      seq <- compress_sequence(window_data$predicted_class)
      
      # validate full trajectory
      if (is_valid_sequence(seq, allowed_patterns)) {
        
        best_window <- window_data
        found <- TRUE
        break
      }
    }
    
    if (!is.null(best_window)) {
      
      cleaned_window <- best_window %>%
        select(image_date, parcel_id, predicted_class, predicted_probability)
      
      cleaned_window <- remove_consecutive_duplicates(cleaned_window)
      
      results[[pid]] <- cleaned_window
    }
  }
  
  return(results)
}

# ------------------------------------------------------------
# RUN
# ------------------------------------------------------------
results <- moving_window_30days_filtered(df)

# Later, create second prediction script which will test on new un-labelled data from Leeuwarden
# Takes .gpkg with all Leeuwarden parcels
# grabs new images, runs model, repeats pattern search (prediction)