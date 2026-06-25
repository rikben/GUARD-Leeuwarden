# Main function of this script
# Read RF model -> take information from attributes date, class_label, uncertainty, group_id
# -> create a moving window that iterates over time and keeps the accepted class_label patterns
# -> removes duplicates (based on following rules: 'latest' 1 and 'earliest' 4, and the image of the classes 
# between 1-4 which have the highest certainty are kept)

library(readr)
library(dplyr)
library(here)

out_dir <- "data"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# LOAD
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
# CLEAN
# ------------------------------------------------------------
df <- df %>%
  filter(!is.na(predicted_class)) %>%
  arrange(parcel_id, image_date)

# ------------------------------------------------------------
# DUPLICATE REMOVAL (IMPORTANT: keep signal but compress noise)
# ------------------------------------------------------------
remove_consecutive_duplicates <- function(df) {
  
  parcels <- split(df, df$parcel_id)
  out <- list()
  
  for (pid in names(parcels)) {
    
    d <- parcels[[pid]]
    
    run_id <- cumsum(
      c(TRUE, d$predicted_class[-1] != d$predicted_class[-nrow(d)])
    )
    
    cleaned <- lapply(split(d, run_id), function(x) {
      
      cls <- x$predicted_class[1]
      
      if (cls == "green") {
        x[nrow(x), ]                # latest green
        
      } else if (cls == "ploughed") {
        x[1, ]                      # earliest ploughed
        
      } else {
        x[which.max(x$predicted_probability), ]  # strongest signal
      }
    })
    
    out[[pid]] <- bind_rows(cleaned)
  }
  
  bind_rows(out) %>% arrange(parcel_id, image_date)
}

df_clean <- remove_consecutive_duplicates(df)

# ------------------------------------------------------------
# ALLOWED PATTERNS
# ------------------------------------------------------------
allowed_patterns <- list(
  c("green","slightly_yellow","yellow","ploughed"),
  c("green","yellow","ploughed"),
  c("green","slightly_yellow","ploughed"),
  c("slightly_yellow","yellow","ploughed")
)

# ------------------------------------------------------------
# COMPRESS SEQUENCE
# ------------------------------------------------------------
compress_seq <- function(x) rle(x)$values

# ------------------------------------------------------------
# CHECK VALID WINDOW
# ------------------------------------------------------------
is_valid_window <- function(seq, allowed_patterns) {
  
  seq <- as.character(seq)
  
  for (p in allowed_patterns) {
    if (identical(seq, p)) return(TRUE)
  }
  
  return(FALSE)
}

# ------------------------------------------------------------
# FINAL PIPELINE
# ------------------------------------------------------------
process_parcels <- function(df, window_days = 30) {
  
  parcels <- split(df, df$parcel_id)
  results <- list()
  
  for (pid in names(parcels)) {
    
    d <- parcels[[pid]] %>% arrange(image_date)
    dates <- unique(d$image_date)
    
    glyphosate <- FALSE
    
    # -----------------------------
    # WINDOW SEARCH (classification only)
    # -----------------------------
    for (start_date in dates) {
      
      window <- d %>%
        filter(image_date >= start_date &
                 image_date <= start_date + window_days) %>%
        arrange(image_date)
      
      if (nrow(window) < 2) next
      
      seq <- compress_seq(window$predicted_class)
      
      if (is_valid_window(seq, allowed_patterns)) {
        glyphosate <- TRUE
        break
      }
    }
    
    # -----------------------------
    # FINAL OUTPUT (IMPORTANT FIX)
    # -----------------------------
    results[[pid]] <- data.frame(
      parcel_id = pid,
      glyphosate = ifelse(glyphosate, "yes", "no"),
      
      # ✔ THIS is what you asked for:
      avg_probability = mean(d$predicted_probability, na.rm = TRUE)
    )
  }
  
  bind_rows(results)
}

# ------------------------------------------------------------
# RUN + EXPORT
# ------------------------------------------------------------
final_results <- process_parcels(df_clean)

write_csv(final_results, here("downloading/data", "parcel_predictions.csv"))


# Later, create second prediction script which will test on new un-labelled data from Leeuwarden
# Takes .gpkg with all Leeuwarden parcels
# grabs new images, runs model, repeats pattern search (prediction)