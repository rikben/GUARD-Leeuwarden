# Main function of this script
# Read RF model -> take information from attributes date, class_label, uncertainty, group_id
# -> create a moving window that iterates over time and keeps the accepted class_label patterns
# -> removes duplicates (based on following rules: 'latest' 1 and 'earliest' 4, and the image of the classes 
# between 1-4 which have the highest certainty are kept)

# load packages
library(readr)
library(here)

# setup output directory
out_dir <- "data"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# 1. READ CSV (input step)
file_path <- here("downloading", "metadata", "image_metadata2020_final.csv")

df <- read_csv(file_path, show_col_types = FALSE)

#ADD GROUP_ID IN FINAL SCRIPT
# Checks whether required attributes are there
required_columns <- c("class_label", "image_date", "discarded")

missing_columns <- required_columns[!required_columns %in% names(df)]

if (length(missing_columns) == 0) {
  message("All required columns exist")
} else {
  message("Missing columns: ", paste(missing_columns, collapse = ", "))
}

###------------------commented out until mathijs' input is used----------------------------

#if ("uncertainty" %in% names(df)) {
#  message("uncertainty exists")
#} else {
#  message("uncertainty does not exist")
#}

###--------------------------------------------------------------------------------------





#REPLACE PARCEL_ID WITH GROUP_ID
#excluded_parcels <- c(
#  2, 3, 4, 5, 6, 9, 13, 15, 16, 18, 19, 21, 31, 46, 51, 52, 54, 57, 59, 66, 78, 79,
#  101, 103, 104, 105, 106, 110, 111, 114, 115, 116, 118, 119, 120, 121, 123, 124, 129, 130, 132, 135, 137, 138,
#  140, 141, 143, 145, 146, 147, 149, 150, 151, 152, 153, 154, 156, 157, 158, 159, 161, 162, 163, 164, 165, 166,
#  167, 169, 170, 172, 173, 174, 177, 178, 179, 180, 181, 187, 188, 189, 190, 191, 194, 196, 197, 198, 199, 206,
#  207, 210, 212, 215, 217, 218, 219, 220, 221, 222, 223, 226, 230, 231, 232, 234, 235, 236, 237, 238, 239, 243,
#  244, 245, 246, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 263, 264, 265, 266, 267,
# 268, 269, 271, 272, 273, 275, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 295,
#  296, 297, 298, 300, 303, 304, 305, 308, 309, 310, 311, 312, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323,
#  326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 348, 350,
# 353, 354, 356, 357, 360, 361, 362, 363, 364, 365, 367, 368, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379,
#  380, 381, 382, 383, 384, 385, 386, 387, 390, 391, 392, 393, 394, 395, 397, 398, 400
#)

#data cleaning - removes rows with class_label "no_data", rows marked as discarded = TRUE, and discarded parcels (only for the dummy data)
clean_and_sort_metadata <- function(df) {
  
  df <- df[
    df$class_label != "no_data" &
      df$discarded != TRUE &
      !(df$parcel_id %in% excluded_parcels),
  ]
  
  df <- df[order(df$parcel_id, df$image_date), ]
  
  return(df)
}


df <- clean_and_sort_metadata(df)

#remove duplicates
remove_consecutive_duplicates <- function(df) {
  
  # ensure chronological order
  df <- df[order(df$image_date), ]
  
  # keep only rows where class changes
  df <- df[
    c(TRUE, df$class_label[-1] != df$class_label[-nrow(df)]),
  ]
  
  return(df)
}

#Convert date column
df$image_date <- as.Date(df$image_date)

#Window extraction function (30-day moving window)
# For each parcel:
# - iterates over available observation dates
# - builds a 30-day forward window
# - checks whether the set of observed classes matches an allowed pattern
# - keeps only the first valid window per parcel
#
# Allowed patterns represent valid vegetation/crop progression states:
# - full cycle: green → slightly_yellow → yellow → ploughed
# - partial variants of this cycle
moving_window_30days_filtered <- function(df, window_days = 30) {
  
  results <- list()
  
  allowed_patterns <- list(
    c("green","slightly_yellow","yellow","ploughed"),
    c("green","yellow","ploughed"),
    c("green","slightly_yellow","ploughed"),
    c("slightly_yellow","yellow","ploughed")
  )
  
  parcels <- split(df, df$parcel_id)
  
  for (pid in names(parcels)) {
    
    parcel_data <- parcels[[pid]]
    parcel_data <- parcel_data[order(parcel_data$image_date), ]
    
    dates <- unique(parcel_data$image_date)
    
    best_window <- NULL
    found <- FALSE
    
    for (start_date in dates) {
      
      if (found) break
      
      end_date <- start_date + window_days
      
      window_data <- parcel_data[
        parcel_data$image_date >= start_date &
          parcel_data$image_date <= end_date,
      ]
      
      if (nrow(window_data) > 0) {
        
        labels <- sort(unique(window_data$class_label))
        
        for (pattern in allowed_patterns) {
          if (identical(labels, sort(pattern))) {
            best_window <- window_data
            found <- TRUE
            break
          }
        }
      }
    }
    
    if (!is.null(best_window)) {
      cleaned_window <- best_window[, c("image_date", "parcel_id", "class_label")]
      
      cleaned_window <- remove_consecutive_duplicates(cleaned_window)
      
      results[[length(results) + 1]] <- cleaned_window
    }
  }
  
  return(results)
}

results <- moving_window_30days_filtered(df)



# Once it has time window, it removes any duplicate numbers (for for 1, take the latest possible, for 4,take the earliest possible)
# Removes any NA's in the same go as duplicates


### scenarios for testing: ###
# Train & test on 2020
# Train & test on 2025
# Train & test on 2020 + 2025
# Train on 2020 and test on 2025

# Later, create second prediction script which will test on new un-labelled data from Leeuwarden
# Takes .gpkg with all Leeuwarden parcels
# grabs new images, runs model, repeats pattern search (prediction)