source("downloading/results_download.R")
source("Random_Forest/random_forest_predict.R")
source("detection_algorithm/pattern_recognition.R")

yr <- 2026
input_vector_file <- file.path("Random_Forest/leeuwarden_brp_parcels.gpkg")

# 1. Download imagery / regularize / extract metadata
image_metadata <- run_data_pipeline(yr, input_vector_file)

# 2. Apply trained ranger RF model 
final_predictions <- run_rf_prediction(image_metadata, yr,
                                       model_path = "Random_Forest/rf_model.rds")

if (!is.null(final_predictions)) {
  
  # 3. Temporal pattern search
  final_parcel_results <- run_temporal_pattern_search(final_predictions, yr, window_days = 25)
  
  # 4. Join results to geopackage
  parcels_with_results <- run_join_to_geopackage(final_parcel_results, input_vector_file, yr)
  
} else {
  cat("Pipeline stopped: no predictions available, skipping temporal pattern search and geopackage join.\n")
}

cat("\n===== Pipeline complete! =====\n")
