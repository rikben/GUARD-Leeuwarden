required_packages <- c("dplyr", "ranger")

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
# RUN: Apply the trained ranger RF model to image_metadata
#   Expects `image_metadata` and `yr` to be defined in the calling
#   environment (produced by 01_data_pipeline.R / set in main.R).
#   Returns `final_predictions` (data.frame) or NULL if no valid rows
#   or the model file is missing.
# ─────────────────────────────────────────────

run_rf_prediction <- function(image_metadata, yr,
                              model_path = "rf_model_scenario3_both.rds") {
  cat("\n===== 2. Applying Ranger Random Forest Model =====\n")
  
  if (!file.exists(model_path)) {
    cat("Error: Model file", model_path, "not found. Ensure it is in the working directory.\n")
    return(NULL)
  }
  
  rf_model <- readRDS(model_path)
  
  # Filter out rows with zero valid pixels
  valid_metadata <- image_metadata %>%
    filter(is.na(discarded))
  
  if (nrow(valid_metadata) == 0) {
    cat("No valid data available for predictions (all rows discarded).\n")
    return(NULL)
  }
  
  # Predict using ranger syntax
  pred_obj    <- predict(rf_model, data = valid_metadata)
  prob_matrix <- pred_obj$predictions
  
  pred_classes <- colnames(prob_matrix)[apply(prob_matrix, 1, which.max)]
  max_probs    <- apply(prob_matrix, 1, max)
  
  # Dataframe to pass to temporal pattern analysis
  final_predictions <- data.frame(
    image_id              = valid_metadata$image_id,
    parcel_id             = valid_metadata$parcel_id,
    image_date            = as.Date(valid_metadata$image_date),
    predicted_class       = as.character(pred_classes),
    predicted_probability = as.numeric(round(max_probs, 4))
  ) %>%
    arrange(parcel_id, image_date)
  
  # Save raw predictions
  dir.create("metadata", showWarnings = FALSE, recursive = TRUE)
  out_preds_path <- file.path("metadata", paste0("predictions_", yr, ".csv"))
  write.csv(final_predictions, out_preds_path, row.names = FALSE)
  cat("Image-level predictions saved to:", out_preds_path, "\n")
  
  return(final_predictions)
}