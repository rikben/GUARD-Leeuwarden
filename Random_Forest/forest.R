library(dplyr)
library(ranger)
library(caret)
library(corrplot)

# ─────────────────────────────────────────────
# PREDICTOR COLUMNS
#   Hardcoded so individual predictors can be commented out
#   to test reduced feature sets without touching any other code.
#   All functions reference this vector rather than using grep().
# ─────────────────────────────────────────────
PREDICTOR_COLS <- c(
  "ndvi_mean", 
  "ndre_mean",
  "gndvi_mean", 
  "nirv_mean",
  "B04_mean"
)

# ─────────────────────────────────────────────
# 1. DATA PREPARATION
#    Loads one or more year pairs, repairs European number formatting,
#    filters discarded rows/parcels, joins glyphosate label from parcel
#    metadata, and tags every row with a year-aware group_id.
# ─────────────────────────────────────────────
prepare_training_data <- function(image_csv_paths, parcel_csv_paths) {
  read_csv_auto <- function(path) {
    first_line <- readLines(path, n = 1)
    sep <- if (lengths(regmatches(first_line, gregexpr(";", first_line))) >
               lengths(regmatches(first_line, gregexpr(",", first_line)))) ";" else ","
    read.csv(path, sep = sep, stringsAsFactors = FALSE)
  }
  
  fix_european_numeric <- function(x) {
    x_chr <- as.character(x)
    fix_one <- function(v) {
      if (is.na(v) || v == "" || v == "NA") return(NA_real_)
      if (grepl(",", v, fixed = TRUE)) {
        v <- gsub(".", "", v, fixed = TRUE)
        v <- gsub(",", ".", v, fixed = TRUE)
        return(suppressWarnings(as.numeric(v)))
      }
      n_dots <- lengths(regmatches(v, gregexpr("\\.", v)))
      if (n_dots <= 1) return(suppressWarnings(as.numeric(v)))
      first_dot_pos <- regexpr("\\.", v)[1]
      integer_part  <- substr(v, 1, first_dot_pos - 1)
      decimal_part  <- gsub("\\.", "", substr(v, first_dot_pos + 1, nchar(v)))
      suppressWarnings(as.numeric(paste0(integer_part, ".", decimal_part)))
    }
    vapply(x_chr, fix_one, numeric(1), USE.NAMES = FALSE)
  }
  
  if (length(image_csv_paths) != length(parcel_csv_paths))
    stop("image_csv_paths and parcel_csv_paths must be the same length.")
  
  all_data <- list()
  
  for (i in seq_along(image_csv_paths)) {
    raw_data    <- read_csv_auto(image_csv_paths[i])
    parcel_data <- read_csv_auto(parcel_csv_paths[i])
    
    numeric_cols <- PREDICTOR_COLS
    raw_data[numeric_cols] <- lapply(raw_data[numeric_cols], fix_european_numeric)
    
    discarded_parcels <- parcel_data %>%
      filter(toupper(as.character(discarded)) == "TRUE") %>%
      pull(parcel_id)
    
    # Bring glyphosate label from parcel metadata into the image rows
    glyphosate_lookup <- parcel_data %>%
      select(parcel_id, glyphosate) %>%
      mutate(glyphosate = as.integer(glyphosate))
    
    year_data <- raw_data %>%
      mutate(.img_discarded = toupper(trimws(as.character(discarded))) %in% c("TRUE", "YES")) %>%
      filter(!parcel_id %in% discarded_parcels) %>%
      filter(!.img_discarded) %>%
      select(-.img_discarded) %>%
      filter(!is.na(class_label) & class_label != "" &
               class_label != "NA" & class_label != "no_data") %>%
      left_join(glyphosate_lookup, by = "parcel_id") %>%
      mutate(
        class_label = as.factor(class_label),
        source_file = image_csv_paths[i],
        group_id    = paste0(source_file, "_", parcel_id)
      )
    
    cat("Loaded", nrow(year_data), "labeled rows from", image_csv_paths[i], "\n")
    all_data[[i]] <- year_data
  }
  
  training_data <- dplyr::bind_rows(all_data) %>%
    mutate(class_label = as.factor(class_label))
  
  cat("\nTotal combined labeled rows:", nrow(training_data), "\n")
  cat("Class distribution:\n")
  print(table(training_data$class_label))
  cat("Glyphosate distribution (0 = no, 1 = yes):\n")
  print(table(training_data$glyphosate))
  
  return(training_data)
}

# ─────────────────────────────────────────────
# 2. BOOTSTRAP
#    Upsamples non-glyphosate (glyphosate == 0) rows to a 2:1 ratio
#    versus glyphosate (glyphosate == 1) rows within the training set.
#    The test set is never touched.
# ─────────────────────────────────────────────
bootstrap_glyphosate <- function(train_raw) {
  glyph_1 <- train_raw %>% filter(glyphosate == 1)
  glyph_0 <- train_raw %>% filter(glyphosate == 0)
  
  if (nrow(glyph_1) == 0 || nrow(glyph_0) == 0) {
    warning("One glyphosate class is absent in training fold — skipping bootstrap.")
    return(train_raw)
  }
  
  # Target: non-glyphosate rows = 2 × glyphosate rows
  n_glyph_0_target <- 2 * nrow(glyph_1)
  glyph_0_upsampled <- glyph_0[sample(nrow(glyph_0), n_glyph_0_target, replace = TRUE), ]
  
  bootstrapped <- bind_rows(glyph_1, glyph_0_upsampled)
  cat("  Bootstrap: glyphosate=1:", nrow(glyph_1),
      "| glyphosate=0 (upsampled to 2x):", nrow(glyph_0_upsampled),
      "| total:", nrow(bootstrapped), "\n")
  return(bootstrapped)
}

# ─────────────────────────────────────────────
# 3. TRAIN ONE RANGER MODEL
#    Fits both a probability model (for per-image confidence scores)
#    and a Gini-importance model. Bootstrapping is applied inside here
#    so it can be called per CV fold as well as for the final model.
# ─────────────────────────────────────────────
fit_rf_models <- function(train_set, optimal_trees = 500) {
  train_boot <- bootstrap_glyphosate(train_set)
  train_boot <- train_boot %>% select(-glyphosate)
  
  list(
    model = ranger(class_label ~ ., data = train_boot,
                   num.trees = optimal_trees,
                   importance = "permutation",
                   probability = TRUE)
  )
}

# ─────────────────────────────────────────────
# 4. HYPERPARAMETER TUNING
#    Fits quick OOB models across a grid of tree counts and returns
#    the count with the lowest OOB classification error.
#    Note: OOB error from a probability forest is MSE-based; we use
#    a non-probability model here purely for tuning speed.
# ─────────────────────────────────────────────
tune_num_trees <- function(train_set, tree_grid = c(100, 300, 500, 800, 1500)) {
  cat("\n===== Tuning num.trees =====\n")
  
  train_boot <- bootstrap_glyphosate(train_set) %>% select(-glyphosate)
  results    <- data.frame(num_trees = integer(), oob_error = numeric())
  
  for (nt in tree_grid) {
    cat("Testing num.trees =", nt, "...")
    temp_model <- ranger(class_label ~ ., data = train_boot,
                         num.trees = nt, probability = FALSE)
    error <- temp_model$prediction.error
    cat(" OOB Error:", round(error * 100, 3), "%\n")
    results <- rbind(results, data.frame(num_trees = nt, oob_error = error))
  }
  
  best_trees <- results$num_trees[which.min(results$oob_error)]
  cat("--> Optimal number of trees:", best_trees, "\n")
  return(best_trees)
}

# ─────────────────────────────────────────────
# 5. GROUPED 5-FOLD CROSS-VALIDATION
#    Splits training parcels (by group_id) into 5 folds. Each fold
#    acts as a validation set once while the remaining 4 folds train.
#    Bootstrapping is applied fresh inside each fold's training portion.
#    Returns per-image predictions and fold-level accuracy metrics.
# ─────────────────────────────────────────────
run_cross_validation <- function(train_data, optimal_trees = 500, n_folds = 5, seed = 42) {
  set.seed(seed)
  cat("\n===== 5-Fold Cross-Validation =====\n")
  
  predictor_cols <- PREDICTOR_COLS
  
  # Assign folds at the group_id (parcel) level so all images of a
  # parcel land in the same fold — consistent with the train/test split
  unique_groups  <- unique(train_data$group_id)
  fold_ids       <- sample(rep(1:n_folds, length.out = length(unique_groups)))
  group_fold_map <- data.frame(group_id = unique_groups, fold = fold_ids,
                               stringsAsFactors = FALSE)
  
  train_data <- train_data %>% left_join(group_fold_map, by = "group_id")
  
  all_fold_results <- list()
  fold_accuracies  <- numeric(n_folds)
  
  for (fold in 1:n_folds) {
    cat("\n-- Fold", fold, "/", n_folds, "--\n")
    
    fold_train <- train_data %>%
      filter(fold != !!fold) %>%
      select(class_label, glyphosate, all_of(predictor_cols))
    
    # Keep identifier columns in fold_val so they survive into the results
    fold_val <- train_data %>%
      filter(fold == !!fold)
    
    fold_val_predictors <- fold_val %>% select(class_label, glyphosate, all_of(predictor_cols))
    
    rf <- fit_rf_models(fold_train, optimal_trees = optimal_trees)
    
    # predict() on a probability forest returns a matrix: rows = observations,
    # cols = classes. The predicted class is whichever column has the highest prob.
    val_no_glyph <- fold_val_predictors %>% select(-glyphosate)
    prob_matrix  <- predict(rf$model, data = val_no_glyph)$predictions
    pred_classes <- colnames(prob_matrix)[apply(prob_matrix, 1, which.max)]
    pred_classes <- factor(pred_classes, levels = levels(fold_val$class_label))
    
    # Predicted probability for the winning class for each image
    pred_probs <- apply(prob_matrix, 1, max)
    
    fold_acc <- mean(pred_classes == fold_val$class_label, na.rm = TRUE)
    fold_accuracies[fold] <- fold_acc
    cat("  Fold", fold, "accuracy:", round(fold_acc * 100, 2), "%\n")
    
    all_fold_results[[fold]] <- data.frame(
      split_type       = "cv",
      fold             = fold,
      group_id         = fold_val$group_id,
      parcel_id        = fold_val$parcel_id,
      image_date       = fold_val$image_date,
      true_class       = fold_val$class_label,
      predicted_class  = pred_classes,
      predicted_prob   = round(pred_probs, 4),
      stringsAsFactors = FALSE
    )
  }
  
  cv_results <- dplyr::bind_rows(all_fold_results)
  
  cat("\n===== CV Summary =====\n")
  cat("Per-fold accuracies:", paste(round(fold_accuracies * 100, 2), collapse = "%, "), "%\n")
  cat("Mean CV accuracy:   ", round(mean(fold_accuracies) * 100, 2), "%\n")
  cat("SD CV accuracy:     ", round(sd(fold_accuracies) * 100, 2), "%\n")
  
  return(list(cv_results = cv_results, fold_accuracies = fold_accuracies))
}

# ─────────────────────────────────────────────
# 6. EVALUATE ON HELD-OUT TEST SET
#    Runs the final trained model against the fixed 20% test set,
#    adds a predicted_prob column (probability of the predicted class),
#    and reports confusion matrix + variable importance.
# ─────────────────────────────────────────────
evaluate_rf_model <- function(rf_models, test_set) {
  model <- rf_models$model
  
  predictor_cols <- PREDICTOR_COLS
  test_pred_data <- test_set %>% select(all_of(predictor_cols))
  
  prob_matrix  <- predict(model, data = test_pred_data)$predictions
  pred_classes <- colnames(prob_matrix)[apply(prob_matrix, 1, which.max)]
  pred_classes <- factor(pred_classes, levels = levels(test_set$class_label))
  pred_probs   <- apply(prob_matrix, 1, max)
  
  cm <- caret::confusionMatrix(pred_classes, test_set$class_label)
  
  cat("\n===== Confusion Matrix (Test Set) =====\n")
  print(cm$table)
  cat("\n===== Overall Accuracy:", round(cm$overall["Accuracy"] * 100, 2), "% =====\n")
  cat("\n===== Per-Class Stats =====\n")
  per_class <- cm$byClass
  
  # Rename to conventional ML terminology and add F1
  stats <- data.frame(
    Recall      = round(per_class[, "Sensitivity"],    3),
    Specificity = round(per_class[, "Specificity"],    3),
    Precision   = round(per_class[, "Pos Pred Value"], 3)
  )
  stats$F1 <- round(
    2 * (stats$Precision * stats$Recall) / (stats$Precision + stats$Recall),
    3
  )
  # Replace NaN F1 (when both precision and recall are 0) with 0
  stats$F1[is.nan(stats$F1)] <- 0
  print(stats)
  
  cat("\n===== Variable Importance: Permutation =====\n")
  print(sort(ranger::importance(model), decreasing = TRUE))
  
  # Return test set with predictions and per-image probability appended
  test_set$predicted_class <- pred_classes
  test_set$predicted_prob  <- round(pred_probs, 4)
  
  return(list(cm = cm, test_set_with_predictions = test_set))
}

# ─────────────────────────────────────────────
# 7. CORRELATION PLOTS
# ─────────────────────────────────────────────
plot_correlation <- function(df, type = "all", output_path) {
  all_mean_cols <- grep("_mean$", names(df), value = TRUE)
  band_cols     <- grep("^[Bb][0-9]", all_mean_cols, value = TRUE)
  index_cols    <- setdiff(all_mean_cols, band_cols)
  
  mean_cols <- switch(type,
                      all     = all_mean_cols,
                      bands   = band_cols,
                      indices = index_cols,
                      stop("type must be 'all', 'bands', or 'indices'")
  )
  
  title_label <- switch(type,
                        all     = "Correlation matrix — bands & indices",
                        bands   = "Correlation matrix — bands only",
                        indices = "Correlation matrix — indices only"
  )
  
  cor_matrix <- cor(df[, mean_cols], use = "pairwise.complete.obs")
  div_colors <- colorRampPalette(c("#4575b4", "white", "#d73027"))(200)
  
  draw_plot <- function() {
    corrplot(cor_matrix, method = "color", type = "lower",
             tl.cex = 0.8, col = div_colors, addCoef.col = "black",
             number.cex = 0.6, diag = FALSE, title = title_label,
             mar = c(0, 0, 2, 0))
  }
  
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  png(output_path, width = 10, height = 8, units = "in", res = 150)
  draw_plot()
  dev.off()
  draw_plot()
  
  cat("Correlation matrix saved to:", output_path, "\n")
  print(round(cor_matrix, 2))
}

# ─────────────────────────────────────────────
# 8. RUN ONE SCENARIO
#    Encapsulates the full pipeline for a single scenario so MAIN
#    can loop over all four without repeating code.
#    scenario_name : label used in output file names
#    train_data    : rows used for CV + final model training
#    test_data     : held-out rows used for final evaluation only
# ─────────────────────────────────────────────
run_scenario <- function(scenario_name, train_data, test_data,
                         optimal_trees, output_dir = "results") {
  cat("\n\n══════════════════════════════════════════\n")
  cat("  SCENARIO:", scenario_name, "\n")
  cat("══════════════════════════════════════════\n")
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 5-fold CV on training data
  cv <- run_cross_validation(train_data, optimal_trees = optimal_trees)
  
  # Final model trained on ALL training data (with bootstrapping)
  predictor_cols <- PREDICTOR_COLS
  final_train    <- train_data %>% select(class_label, glyphosate, all_of(predictor_cols))
  rf_models      <- fit_rf_models(final_train, optimal_trees = optimal_trees)
  
  # Evaluate on held-out test set
  test_clean <- test_data %>%
    select(class_label, all_of(predictor_cols)) %>%
    mutate(class_label = factor(class_label, levels = levels(train_data$class_label)))
  
  eval <- evaluate_rf_model(rf_models, test_clean)
  
  # Save outputs
  model_path <- file.path(output_dir, paste0("rf_model_", scenario_name, ".rds"))
  saveRDS(rf_models$model, model_path)
  cat("Model saved to:", model_path, "\n")
  
  cv_path <- file.path(output_dir, paste0("cv_results_", scenario_name, ".csv"))
  write.csv(cv$cv_results, cv_path, row.names = FALSE)
  cat("CV results saved to:", cv_path, "\n")
  
  test_path <- file.path(output_dir, paste0("test_predictions_", scenario_name, ".csv"))
  write.csv(eval$test_set_with_predictions, test_path, row.names = FALSE)
  cat("Test predictions saved to:", test_path, "\n")
  
  return(list(cv = cv, eval = eval, model = rf_models$model))
}

# ═════════════════════════════════════════════
# MAIN WORKFLOW
# ═════════════════════════════════════════════

image_csv_paths  <- c("metadata/image_metadata2020_final.csv",
                      "metadata/image_metadata2025_final.csv")
parcel_csv_paths <- c("metadata/parcel_metadata2020_final.csv",
                      "metadata/parcel_metadata2025_final.csv")
correlation_dir  <- "correlation"
results_dir      <- "results"

# ── Load all data ──────────────────────────────────────────────────────────────
data_2020 <- prepare_training_data(image_csv_paths[1], parcel_csv_paths[1])
data_2025 <- prepare_training_data(image_csv_paths[2], parcel_csv_paths[2])
data_both <- bind_rows(data_2020, data_2025) %>%
  mutate(class_label = as.factor(class_label))

# ── Correlation plots (run once on combined data) ─────────────────────────────
plot_correlation(data_both, type = "all",
                 output_path = file.path(correlation_dir, "corr_matr_all.png"))
plot_correlation(data_both, type = "bands",
                 output_path = file.path(correlation_dir, "corr_matr_bands.png"))
plot_correlation(data_both, type = "indices",
                 output_path = file.path(correlation_dir, "corr_matr_indices.png"))

# ── Tune tree count once on combined data (reused across all scenarios) ────────
predictor_cols <- PREDICTOR_COLS
tune_train     <- data_both %>% select(class_label, glyphosate, all_of(predictor_cols))
best_trees     <- tune_num_trees(tune_train)

# ── Helper: grouped 80/20 holdout split ───────────────────────────────────────
grouped_split <- function(data, train_fraction = 0.8, seed = 42) {
  set.seed(seed)
  unique_groups <- unique(data$group_id)
  train_groups  <- sample(unique_groups, size = round(length(unique_groups) * train_fraction))
  list(
    train = data %>% filter(group_id %in% train_groups),
    test  = data %>% filter(!group_id %in% train_groups)
  )
}

# ── Scenario 1: 2020 only ─────────────────────────────────────────────────────
s1 <- grouped_split(data_2020)
results_s1 <- run_scenario("scenario1_2020only",
                           train_data    = s1$train,
                           test_data     = s1$test,
                           optimal_trees = best_trees,
                           output_dir    = results_dir)

# ── Scenario 2: 2025 only ─────────────────────────────────────────────────────
s2 <- grouped_split(data_2025)
results_s2 <- run_scenario("scenario2_2025only",
                           train_data    = s2$train,
                           test_data     = s2$test,
                           optimal_trees = best_trees,
                           output_dir    = results_dir)

# ── Scenario 3: both years combined ───────────────────────────────────────────
s3 <- grouped_split(data_both)
results_s3 <- run_scenario("scenario3_both",
                           train_data    = s3$train,
                           test_data     = s3$test,
                           optimal_trees = best_trees,
                           output_dir    = results_dir)

# ── Scenario 4: train on 2020, test on 2025 ───────────────────────────────────
# No holdout split needed — the years themselves define the split.
# group_id is still created in both datasets for consistency.
results_s4 <- run_scenario("scenario4_train2020_test2025",
                           train_data    = data_2020,
                           test_data     = data_2025,
                           optimal_trees = best_trees,
                           output_dir    = results_dir)

cat("\n===== All scenarios complete! =====\n")