library(dplyr)
library(ranger)
library(caret)
library(corrplot)

# ─────────────────────────────────────────────
# PREDICTOR COLUMNS
#   Full set of candidate predictors. suggested_class_label is excluded
#   because it is derived from / too correlated with the target class_label.
#   Individual predictors can still be commented out for ad-hoc testing.
# ─────────────────────────────────────────────
ALL_PREDICTOR_COLS <- c(
  "ndvi_mean",  "ndvi_sd",
  "ndre_mean",  "ndre_sd",
  "gndvi_mean", "gndvi_sd",
  "nirv_mean",  "nirv_sd",
  "ndwi_mean",  "ndwi_sd",
  "B02_mean",   "B02_sd",
  "B03_mean",   "B03_sd",
  "B04_mean",   "B04_sd",
  "B05_mean",   "B05_sd",
  "B08_mean",   "B08_sd",
  "B8A_mean",   "B8A_sd",
  "B11_mean",   "B11_sd"
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
    
    # Only fix columns that actually exist in this file
    numeric_cols <- intersect(ALL_PREDICTOR_COLS, names(raw_data))
    raw_data[numeric_cols] <- lapply(raw_data[numeric_cols], fix_european_numeric)
    
    discarded_parcels <- parcel_data %>%
      filter(toupper(as.character(discarded)) == "TRUE") %>%
      pull(parcel_id)
    
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
#    Must be called only on training folds, never on validation folds.
# ─────────────────────────────────────────────
bootstrap_glyphosate <- function(train_raw) {
  glyph_1 <- train_raw %>% filter(glyphosate == 1)
  glyph_0 <- train_raw %>% filter(glyphosate == 0)
  
  if (nrow(glyph_1) == 0 || nrow(glyph_0) == 0) {
    warning("One glyphosate class is absent in training fold — skipping bootstrap.")
    return(train_raw)
  }
  
  n_glyph_0_target  <- 2 * nrow(glyph_1)
  glyph_0_upsampled <- glyph_0[sample(nrow(glyph_0), n_glyph_0_target, replace = TRUE), ]
  
  bootstrapped <- bind_rows(glyph_1, glyph_0_upsampled)
  cat("  Bootstrap: glyphosate=1:", nrow(glyph_1),
      "| glyphosate=0 (upsampled 2x):", nrow(glyph_0_upsampled),
      "| total:", nrow(bootstrapped), "\n")
  return(bootstrapped)
}

# ─────────────────────────────────────────────
# 3. IN-FOLD FEATURE SELECTION
#    Fits a full-feature model on the (bootstrapped) training fold,
#    computes permutation importances, and returns only those features
#    whose importance exceeds the mean importance across all features.
#    This is called exclusively on training fold data.
# ─────────────────────────────────────────────
select_features_by_importance <- function(train_boot, candidate_cols, num_trees = 500) {
  train_select <- train_boot %>% select(class_label, all_of(candidate_cols))
  
  full_model <- ranger(
    class_label ~ .,
    data        = train_select,
    num.trees   = num_trees,
    importance  = "permutation",
    probability = FALSE   # classification mode is sufficient for selection
  )
  
  imp        <- ranger::importance(full_model)
  mean_imp   <- mean(imp)
  selected   <- names(imp[imp > mean_imp])
  
  cat("  Feature selection: mean importance =", round(mean_imp, 5),
      "| kept", length(selected), "of", length(imp), "features\n")
  cat("  Selected:", paste(selected, collapse = ", "), "\n")
  
  return(list(selected = selected, importances = imp))
}

# ─────────────────────────────────────────────
# 4. FIT ONE RANGER MODEL
#    Fits a probability forest on an already-bootstrapped, already-
#    feature-selected training set. Returns model + the feature list
#    so callers can record which features each fold used.
# ─────────────────────────────────────────────
fit_rf_model <- function(train_boot, selected_features, num_trees = 500) {
  train_fit <- train_boot %>% select(class_label, all_of(selected_features))
  
  model <- ranger(
    class_label ~ .,
    data        = train_fit,
    num.trees   = num_trees,
    importance  = "permutation",
    probability = TRUE
  )
  
  return(model)
}

# ─────────────────────────────────────────────
# 5. HYPERPARAMETER TUNING (inside CV)
#    Fits quick OOB non-probability models across a tree-count grid
#    using only the supplied training fold data.
#    Returns the tree count with lowest OOB classification error.
# ─────────────────────────────────────────────
tune_num_trees <- function(train_boot, selected_features,
                           tree_grid = c(100, 300, 500, 800, 1500)) {
  train_tune <- train_boot %>% select(class_label, all_of(selected_features))
  results    <- data.frame(num_trees = integer(), oob_error = numeric())
  
  for (nt in tree_grid) {
    temp_model <- ranger(class_label ~ ., data = train_tune,
                         num.trees = nt, probability = FALSE)
    results <- rbind(results, data.frame(num_trees = nt,
                                         oob_error = temp_model$prediction.error))
  }
  
  best <- results$num_trees[which.min(results$oob_error)]
  cat("  Tree tuning results:\n")
  print(results)
  cat("  --> Optimal num.trees:", best, "\n")
  return(best)
}

# ─────────────────────────────────────────────
# 6. GROUPED K-FOLD CROSS-VALIDATION  ← primary evaluation method
#
#    For each fold:
#      (a) Bootstrap the training folds (class balancing).
#      (b) Select features on bootstrapped training data (mean-importance rule).
#      (c) Tune num.trees on bootstrapped training data with selected features.
#      (d) Refit final fold model (probability forest) with optimal trees.
#      (e) Predict on the validation fold; record per-image results.
#
#    All steps that touch labels happen only on training folds.
#    The validation fold is never seen until step (e).
#
#    Returns:
#      cv_results       — per-image predictions for every fold
#      fold_metrics     — accuracy, per-class stats, confusion matrix per fold
#      fold_importances — raw importance vector per fold
#      fold_features    — which features were selected in each fold
# ─────────────────────────────────────────────
run_cross_validation <- function(data, candidate_cols,
                                 n_folds   = 5,
                                 tree_grid = c(100, 300, 500, 800, 1500),
                                 seed      = 42) {
  set.seed(seed)
  cat("\n===== Grouped", n_folds, "-Fold Cross-Validation =====\n")
  
  # Assign folds at parcel level so all images of a parcel stay together
  unique_groups  <- unique(data$group_id)
  fold_ids       <- sample(rep(1:n_folds, length.out = length(unique_groups)))
  group_fold_map <- data.frame(group_id = unique_groups, fold = fold_ids,
                               stringsAsFactors = FALSE)
  data <- data %>% left_join(group_fold_map, by = "group_id")
  
  all_row_results  <- list()
  fold_metrics     <- list()
  fold_importances <- list()
  fold_features    <- list()
  
  for (fold in 1:n_folds) {
    cat("\n── Fold", fold, "/", n_folds, "──\n")
    
    # ── (a) Split ────────────────────────────────────────────────────────────
    fold_train_raw <- data %>%
      filter(fold != !!fold) %>%
      select(class_label, glyphosate, all_of(candidate_cols))
    
    fold_val <- data %>% filter(fold == !!fold)
    
    # ── (b) Bootstrap (training folds only) ──────────────────────────────────
    fold_train_boot <- bootstrap_glyphosate(fold_train_raw)
    
    # ── (c) Feature selection (training folds only) ──────────────────────────
    sel <- select_features_by_importance(
      fold_train_boot %>% select(-glyphosate),
      candidate_cols
    )
    selected   <- sel$selected
    fold_features[[fold]]    <- selected
    fold_importances[[fold]] <- sel$importances  # full importance vector
    
    # ── (d) Tune num.trees (training folds only) ─────────────────────────────
    best_trees <- tune_num_trees(
      fold_train_boot %>% select(-glyphosate),
      selected,
      tree_grid = tree_grid
    )
    
    # ── (e) Fit final fold model ──────────────────────────────────────────────
    fold_model <- fit_rf_model(
      fold_train_boot %>% select(-glyphosate),
      selected,
      num_trees = best_trees
    )
    
    # ── (f) Predict on validation fold ───────────────────────────────────────
    val_predictors <- fold_val %>% select(all_of(selected))
    prob_matrix    <- predict(fold_model, data = val_predictors)$predictions
    pred_classes   <- factor(
      colnames(prob_matrix)[apply(prob_matrix, 1, which.max)],
      levels = levels(data$class_label)
    )
    pred_probs <- apply(prob_matrix, 1, max)
    
    # ── (g) Fold-level metrics ────────────────────────────────────────────────
    true_classes <- factor(fold_val$class_label, levels = levels(data$class_label))
    cm           <- caret::confusionMatrix(pred_classes, true_classes)
    fold_acc     <- cm$overall["Accuracy"]
    
    per_class <- cm$byClass
    stats_df  <- data.frame(
      fold        = fold,
      class       = rownames(per_class),
      Recall      = round(per_class[, "Sensitivity"],    4),
      Specificity = round(per_class[, "Specificity"],    4),
      Precision   = round(per_class[, "Pos Pred Value"], 4),
      stringsAsFactors = FALSE
    )
    stats_df$F1 <- round(
      2 * (stats_df$Precision * stats_df$Recall) /
        (stats_df$Precision + stats_df$Recall), 4
    )
    stats_df$F1[is.nan(stats_df$F1)] <- 0
    
    cat("  Fold", fold, "accuracy:", round(fold_acc * 100, 2), "%\n")
    
    fold_metrics[[fold]] <- list(
      fold          = fold,
      accuracy      = fold_acc,
      confusion     = cm$table,
      per_class     = stats_df,
      best_trees    = best_trees,
      n_train       = nrow(fold_train_boot),
      n_val         = nrow(fold_val)
    )
    
    # ── (h) Per-image results ─────────────────────────────────────────────────
    all_row_results[[fold]] <- data.frame(
      fold            = fold,
      group_id        = fold_val$group_id,
      parcel_id       = fold_val$parcel_id,
      image_date      = fold_val$image_date,
      true_class      = as.character(true_classes),
      predicted_class = as.character(pred_classes),
      predicted_prob  = round(pred_probs, 4),
      correct         = pred_classes == true_classes,
      stringsAsFactors = FALSE
    )
  }
  
  cv_results <- dplyr::bind_rows(all_row_results)
  
  # ── Overall CV summary ────────────────────────────────────────────────────
  fold_accs <- sapply(fold_metrics, `[[`, "accuracy")
  cat("\n===== CV Summary =====\n")
  cat("Per-fold accuracies:", paste(round(fold_accs * 100, 2), collapse = "%, "), "%\n")
  cat("Mean CV accuracy:   ", round(mean(fold_accs) * 100, 2), "%\n")
  cat("SD CV accuracy:     ", round(sd(fold_accs) * 100, 2), "%\n")
  
  return(list(
    cv_results       = cv_results,
    fold_metrics     = fold_metrics,
    fold_importances = fold_importances,
    fold_features    = fold_features,
    fold_accuracies  = fold_accs
  ))
}

# ─────────────────────────────────────────────
# 7. AGGREGATE CV OUTPUTS → CSV / TEXT FILES
#    Writes all statistical outputs for a scenario to disk.
# ─────────────────────────────────────────────
save_cv_outputs <- function(cv, scenario_name, output_dir) {
  
  # ── 7a. Per-image predictions ─────────────────────────────────────────────
  write.csv(cv$cv_results,
            file.path(output_dir, paste0(scenario_name, "_cv_predictions.csv")),
            row.names = FALSE)
  
  # ── 7b. Fold-level accuracy summary ──────────────────────────────────────
  fold_summary <- data.frame(
    fold       = seq_along(cv$fold_accuracies),
    accuracy   = round(cv$fold_accuracies * 100, 4),
    n_train    = sapply(cv$fold_metrics, `[[`, "n_train"),
    n_val      = sapply(cv$fold_metrics, `[[`, "n_val"),
    best_trees = sapply(cv$fold_metrics, `[[`, "best_trees")
  )
  fold_summary <- rbind(
    fold_summary,
    data.frame(fold = "MEAN",
               accuracy   = round(mean(cv$fold_accuracies) * 100, 4),
               n_train    = NA, n_val = NA, best_trees = NA),
    data.frame(fold = "SD",
               accuracy   = round(sd(cv$fold_accuracies) * 100, 4),
               n_train    = NA, n_val = NA, best_trees = NA)
  )
  write.csv(fold_summary,
            file.path(output_dir, paste0(scenario_name, "_fold_accuracy_summary.csv")),
            row.names = FALSE)
  
  # ── 7c. Per-class stats across folds ─────────────────────────────────────
  per_class_all <- dplyr::bind_rows(lapply(cv$fold_metrics, `[[`, "per_class"))
  write.csv(per_class_all,
            file.path(output_dir, paste0(scenario_name, "_per_class_per_fold.csv")),
            row.names = FALSE)
  
  # Aggregate: mean per-class stats across folds
  per_class_mean <- per_class_all %>%
    group_by(class) %>%
    summarise(
      mean_Recall      = round(mean(Recall,      na.rm = TRUE), 4),
      mean_Specificity = round(mean(Specificity, na.rm = TRUE), 4),
      mean_Precision   = round(mean(Precision,   na.rm = TRUE), 4),
      mean_F1          = round(mean(F1,          na.rm = TRUE), 4),
      sd_F1            = round(sd(F1,            na.rm = TRUE), 4),
      .groups = "drop"
    )
  write.csv(per_class_mean,
            file.path(output_dir, paste0(scenario_name, "_per_class_mean_across_folds.csv")),
            row.names = FALSE)
  
  # ── 7d. Confusion matrices — one per fold + aggregate ────────────────────
  sink(file.path(output_dir, paste0(scenario_name, "_confusion_matrices.txt")))
  cat("=== Confusion Matrices:", scenario_name, "===\n\n")
  for (m in cv$fold_metrics) {
    cat("Fold", m$fold, "| Accuracy:", round(m$accuracy * 100, 2), "%\n")
    print(m$confusion)
    cat("\n")
  }
  
  # Aggregate confusion matrix (sum across folds)
  cm_agg <- Reduce("+", lapply(cv$fold_metrics, `[[`, "confusion"))
  cat("=== Aggregated Confusion Matrix (sum across all folds) ===\n")
  print(cm_agg)
  overall_acc <- sum(diag(cm_agg)) / sum(cm_agg)
  cat("Overall accuracy (aggregated):", round(overall_acc * 100, 2), "%\n")
  sink()
  
  # ── 7e. Feature importances per fold ─────────────────────────────────────
  imp_list <- lapply(seq_along(cv$fold_importances), function(f) {
    imp <- cv$fold_importances[[f]]
    data.frame(fold     = f,
               feature  = names(imp),
               importance = round(imp, 6),
               selected = names(imp) %in% cv$fold_features[[f]],
               stringsAsFactors = FALSE)
  })
  imp_df <- dplyr::bind_rows(imp_list)
  write.csv(imp_df,
            file.path(output_dir, paste0(scenario_name, "_feature_importances_per_fold.csv")),
            row.names = FALSE)
  
  # Mean importance + selection frequency across folds
  imp_summary <- imp_df %>%
    group_by(feature) %>%
    summarise(
      mean_importance    = round(mean(importance), 6),
      sd_importance      = round(sd(importance),   6),
      selection_count    = sum(selected),
      selection_fraction = round(mean(selected),   3),
      majority_selected  = mean(selected) > 0.5,
      .groups = "drop"
    ) %>%
    arrange(desc(mean_importance))
  write.csv(imp_summary,
            file.path(output_dir, paste0(scenario_name, "_feature_importance_summary.csv")),
            row.names = FALSE)
  
  cat("Outputs written to:", output_dir, "\n")
  
  return(list(per_class_mean  = per_class_mean,
              imp_summary     = imp_summary,
              agg_confusion   = cm_agg,
              overall_acc_agg = overall_acc))
}

# ─────────────────────────────────────────────
# 8. FINAL MODEL
#    After CV, refit on ALL available data using:
#      - majority-vote feature set (selected in > 50% of folds)
#      - tree count = median of per-fold best_trees (avoids re-tuning on full data)
#    This model is for deployment; its accuracy is reported by CV, not here.
# ─────────────────────────────────────────────
fit_final_model <- function(data, cv, candidate_cols, output_dir, scenario_name) {
  cat("\n── Fitting final model on all data ──\n")
  
  # Majority-vote feature set
  imp_summary    <- cv$imp_summary  # already computed in save_cv_outputs
  majority_feats <- imp_summary$feature[imp_summary$majority_selected]
  cat("Majority-vote features (", length(majority_feats), "):",
      paste(majority_feats, collapse = ", "), "\n")
  
  # Median tree count across folds
  best_trees_per_fold <- sapply(cv$fold_metrics, `[[`, "best_trees")
  final_trees         <- as.integer(median(best_trees_per_fold))
  cat("Final num.trees (median of fold bests):", final_trees, "\n")
  
  # Bootstrap on all data, then fit
  all_train      <- data %>% select(class_label, glyphosate, all_of(candidate_cols))
  all_train_boot <- bootstrap_glyphosate(all_train)
  final_model    <- fit_rf_model(all_train_boot %>% select(-glyphosate),
                                 majority_feats,
                                 num_trees = final_trees)
  
  model_path <- file.path(output_dir, paste0(scenario_name, "_final_model.rds"))
  saveRDS(final_model, model_path)
  cat("Final model saved to:", model_path, "\n")
  
  # Save feature set used for deployment
  write.csv(
    data.frame(feature = majority_feats),
    file.path(output_dir, paste0(scenario_name, "_final_model_features.csv")),
    row.names = FALSE
  )
  
  return(final_model)
}

# ─────────────────────────────────────────────
# 9. CORRELATION PLOTS
#    Run once on combined data for exploration.
#    NOT used for feature selection (that happens per fold inside CV).
# ─────────────────────────────────────────────
plot_correlation <- function(df, candidate_cols, type = "all", output_path) {
  band_cols  <- grep("^B[0-9]", candidate_cols, value = TRUE)
  index_cols <- setdiff(candidate_cols, band_cols)
  
  cols_to_plot <- switch(type,
                         all     = candidate_cols,
                         bands   = band_cols,
                         indices = index_cols,
                         stop("type must be 'all', 'bands', or 'indices'")
  )
  title_label <- switch(type,
                        all     = "Correlation matrix — bands & indices",
                        bands   = "Correlation matrix — bands only",
                        indices = "Correlation matrix — indices only"
  )
  
  # Keep only columns that exist and are numeric
  cols_to_plot <- intersect(cols_to_plot, names(df))
  cols_to_plot <- cols_to_plot[sapply(df[cols_to_plot], is.numeric)]
  
  if (length(cols_to_plot) < 2) {
    warning("Not enough numeric columns for correlation plot of type '", type, "' — skipping.")
    return(invisible(NULL))
  }
  
  cor_matrix <- cor(df[, cols_to_plot], use = "pairwise.complete.obs", method = "pearson")
  div_colors <- colorRampPalette(c("#4575b4", "white", "#d73027"))(200)
  
  draw_plot <- function() {
    corrplot(cor_matrix, method = "color", type = "lower",
             tl.cex = 0.8, col = div_colors, addCoef.col = "black",
             number.cex = 0.6, diag = FALSE, title = title_label,
             mar = c(0, 0, 2, 0))
  }
  
  output_dir_path <- dirname(output_path)
  if (!dir.exists(output_dir_path)) dir.create(output_dir_path, recursive = TRUE)
  
  png(output_path, width = 10, height = 8, units = "in", res = 150)
  draw_plot()
  dev.off()
  draw_plot()
  
  # Save correlation matrix as CSV alongside the plot
  csv_path <- sub("\\.png$", ".csv", output_path)
  write.csv(round(cor_matrix, 4), csv_path)
  cat("Correlation matrix saved to:", output_path, "and", csv_path, "\n")
}

# ─────────────────────────────────────────────
# 10. RUN ONE SCENARIO
#     Encapsulates the full pipeline for a single scenario:
#       1. CV (evaluation + per-fold feature selection + tuning)
#       2. Save all CV outputs
#       3. Fit final model on all scenario data (majority-vote features)
# ─────────────────────────────────────────────
run_scenario <- function(scenario_name, data, candidate_cols,
                         tree_grid  = c(100, 300, 500, 800, 1500),
                         n_folds    = 5,
                         output_dir = "results") {
  cat("\n\n══════════════════════════════════════════\n")
  cat("  SCENARIO:", scenario_name, "\n")
  cat("══════════════════════════════════════════\n")
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Step 1: Cross-validation
  cv <- run_cross_validation(data,
                             candidate_cols = candidate_cols,
                             n_folds        = n_folds,
                             tree_grid      = tree_grid)
  
  # Step 2: Save all CV outputs and get importance summary
  cv_outputs <- save_cv_outputs(cv, scenario_name, output_dir)
  cv$imp_summary <- cv_outputs$imp_summary  # attach for use in fit_final_model
  
  # Step 3: Final model on all scenario data
  final_model <- fit_final_model(data, cv, candidate_cols, output_dir, scenario_name)
  
  return(list(cv = cv, cv_outputs = cv_outputs, final_model = final_model))
}


# ═════════════════════════════════════════════
# MAIN WORKFLOW
# ═════════════════════════════════════════════

image_csv_paths  <- c("../labeling/output/image_metadata2020_final.csv",
                      "../labeling/output/image_metadata2025_final.csv")
parcel_csv_paths <- c("../labeling/output/parcel_metadata2020_final.csv",
                      "../labeling/output/parcel_metadata2025_final.csv")
correlation_dir  <- "correlation"
results_dir      <- "results"

# ── Load all data ──────────────────────────────────────────────────────────────
data_2020 <- prepare_training_data(image_csv_paths[1], parcel_csv_paths[1])
data_2025 <- prepare_training_data(image_csv_paths[2], parcel_csv_paths[2])
data_both <- bind_rows(data_2020, data_2025) %>%
  mutate(class_label = as.factor(class_label))

# ── Correlation plots (exploratory only — not used for feature selection) ──────
if (!dir.exists(correlation_dir)) dir.create(correlation_dir)
plot_correlation(data_both, ALL_PREDICTOR_COLS, type = "all",
                 output_path = file.path(correlation_dir, "corr_all.png"))
plot_correlation(data_both, ALL_PREDICTOR_COLS, type = "bands",
                 output_path = file.path(correlation_dir, "corr_bands.png"))
plot_correlation(data_both, ALL_PREDICTOR_COLS, type = "indices",
                 output_path = file.path(correlation_dir, "corr_indices.png"))

# ── Scenario 1: 2020 only ─────────────────────────────────────────────────────
results_s1 <- run_scenario("scenario1_2020only",
                           data           = data_2020,
                           candidate_cols = ALL_PREDICTOR_COLS,
                           output_dir     = results_dir)

# ── Scenario 2: 2025 only ─────────────────────────────────────────────────────
results_s2 <- run_scenario("scenario2_2025only",
                           data           = data_2025,
                           candidate_cols = ALL_PREDICTOR_COLS,
                           output_dir     = results_dir)

# ── Scenario 3: both years combined ───────────────────────────────────────────
results_s3 <- run_scenario("scenario3_both",
                           data           = data_both,
                           candidate_cols = ALL_PREDICTOR_COLS,
                           output_dir     = results_dir)

# ── Scenario 4: train on 2020, evaluate on 2025 ───────────────────────────────
# This scenario is fundamentally a temporal transfer test, not a standard CV
# scenario. CV still runs on data_2020 (for model selection and evaluation),
# but we also do a separate held-out evaluation on the full data_2025 set
# using the final model — the only legitimate use of an explicit holdout here,
# because the year boundary is a meaningful external test of generalisation.
results_s4 <- run_scenario("scenario4_train2020_cv",
                           data           = data_2020,
                           candidate_cols = ALL_PREDICTOR_COLS,
                           output_dir     = results_dir)

# Apply the scenario 4 final model to 2025 as a temporal holdout evaluation
cat("\n── Scenario 4: temporal transfer evaluation (2025 as external test set) ──\n")
s4_majority_feats <- results_s4$cv_outputs$imp_summary$feature[
  results_s4$cv_outputs$imp_summary$majority_selected
]
s4_missing <- setdiff(s4_majority_feats, names(data_2025))
if (length(s4_missing) > 0)
  warning("Columns missing in 2025 data: ", paste(s4_missing, collapse = ", "))

s4_test_pred_data <- data_2025 %>%
  select(all_of(intersect(s4_majority_feats, names(data_2025))))
s4_prob_matrix    <- predict(results_s4$final_model, data = s4_test_pred_data)$predictions
s4_pred_classes   <- factor(
  colnames(s4_prob_matrix)[apply(s4_prob_matrix, 1, which.max)],
  levels = levels(data_both$class_label)
)
s4_true_classes   <- factor(data_2025$class_label, levels = levels(data_both$class_label))
s4_pred_probs     <- apply(s4_prob_matrix, 1, max)

s4_cm  <- caret::confusionMatrix(s4_pred_classes, s4_true_classes)
s4_acc <- s4_cm$overall["Accuracy"]
cat("Scenario 4 temporal transfer accuracy (2025):", round(s4_acc * 100, 2), "%\n")

# Save temporal transfer predictions
s4_transfer_df <- data_2025 %>%
  select(group_id, parcel_id, image_date) %>%
  mutate(
    true_class      = as.character(s4_true_classes),
    predicted_class = as.character(s4_pred_classes),
    predicted_prob  = round(s4_pred_probs, 4),
    correct         = s4_pred_classes == s4_true_classes
  )
write.csv(s4_transfer_df,
          file.path(results_dir, "scenario4_temporal_transfer_predictions.csv"),
          row.names = FALSE)

sink(file.path(results_dir, "scenario4_temporal_transfer_confusion.txt"))
cat("=== Scenario 4: Temporal Transfer (train=2020, test=2025) ===\n\n")
cat("Accuracy:", round(s4_acc * 100, 2), "%\n\n")
print(s4_cm$table)
sink()

cat("\n===== All scenarios complete =====\n")