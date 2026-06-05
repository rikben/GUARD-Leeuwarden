library(sf)
library(dplyr)
library(caret)
library(ranger)
library(doParallel)
library(foreach)

cat("\n=== Loading training data ===\n")
# Load the training shapefile and apply the same quality filters used in prior model runs.
# Class 7 is handpicked for parcels that I deemed unuseful for training (parcels that are divided up for different purposes)
data <- read_sf("C:/Users/NL1G7U/Documents/stage_proj/Data/bruikbare_datasets/dataset_v2.shp")
data <- as.data.frame(data)
data <- data %>% filter(!is.na(class)) %>% filter(class != 7) %>% filter(!is.na(blue_stdev))
names(data)[names(data) == "green_stde"] <- "green_stdev" # Saving shapefiles in FME cuts off too long column names
cat(sprintf("Data loaded: %d samples, %d features\n", nrow(data), ncol(data)))

predictors <- c("ndvi", "ndvi_stdev", "blue_mean", "green_mean", "red_mean", "nir_mean", "blue_stdev", "green_stdev", "red_stdev", "nir_stdev")
dep_var <- "class"

# ── Prepare data ──────────────────────────────────────────────────────────────
cat("\n=== Preparing training data ===\n")
# Keep only predictors + target to avoid leakage from non-feature columns.
train_data <- data[, c(predictors, dep_var)]
train_data[[dep_var]] <- as.factor(train_data[[dep_var]])
cat(sprintf("Target classes: %s\n", paste(levels(train_data[[dep_var]]), collapse = ", ")))

# ── Nested cross-validation ───────────────────────────────────────────────────
# Outer folds provide an unbiased performance estimate;
# inner CV (inside caret::train) tunes mtry, splitrule, and min.node.size.
# ntree is not a caret tuning parameter, so we loop over it manually.

set.seed(42)
n_outer   <- 5          # outer folds
n_inner   <- 5          # inner folds (used by caret trainControl)
ntree_values <- c(50, 90, 150, 400, 500, 600, 700, 800, 900, 1000)

# Split indices once so every ntree value is evaluated on identical outer folds.
outer_folds <- createFolds(train_data[[dep_var]], k = n_outer, list = TRUE, returnTrain = FALSE)

# Parallelise the outer folds (15 workers × 1 thread each avoids oversubscription).
cat("\n=== Starting nested cross-validation ===\n")
cat(sprintf("Outer folds: %d, Inner folds: %d, ntree values: %s\n", n_outer, n_inner, paste(ntree_values, collapse = ", ")))
cat("Registering 15 parallel workers...\n")
cl <- makeCluster(15)
registerDoParallel(cl)
cat(sprintf("Start time: %s\n", format(Sys.time(), "%H:%M:%S")))

nested_cv_results <- foreach(
  ntree = ntree_values,
  .combine  = rbind,
  .packages = c("caret", "ranger")
) %:% foreach(
  fold_idx = seq_along(outer_folds),
  .combine  = rbind,
  .packages = c("caret", "ranger")
) %dopar% {

  # Hold out one outer fold for testing and train on the remaining folds.
  test_idx   <- outer_folds[[fold_idx]]
  train_fold <- train_data[-test_idx, ]
  test_fold  <- train_data[ test_idx, ]

  # Inner CV: caret tunes mtry, splitrule, min.node.size
  inner_ctrl <- trainControl(
    method           = "cv",
    number           = n_inner,
    search           = "random",   # random search over caret's tunable params
    allowParallel    = FALSE       # worker already owns its core
  )

  fit <- train(
    x          = train_fold[, predictors],
    y          = train_fold[[dep_var]],
    method     = "ranger",
    trControl  = inner_ctrl,
    tuneLength = 10,               # 10 random candidate combinations
    # fixed args passed through to ranger()
    num.trees  = ntree,
    num.threads = 1                # one thread per parallel worker
  )

  preds   <- predict(fit, test_fold[, predictors])
  cm      <- confusionMatrix(preds, test_fold[[dep_var]])
  overall_acc <- cm$overall["Accuracy"]

  # Progress message (from worker)
  cat(sprintf("[ntree=%d, fold=%d] Accuracy: %.4f\n", ntree, fold_idx, overall_acc))

  # Return one result row per (ntree, outer fold) to summarize after all workers finish.
  data.frame(
    ntree        = ntree,
    fold         = fold_idx,
    accuracy     = overall_acc,
    kappa        = cm$overall["Kappa"],
    best_mtry    = fit$bestTune$mtry,
    best_splitrule = fit$bestTune$splitrule,
    best_min_node  = fit$bestTune$min.node.size
  )
}

stopCluster(cl)
registerDoSEQ()   # restore sequential execution
cat(sprintf("Nested CV complete at: %s\n", format(Sys.time(), "%H:%M:%S")))

# ── Summarise nested CV results ───────────────────────────────────────────────
cat("\n=== Nested CV Results ===\n")
cv_summary <- nested_cv_results %>%
  group_by(ntree) %>%
  summarise(
    mean_accuracy = mean(accuracy),
    sd_accuracy   = sd(accuracy),
    mean_kappa    = mean(kappa),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_accuracy))

print(cv_summary)
cat(sprintf("\nBest ntree selected: %d (mean accuracy: %.4f ± %.4f)\n", cv_summary$ntree[1], cv_summary$mean_accuracy[1], cv_summary$sd_accuracy[1]))

# ── Final model on full data with best ntree ──────────────────────────────────
# Refit on all training records using the best tree count found by nested CV.
best_ntree <- cv_summary$ntree[1]
cat("\n=== Training final model ===\n")
cat(sprintf("Using ntree = %d on full dataset\n", best_ntree))

# Re-register parallel backend for the final inner tuning
cat("Registering 8 parallel workers for final model tuning...\n")
cl <- makeCluster(15)
registerDoParallel(cl)
cat(sprintf("Start time: %s\n", format(Sys.time(), "%H:%M:%S")))

final_ctrl <- trainControl(
  method        = "cv",
  number        = n_inner,
  search        = "random",
  allowParallel = TRUE   # caret distributes inner folds across workers
)

final_model <- train(
  x          = train_data[, predictors],
  y          = train_data[[dep_var]],
  method     = "ranger",
  trControl  = final_ctrl,
  tuneLength = 10,
  num.trees  = best_ntree,
  importance = "impurity"
  # num.threads is handled by doParallel here
)

stopCluster(cl)
registerDoSEQ()
cat(sprintf("Final model training complete at: %s\n\n", format(Sys.time(), "%H:%M:%S")))

cat("=== Final Model Summary ===\n")
print(final_model)
cat("\n=== Variable Importance ===\n")
print(varImp(final_model))

# Predict compare

# Compare in-sample predictions against truth and export for spatial QA/QC.
train_pred <- predict(final_model$finalModel, train_data)$pred
truth <- train_data$class

compare <- data.frame( 
  truth = truth,
  train_pred = train_pred,
  geometry = data$geometry
)

compare$wrong <- compare$truth == compare$train_pred

compare <- st_as_sf(compare, sf_column_name = "geometry")
st_write(compare, "rf_model_results_v2.shp", append=FALSE)

# Persist the fitted model for downstream prediction scripts.
saveRDS(final_model, "C:/Users/NL1G7U/Documents/stage_proj/Data/rf_modellen/RF_model_glyph_v2.rds")
