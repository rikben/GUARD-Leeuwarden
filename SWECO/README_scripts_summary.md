# README – Summary of RF v2 Scripts

=================================

## This file summarizes the workflow and outputs of:
- rf_train_v2.R
- rf_predict_v2.R
- rf_predict_new_v2.R


## 1) rf_train_v2.R
## Purpose:
Train and tune a Random Forest classifier (ranger via caret) for parcel class prediction.

## Main steps:
- Loads training data from dataset_v2.shp.
- Cleans/filter data (removes NA class, class 7, NA blue_stdev). Assuming if blue_stdev is filled in, the rest is as well.
- Standardizes truncated field name green_stde -> green_stdev.
## - Uses nested cross-validation:
  - Outer CV for unbiased performance estimate.
  - Inner CV (random search) for tuning mtry/splitrule/min.node.size.
  - Loops manually over multiple num.trees (ntree) values.
- Selects best ntree from mean outer-fold accuracy.
- Trains final model on full filtered dataset.
- Exports comparison shapefile with truth vs training prediction.
- Saves final trained model as RF_model_glyph_v2.rds.

## Key outputs:
- rf_model_results_v2.shp
- C:/Users/NL1G7U/Documents/stage_proj/Data/rf_modellen/RF_model_glyph_v2.rds


## 2) rf_predict_v2.R
## Purpose:
Apply the trained model to dated RF_traindata shapefiles and detect time-patterns per parcel.

## Main steps:
- Loads RF_model_glyph_v2.rds.
- Loads reference parcel layer (dataset_v2.shp), filters records, and adds parcel_index.
- Finds input files matching RF_traindata_<date>.shp and sorts by date.
## - For each file:
  - Reads shapefile and aligns CRS to reference data.
  - Fixes green_stde naming if needed.
  - Verifies all predictor columns exist.
  - Spatially matches parcels (st_equals, fallback st_intersects).
  - Predicts class for matched parcels.
- Builds a wide time-series table (pred_YYYY-MM-DD columns).
- Runs rolling 28-day pattern detection using accepted class sequences.
- Adds date_* columns and writes table to CSV.
- Converts to sf and shortens field names for DBF 10-character limit.
- Writes final shapefile output.

## Key outputs:
- rf_parcel_predictions_over_time_V3.csv
- glyf_pred_v3.shp


## 3) rf_predict_new_v2.R
## Purpose:
Apply the same model to Leeuwarden parcel date-files with parallel prediction and pattern export.

## Main steps:
- Loads RF_model_glyph_v2.rds and predictor list.
- Finds leeuwarden_percelen_<date>.shp files and sorts by date.
## - Runs per-file prediction in parallel (foreach + doParallel):
  - Reads each date file.
  - Fixes green_stde naming if needed.
  - Validates required predictors.
  - Filters rows where blue_stdev is valid/non-zero.
  - Predicts classes and stores geometry as WKT.
- Combines all predictions, deduplicates per geometry/date, and pivots wide.
- Orders pred_* columns chronologically and creates date_* columns.
- Performs rolling 28-day pattern detection (same sequence logic as rf_predict_v2.R).
- Rebuilds geometry from WKT.
- Converts field names to shapefile-safe DBF names (<=10 chars, unique).
## - Writes:
  - Full classified parcel shapefile.
  - Subset shapefile with detected pattern parcels.

## Key outputs:
- leeuwarden_classified_parcels_v2.shp
- leeuwarden_percelen_pattern_spotted_v2.shp

Workflow order
## Typical run order is:
1. rf_train_v2.R (train + save model)
2. rf_predict_v2.R (predict on RF_traindata files)
3. rf_predict_new_v2.R (predict on Leeuwarden parcel files)