# GUARD-Leeuwarden

**Glyphosate Usage Assessment and Remote Detection**

GUARD-Leeuwarden is an Academic Consultancy Training (ACT) project within the Remote Sensing and GIS Integration programme at Wageningen University & Research (WUR).

The project investigates the detection of glyphosate application in agricultural fields using remote sensing, GIS, and machine learning techniques. Using the municipality of Leeuwarden as a case study, the project aims to improve and evaluate methods for monitoring glyphosate use from satellite imagery.

## Project Partners

- Wageningen University & Research (WUR)
- Sweco Nederland
- Municipality of Leeuwarden

## Objectives

- Detect glyphosate application using Sentinel-2 imagery
- Improve existing detection workflows
- Evaluate model performance and reliability
- Develop recommendations for operational monitoring

## Status
 
✅ Model trained and validated. The pipeline can be used as-is to detect glyphosate application for any study area using the already-trained Random Forest model, or extended with new training data for other regions, or years.

## Repository Structure
 
```text
data_collection.R                   # Builds a new training dataset from scratch (sampling + Sentinel-2 download)
labelling.R                         # Launches the Shiny app for manual parcel labelling
labelling_finalise_metadata.R       # Consolidates labelled metadata into a single training-ready dataset
train_rf.R                          # Trains the Random Forest model on the finalised, labelled dataset
get_results.R                       # End-to-end prediction pipeline for a new study area
 
sampling/                           # Stratified parcel sampling and citizen-science observation linkage
  create_country_grid.R
  prepare_soil_brp.R
  summarise_soils.R
  waarneming_obs.R
  points_to_parcels.R
  sample_brp_parcels.R
 
downloading/                        # Sentinel-2 imagery retrieval and preprocessing
  sentinel_download_and_statistics.R  # Used by data_collection.R to build training data
  results_download.R                  # Used by get_results.R to download/regularise imagery for prediction
  images/                              # Downloaded RGB patches (not version-controlled)
 
labeling/                           # Semi-automated parcel labelling
  app.R                             # R Shiny labelling application
  finalise_metadata.R               # Metadata consolidation script
 
random_forest/                      # Model training and prediction
  random_forest.R                   # RFC training, cross-validation, and evaluation
  random_forest_predict.R           # Applies the trained model to new parcel observations
  rf_model.rds                      # Trained model used by get_results.R
  leeuwarden_brp_parcels.gpkg       # Example study-area parcels (Leeuwarden)
 
detection_algorithm/                # Temporal pattern recognition
  pattern_recognition.R             # Sliding-window glyphosate detection from per-image class predictions
```
 
Large data files (Sentinel-2 imagery, GeoPackages) are generally not committed to the repository due to their size but are fully reproducible from publicly accessible sources using the documented scripts. Version control is applied to all code via Git; empty folders contain a `.gitkeep` file to preserve directory structure.

## Usage
 
### Predicting glyphosate use for a new study area
 
If you only want to apply the model that was trained during this project, you don't need to retrain anything. Use [`get_results.R`](get_results.R):
 
1. Prepare a GeoPackage containing polygons of the fields you want to inspect.
2. Set `yr` and `input_vector_file` (the path to your GeoPackage) at the top of `get_results.R`.
3. Run the script. It will:
   - download and regularise Sentinel-2 imagery for your fields and compute per-parcel statistics ([`downloading/results_download.R`](downloading/results_download.R)),
   - classify each image with the trained Random Forest model ([`random_forest/random_forest_predict.R`](random_forest/random_forest_predict.R)),
   - run the temporal moving-window detection algorithm to flag glyphosate-treated parcels ([`detection_algorithm/pattern_recognition.R`](detection_algorithm/pattern_recognition.R)),
   - and join the results back onto your input GeoPackage.
### Collecting new training data and retraining the model
 
To extend or retrain the model (e.g. for a different study area, additional years, or extra observations):
 
1. **Collect data** with [`data_collection.R`](data_collection.R), which runs the full sampling and Sentinel-2 download pipeline (grid creation, BRP/soil preparation, Waarnemingen.nl observation download, stratified parcel sampling, and imagery download).
2. **Label parcels** with [`labelling.R`](labelling.R), which launches the R Shiny labelling app for manual colour-stage labelling of the downloaded image patches.
3. **Finalise the labelled metadata** with [`labelling_finalise_metadata.R`](labelling_finalise_metadata.R), which consolidates and cleans the per-year labelling output into a single training-ready dataset.
4. **Train the model** with [`train_rf.R`](train_rf.R), which trains and evaluates the Random Forest Classifier on the finalised dataset.
Each of these top-level scripts is a thin wrapper around the corresponding scripts in `sampling/`, `downloading/`, `labeling/`, and `random_forest/`, and exposes the key parameters (study years, sample sizes, file paths) that need to be set before running. Some scripts require additional parameters to be set inside the sourced files themselves — check the inline comments before running.

## License

Copyright (C) 2026 Dirk Emaus, Nout Heine, Mathijs Keijzer, Jan Müldner, Rik Oudega

This software is licensed under the GNU General Public License v3.0. See `LICENSE` for details.

In addition, Sweco Group is granted a separate commercial license allowing internal use, modification, integration into proprietary software, and distribution without the copyleft obligations of the GPL.

This commercial license applies only to the Sweco Group and its subsidiaries, unless otherwise agreed in writing.
