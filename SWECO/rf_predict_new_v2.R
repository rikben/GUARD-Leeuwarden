library(sf)
library(dplyr)
library(tidyr)
library(caret)
library(ranger)
library(doParallel)
library(foreach)

# Load the trained model that will classify each parcel-date record.
final_model <- readRDS("C:/Users/NL1G7U/Documents/stage_proj/Data/rf_modellen/RF_model_glyph_v2.rds")

# Predictor set must match the feature set used during model training.
predictors <- c("ndvi", "ndvi_stdev", "blue_mean", "green_mean", "red_mean", "nir_mean", "blue_stdev", "green_stdev", "red_stdev", "nir_stdev")

data_dir <- "C:/Users/NL1G7U/Documents/stage_proj/Data/leeuwarden_percelen"

# Collect all dated parcel shapefiles for the prediction run.
rf_files <- list.files(
	path = data_dir,
	pattern = "^leeuwarden_percelen_([0-9]{8}|[0-9]{4}-[0-9]{2}-[0-9]{2})\\.shp$",
	full.names = TRUE
)

if (length(rf_files) == 0) {
	stop(sprintf("No leeuwarden_percelen*.shp files found in: %s", data_dir))
}

extract_file_date <- function(path) {
	fname <- basename(path)
	token <- sub("^leeuwarden_percelen_?(.*)\\.shp$", "\\1", fname)

	if (identical(token, fname) || token == "") {
		return(token)
	}

	if (grepl("^[0-9]{8}$", token)) {
		return(as.character(as.Date(token, format = "%Y%m%d")))
	}

	if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", token)) {
		return(as.character(as.Date(token)))
	}

	return(token)
}

file_dates <- vapply(rf_files, extract_file_date, character(1))
rf_files <- rf_files[order(file_dates)]
file_dates <- file_dates[order(file_dates)]

cat(sprintf("RF shapefiles found: %d\n", length(rf_files)))

# Preserve CRS for geometry reconstruction after wide-table processing.
output_crs <- st_crs(st_read(rf_files[1], quiet = TRUE))

# Parallelize per-file prediction because each date file is independent.
cl <- parallel::makeCluster(15)
doParallel::registerDoParallel(cl)
on.exit(parallel::stopCluster(cl), add = TRUE)

prediction_rows <- foreach(
	i = seq_along(rf_files),
	.packages = c("sf", "dplyr", "ranger", "caret")
) %dopar% {
	rf_file <- rf_files[i]
	class_date <- file_dates[i]

	# Process a single date file end-to-end inside each worker.
	shp <- st_read(rf_file, quiet = TRUE)

	if ("green_stde" %in% names(shp)) {
		names(shp)[names(shp) == "green_stde"] <- "green_stdev"
	}

	missing_predictors <- setdiff(predictors, names(shp))
	if (length(missing_predictors) > 0) {
		stop(sprintf(
			"Missing predictor columns in %s: %s",
			basename(rf_file),
			paste(missing_predictors, collapse = ", ")
		))
	}

	valid_blue <- !is.na(shp$blue_stdev) & shp$blue_stdev != 0

	if (!any(valid_blue)) {
		warning(sprintf("No rows with blue_stdev != 0 in %s; skipping", basename(rf_file)))
		return(NULL)
	}

	# Predict only for valid rows and keep geometry as WKT for safe row-wise joins.
	shp_valid <- shp[valid_blue, ]
	predict_input <- st_drop_geometry(shp_valid)[, predictors, drop = FALSE]
	pred <- predict(final_model, newdata = predict_input)

	data.frame(
		geom_wkt = st_as_text(st_geometry(shp_valid)),
		class_date = class_date,
		pred_class = as.character(pred),
		stringsAsFactors = FALSE
	)
}

pred_counts <- vapply(prediction_rows, function(x) if (is.null(x)) 0L else nrow(x), integer(1))
for (i in seq_along(rf_files)) {
	cat(sprintf("Predicted %d parcels for %s\n", pred_counts[i], file_dates[i]))
}

# Collapse duplicates per geometry/date and pivot into one column per date.
pred_long <- bind_rows(prediction_rows)
if (nrow(pred_long) == 0) {
	stop("No predictions were produced. Check predictor columns and blue_stdev filtering in input shapefiles.")
}

pred_long <- pred_long %>%
	group_by(geom_wkt, class_date) %>%
	summarise(pred_class = dplyr::first(pred_class), .groups = "drop")

pred_wide <- pred_long %>%
	select(geom_wkt, class_date, pred_class) %>%
	pivot_wider(
		names_from = class_date,
		values_from = pred_class,
		names_prefix = "pred_"
	)

pred_wide$parcel_index <- seq_len(nrow(pred_wide))

pred_cols <- grep("^pred_", names(pred_wide), value = TRUE)
if (length(pred_cols) == 0) {
	stop("No prediction date columns were created.")
}

pred_dates <- as.Date(sub("^pred_", "", pred_cols))

if (any(is.na(pred_dates))) {
	stop("Prediction columns must have valid dates in format pred_YYYY-MM-DD for kernel analysis.")
}

# Keep chronological ordering so rolling-window pattern checks are correct.
ord <- order(pred_dates)
pred_cols <- pred_cols[ord]
pred_dates <- pred_dates[ord]

# Reorder prediction columns in the data frame itself by date.
pred_wide <- pred_wide[, c("geom_wkt", "parcel_index", pred_cols), drop = FALSE]

date_cols <- sub("^pred_", "date_", pred_cols)
for (j in seq_along(pred_cols)) {
	pred_wide[[date_cols[j]]] <- sub("^pred_", "", pred_cols[j])
}

has_pattern_123 <- function(class_vals) {
	class_vals <- class_vals[!is.na(class_vals) & class_vals != ""]
	# Class 5 should be ignored when detecting temporal patterns.
	class_vals <- class_vals[class_vals != "5"]
	if (length(class_vals) < 3) {
		return(FALSE)
	}

	# Apply misclassification correction:
	# If class_before == class_after but current differs, fix the current to match
	corrected_vals <- class_vals
	for (i in 2:(length(class_vals)-1)) {
		if (class_vals[i-1] == class_vals[i+1] && class_vals[i] != class_vals[i-1]) {
			corrected_vals[i] <- class_vals[i-1]
		}
	}

	# Check patterns with corrected values
	if (check_pattern_exists(corrected_vals)) {
		return(TRUE)
	}

	# If no pattern found and there are 6s, try replacing them one at a time
	six_positions <- which(corrected_vals == "6")
	if (length(six_positions) > 0) {
		for (six_pos in six_positions) {
			for (replacement in c("2", "3")) {
				test_vals <- corrected_vals
				test_vals[six_pos] <- replacement
				if (check_pattern_exists(test_vals)) {
					return(TRUE)
				}
			}
		}
	}

	FALSE
}

# Helper function to check if exact patterns exist:
# 1,2,3,4; 1,2,4; 1,3,4; 6,2,3,4; 6,2,4; 6,3,4
check_pattern_exists <- function(class_vals) {
	# Collapse only consecutive repeats, preserving sequence changes.
	runs <- rle(class_vals)$values
	if (length(runs) < 3) {
		return(FALSE)
	}

	for (j in seq_len(length(runs))) {
		# Accept pattern 1,2,3,4
		if (j <= (length(runs) - 3) &&
			runs[j] == "1" &&
			runs[j + 1] == "2" &&
			runs[j + 2] == "3" &&
			runs[j + 3] == "4") {
			return(TRUE)
		}

		# Accept pattern 6,2,3,4
		if (j <= (length(runs) - 3) &&
			runs[j] == "6" &&
			runs[j + 1] == "2" &&
			runs[j + 2] == "3" &&
			runs[j + 3] == "4") {
			return(TRUE)
		}

		# Accept pattern 1,2,4
		if (j <= (length(runs) - 2) &&
			runs[j] == "1" &&
			runs[j + 1] == "2" &&
			runs[j + 2] == "4") {
			return(TRUE)
		}

		# Accept pattern 6,2,4
		if (j <= (length(runs) - 2) &&
			runs[j] == "6" &&
			runs[j + 1] == "2" &&
			runs[j + 2] == "4") {
			return(TRUE)
		}

		# Accept pattern 1,3,4
		if (j <= (length(runs) - 2) &&
			runs[j] == "1" &&
			runs[j + 1] == "3" &&
			runs[j + 2] == "4") {
			return(TRUE)
		}

		# Accept pattern 6,3,4
		if (j <= (length(runs) - 2) &&
			runs[j] == "6" &&
			runs[j + 1] == "3" &&
			runs[j + 2] == "4") {
			return(TRUE)
		}
	}

	FALSE
}

# Evaluate each parcel over rolling 28-day windows and flag detected patterns.
kernel_days <- 28
pred_wide$pattern_123_3week <- vapply(seq_len(nrow(pred_wide)), function(row_i) {
	row_vals <- as.character(pred_wide[row_i, pred_cols, drop = TRUE])

	for (start_idx in seq_along(pred_dates)) {
		in_kernel <- which(pred_dates >= pred_dates[start_idx] & pred_dates <= (pred_dates[start_idx] + kernel_days))
		if (length(in_kernel) == 0) {
			next
		}

		if (has_pattern_123(row_vals[in_kernel])) {
			return(1L)
		}
	}

	2L
}, integer(1))

# Rebuild sf geometry column from WKT after tabular reshaping.
pred_wide$geometry <- st_as_sfc(pred_wide$geom_wkt, crs = output_crs)

# Keep only one geometry representation and place columns in a stable order.
pred_wide <- pred_wide[, c("parcel_index", "pattern_123_3week", pred_cols, date_cols, "geometry"), drop = FALSE]

pred_wide_sf <- st_as_sf(pred_wide, sf_column_name = "geometry")
pattern_sf <- pred_wide_sf %>% filter(pattern_123_3week == 1)

to_shp_safe_names <- function(sf_obj) {
	old_names <- names(sf_obj)
	new_names <- old_names
	geom_col <- attr(sf_obj, "sf_column")
	if (is.null(geom_col) || length(geom_col) == 0) {
		geom_col <- "geometry"
	}

	for (i in seq_along(new_names)) {
		old_name <- old_names[i]

		if (identical(old_name, geom_col)) {
			next
		}

		if (old_name == "parcel_index") {
			new_names[i] <- "parcel_id"
		} else if (old_name == "pattern_123_3week") {
			new_names[i] <- "pat_3w_123"
		} else if (grepl("^pred_", old_name)) {
			date_str <- sub("^pred_", "", old_name)
			date_str <- gsub("-", "", date_str)
			new_names[i] <- paste0("p_", date_str)
		} else if (grepl("^date_", old_name)) {
			date_str <- sub("^date_", "", old_name)
			date_str <- gsub("-", "", date_str)
			new_names[i] <- paste0("d_", date_str)
		} else {
			new_names[i] <- gsub("[^A-Za-z0-9_]", "_", old_name)
		}

		new_names[i] <- substr(new_names[i], 1, 10)
	}

	# Ensure uniqueness within the 10-character DBF field-name limit.
	used <- character(0)
	for (i in seq_along(new_names)) {
		name_i <- new_names[i]
		if (!(name_i %in% used)) {
			used <- c(used, name_i)
			next
		}

		base <- substr(name_i, 1, 8)
		suffix <- 1
		repeat {
			candidate <- paste0(base, sprintf("%02d", suffix))
			if (!(candidate %in% used)) {
				new_names[i] <- candidate
				used <- c(used, candidate)
				break
			}
			suffix <- suffix + 1
		}
	}

	names(sf_obj) <- new_names
	sf_obj
}

pred_wide_shp <- to_shp_safe_names(pred_wide_sf)
pattern_shp <- to_shp_safe_names(pattern_sf)

# Export full classified parcels and subset where the temporal pattern is present.
classified_shp <- file.path(data_dir, "leeuwarden_classified_parcels_v2.shp")
st_write(pred_wide_shp, classified_shp, append = FALSE)

output_shp <- file.path(data_dir, "leeuwarden_percelen_pattern_spotted_v2.shp")
st_write(pattern_shp, output_shp, append = FALSE)

cat(sprintf("Saved %d classified parcels to: %s\n", nrow(pred_wide_sf), classified_shp))
cat(sprintf("Saved %d pattern polygons to: %s\n", nrow(pattern_shp), output_shp))
