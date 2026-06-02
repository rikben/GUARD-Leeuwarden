library(sf)
library(dplyr)
library(tidyr)
library(caret)
library(ranger)
library(doParallel)
library(foreach)

# Load the trained random-forest model that will be applied to each dated shapefile.
final_model <- readRDS("C:/Users/NL1G7U/Documents/stage_proj/Data/rf_modellen/RF_model_glyph_v2.rds")

# Read the reference parcel layer and keep only records eligible for prediction.
data <- read_sf("C:/Users/NL1G7U/Documents/stage_proj/Data/bruikbare_datasets/dataset_v2.shp")
data <- data %>% filter(!is.na(glyf)) %>% filter(glyf != 0) %>% filter(class != 7) %>% filter(!is.na(blue_stdev))
names(data)[names(data) == "green_stde"] <- "green_stdev"
cat(sprintf("Data loaded: %d samples, %d features\n", nrow(data), ncol(data)))

# Attach a stable key so predictions can be merged back after per-date processing.
data$parcel_index <- seq_len(nrow(data))

predictors <- c("ndvi", "ndvi_stdev", "blue_mean", "green_mean", "red_mean", "nir_mean", "blue_stdev", "green_stdev", "red_stdev", "nir_stdev")
dep_var <- "class"

data_dir <- "C:/Users/NL1G7U/Documents/stage_proj/Data/bruikbare_datasets"

# Discover all time-stamped prediction inputs in the target folder.
rf_files <- list.files(
	path = data_dir,
	pattern = "^RF_traindata_.*\\.shp$",
	full.names = TRUE
)

if (length(rf_files) == 0) {
	stop(sprintf("No RF_traindata_<date>.shp files found in: %s", data_dir))
}

extract_file_date <- function(path) {
	fname <- basename(path)
	token <- sub("^RF_traindata_(.*)\\.shp$", "\\1", fname)

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

cat(sprintf("Parcels in data: %d\n", nrow(data)))
cat(sprintf("RF shapefiles found: %d\n", length(rf_files)))

# Collect one data.frame per date and combine them later.
prediction_rows <- vector("list", length(rf_files))

for (i in seq_along(rf_files)) {
	rf_file <- rf_files[i]
	class_date <- file_dates[i]

	# Read one date layer and align CRS with the reference parcel geometry.
	shp <- st_read(rf_file, quiet = TRUE)

	if (st_crs(shp) != st_crs(data)) {
		shp <- st_transform(shp, st_crs(data))
	}

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

	# Match rows spatially so each reference parcel gets the corresponding date-specific predictors.
	geom_match <- st_equals(data, shp)
	if (any(lengths(geom_match) == 0)) {
		geom_match <- st_intersects(data, shp)
	}
	shp_idx <- vapply(geom_match, function(x) if (length(x) > 0) x[1] else NA_integer_, integer(1))
	matched <- !is.na(shp_idx)

	if (!all(matched)) {
		warning(sprintf("%d parcels in data could not be spatially matched in %s", sum(!matched), basename(rf_file)))
	}

	if (!any(matched)) {
		warning(sprintf("No spatial matches found for %s; skipping", basename(rf_file)))
		next
	}

	# Predict class labels for matched parcels only.
	predict_input <- st_drop_geometry(shp[shp_idx[matched], ])[, predictors, drop = FALSE]
	
	
	
	pred <- predict(final_model, newdata = predict_input)

	prediction_rows[[i]] <- data.frame(
		parcel_index = data$parcel_index[matched],
		class_date = class_date,
		pred_class = as.character(pred),
		stringsAsFactors = FALSE
	)

	cat(sprintf("Predicted %d parcels for %s\n", sum(matched), class_date))
}

# Convert long predictions (one row per parcel-date) to wide time-series columns.
pred_long <- bind_rows(prediction_rows)

pred_wide <- pred_long %>%
	select(parcel_index, class_date, pred_class) %>%
	pivot_wider(
		names_from = class_date,
		values_from = pred_class,
		names_prefix = "pred_"
	)

pred_wide <- data.frame(parcel_index = data$parcel_index, glyf = data$glyf, geometry = data$geometry) %>%
	left_join(pred_wide, by = "parcel_index")

pred_cols <- grep("^pred_", names(pred_wide), value = TRUE)
pred_dates <- as.Date(sub("^pred_", "", pred_cols))

if (any(is.na(pred_dates))) {
	stop("Prediction columns must have valid dates in format pred_YYYY-MM-DD for kernel analysis.")
}

# Ensure temporal columns are processed in chronological order.
ord <- order(pred_dates)
pred_cols <- pred_cols[ord]
pred_dates <- pred_dates[ord]

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

# Slide a 28-day kernel over each parcel timeline and flag parcels where a valid pattern appears.
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

# Add explicit date columns so exported files preserve the date labels as attributes.
date_cols <- pred_cols
for (col_name in date_cols) {
	date_value <- sub("^pred_", "", col_name)
	pred_wide[[sub("^pred_", "date_", col_name)]] <- date_value
}

pred_wide$glyf_in_per <- 2
pred_wide <- pred_wide[, c("parcel_index", "glyf_in_per", "glyf", "pattern_123_3week", pred_cols, grep("^date_", names(pred_wide), value = TRUE), "geometry")]

cat(sprintf("\nFinal table shape: %d parcels x %d columns\n", nrow(pred_wide), ncol(pred_wide)))

write.csv(pred_wide, file.path(data_dir, "rf_parcel_predictions_over_time_V3.csv"), row.names = FALSE)
cat("Saved predictions to: rf_parcel_predictions_over_time_V3.csv\n")

pred_wide_sf <- st_as_sf(pred_wide, sf_column_name = "geometry")

# Shorten column names to meet shapefile DBF 10-character limit
new_names <- names(pred_wide_sf)

for (i in seq_along(new_names)) {
	old_name <- new_names[i]
	
	if (nchar(old_name) <= 10) {
		next
	}
	
	# Apply specific shortenings
	if (old_name == "parcel_index") {
		new_names[i] <- "parcel_id"
	} else if (old_name == "glyf_in_per") {
		new_names[i] <- "glyf_pct"
	} else if (old_name == "pattern_123_3week") {
		new_names[i] <- "pat_3w_123"
	} else if (grepl("^pred_", old_name)) {
		# Convert pred_YYYY-MM-DD to p_YYYYMMDD
		date_str <- sub("^pred_", "", old_name)
		date_str <- gsub("-", "", date_str)
		new_names[i] <- paste0("p_", date_str)
	} else if (grepl("^date_", old_name)) {
		# Convert date_YYYY-MM-DD to d_YYYYMMDD
		date_str <- sub("^date_", "", old_name)
		date_str <- gsub("-", "", date_str)
		new_names[i] <- paste0("d_", date_str)
	} else {
		# Generic truncation to 10 chars
		new_names[i] <- substr(old_name, 1, 10)
	}
}

names(pred_wide_sf) <- new_names

# Write final spatial output with DBF-safe field names.
st_write(pred_wide_sf, "glyf_pred_v3.shp", append=FALSE)
