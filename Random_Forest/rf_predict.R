# Script which takes .rds RF model and .csv labelled image metadata files (either one merged, or 4 separate ones)

# Checks whether class attribute is there

# Converts class labels from text to numbers (>1/2/3/4)
# Consider doing a enum, or just keep strings

# Other fields needed:
# - Image date, class label
# - possible inputs: Group ID

# Check with Mahtijs that RF script does not take NA values into training - then we skip the following step:

# It will run a (moving) fixed time window of 25 or 30 days
# Loop over images, check whether NA's are there or not

# Define what pattern is acceptable: 1/2/3/4, 1/3/4, 1/2/4,(2/3/4)
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