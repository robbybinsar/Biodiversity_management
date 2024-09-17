######################################################################
#
#                     Extract covariates around points 
#
######################################################################

# Clean workspace
rm(list=ls())

# Load packages
library(tidyverse)
library(terra)
library(sf)


# Set projection to target CRS (WGS 84 / UTM zone 49S)
target_crs <- "EPSG:32749"


# Read in points
Rpoints <- vect("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Data/OUTPUT/point_survey_UPDATED_2.shp")
Rpoints_repro <- project(Rpoints, target_crs)

################################################################################
#
##   Read in and process covariate
#
###  Update with each covariate

#covar <- rast("X:/Landcover/Indonesia/TMF_JRC_forest/2023_version/AnnualChange/JRC_TMF_AnnualChange_v1_2023_ASI_ID63_N10_E120.tif")
covar <- rast("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Data/OUTPUT/distance_to_forest_5.tif")
ras_crs <- crs(covar)
# Create a bounding box around the points
bbox <- st_bbox(project(Rpoints_repro, ras_crs))

# Convert bounding box to an sf polygon
bbox_polygon <- st_as_sfc(bbox)

# Buffer around the bounding box (extent)
buffer_distance <- 6000  # 6 km in meters
buffer_polygon <- st_buffer(bbox_polygon, dist = buffer_distance)

# Convert the sf buffer polygon to a SpatVector
buffer_spatvector <- vect(buffer_polygon)

# Clip covariate to bounding box 
covar_clipped <- crop(covar, buffer_spatvector)


# Reproject raster to UTM zone 49S
covar_reprojected <- project(covar_clipped, target_crs)

# plot to make sure it has worked so far 
plot(covar_reprojected)

# Extract values
values_forest <- as.integer(values(covar_reprojected))

# Apply reclassification
values_forest[values_forest == 1] <- 1
values_forest[values_forest == 2] <- 1
values_forest[values_forest %in% c(3, 4, 6)] <- 0
values_forest[values_forest == 5] <- NA  # Setting 5 to NoData (NA)

# Create a new raster with the reclassified values
covar_reclassified <- covar_reprojected
values(covar_reclassified) <- values_forest

# Plot to make sure it all looks ok
plot(covar_reclassified)


################################################################################
#
##  Define buffer sizes and extract covarites

# Define buffer sizes
buffer_sizes <- c(50, 100, 250, 500)

# Initialize an empty data frame to store results
all_values <- data.frame(ID = 1:nrow(Rpoints_repro))  # Assuming ID column to match points
all_values$ID <- Rpoints_repro$PointID
# Loop over each buffer size
for (size in buffer_sizes) {
  
  # Create buffer
  buffer <- buffer(Rpoints_repro, width = size)
  
  # Plot buffer (optional visualization step)
  plot(buffer, add = TRUE)
  
  # Extract raster values within the buffer
  values <- extract(covar_reprojected, buffer, fun = mean, na.rm = TRUE)
  
  # Add buffer size information to the extracted values as a new column
  ## Update with the name of the covariate
  column_name <- paste("Distance_to_forest_", size, "m", sep = "")
  all_values[[column_name]] <- values[, 2]  # Assuming values[, 2] contains the extracted raster values
}


# Save the extracted values to a CSV file
write.csv(all_values, " distance_to_cont_forest.csv", row.names = FALSE)
