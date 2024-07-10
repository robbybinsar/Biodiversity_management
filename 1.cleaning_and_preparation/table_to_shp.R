library(sf)
# Read CSV file
data <- read.csv("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Data/SURVEY_POINTS_WINARNI2023.csv")

# Create sf object
sf_data <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

# Write to shapefile
st_write(sf_data, "points_survey_winarni2023.shp")
