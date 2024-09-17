library(spocc)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(sf)
library(rebird)
library(rgbif)


set_token("Fafh0Cn4SPZ5rI86BeH5fwtt")
Sys.setenv(ENTREZ_KEY = "f7041649502ffe57baeb36b4be7acd695808")
Sys.setenv(IUCN_REDLIST_KEY = "5abaac6464f1172201641c99a3357982c62423e69a08920925bc430d1753812b")
Sys.setenv(EBIRD_KEY = "6bd80i259l13")

# Vector of countries in Southeast Asia
southeast_asia <- c("Brunei", "Cambodia", "East Timor", "Indonesia", "Laos", 
                    "Malaysia", "Myanmar", "Philippines", "Singapore", 
                    "Thailand", "Vietnam")

# Vector of countries in Australia
australia <- c("Australia")

# Combine both vectors into a single vector
mycountries <- c(southeast_asia, australia)

shape <- ne_countries(scale = 50, country = mycountries, returnclass = 'sf')
shape <- shape %>% dplyr::select(admin, geometry)

ggplot() + geom_sf(data = shape)


get_occurrence <- function(latin_name) {
    #retrieve species occurrence
    species <- spocc::occ(query = c(latin_name), 
                         limit = 1000)
    
    species <- occ2df(species)
    
    # Convert 'longitude' and 'latitude' columns into numbers
    species <- species %>% mutate_at(c('longitude', 'latitude'), as.numeric)
    
    # Remove lines with NA for 'longitude' or 'latitude', if any
    species <- species %>% filter_at(vars(longitude, latitude), all_vars(!is.na(.)))
    
    # Convert longitude/latitude to POINT
    species <- st_as_sf(x = species, coords = c('longitude', 'latitude'), crs = st_crs(shape))
    # Select locations that belong to South America
    species <- st_join(x = species, y = shape, left = FALSE) # if left = TRUE, return left join
    
    # Plot occurrence 
    ggplot() +
      geom_sf(data = shape) +
      geom_sf(data = species, aes(color = prov), size = 1) + 
      #scale_color_manual(name = 'Provider')
      labs(x = 'Longitude', y = 'Latitude') + 
      theme_bw()
}
