library(geometry)
library(plotly)

# FOREST
# Create a 3D plot using plotly
plot_3d <- plot_ly(NMDS_df, x = ~MDS1, y = ~MDS2, z = ~MDS3, 
                   color = ~habitat, colors = c("Coconut" = "#800080", 
                                                "Forest" = "olivedrab", 
                                                "Coconut near forest" = "#1E90FF"),
                   #symbol = ~as.factor(site), symbols = c(6, 19, 17, 10), 
                   marker = list(size = 5)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'MDS1'),
                      yaxis = list(title = 'MDS2'),
                      zaxis = list(title = 'MDS3')),
         legend = list(title = list(text = 'Habitat type')),
         title = '3D NMDS Plot')

# Show plot
plot_3d

#-------------------------------------------------------------------------
# FOREST
# Function to calculate convex hull for 3D points
compute_convex_hull <- function(df) {
  ch <- convhulln(as.matrix(df[, c("MDS1", "MDS2", "MDS3")]))
  df[ch, ]
}

# Calculate convex hulls for each habitat
hull_Coconut <- compute_convex_hull(NMDS_df[NMDS_df$habitat == "Coconut", ])
hull_Coconut_near_forest <- compute_convex_hull(NMDS_df[NMDS_df$habitat == "Coconut near forest", ])
hull_Forest <- compute_convex_hull(NMDS_df[NMDS_df$habitat == "Forest", ])

# 3D Plot with convex hulls
plot_3d <- plot_ly(NMDS_df, x = ~MDS1, y = ~MDS2, z = ~MDS3, 
                   color = ~habitat, colors = c("Coconut" = "#800080", 
                                                "Forest" = "olivedrab", 
                                                "Coconut near forest" = "#1E90FF"),
                   #symbol = ~as.factor(site), symbols = c(6, 19, 17, 10), 
                   marker = list(size = 5)) %>%
  add_markers() %>%
  add_trace(data = hull_Coconut, x = ~MDS1, y = ~MDS2, z = ~MDS3,
            type = "mesh3d", opacity = 0.2, alphahull = 0, colors = "#800080", name = "Coconut") %>%
  add_trace(data = hull_Coconut_near_forest, x = ~MDS1, y = ~MDS2, z = ~MDS3,
            type = "mesh3d", opacity = 0.2, alphahull = 0, colors = "#1E90FF", name = "Coconut near forest") %>%
  add_trace(data = hull_Forest, x = ~MDS1, y = ~MDS2, z = ~MDS3,
            type = "mesh3d", opacity = 0.2, alphahull = 0, colors = "olivedrab", name = "Forest") %>%
  layout(scene = list(xaxis = list(title = 'MDS1'),
                      yaxis = list(title = 'MDS2'),
                      zaxis = list(title = 'MDS3'),
                      legend = list(title = list(text = 'Habitat type'))))
# Show plot
plot_3d
#------------------------------------------------------------------------------

#COCONUT
# Create a 3D plot using plotly
plot_3d <- plot_ly(NMDS_df, x = ~MDS1, y = ~MDS2, z = ~MDS3, 
                   color = ~crop_setting, colors = c("MONO" = "#800022", "INTERCROP" = "#135222"),
                   #symbol = ~as.factor(site), symbols = c(6, 19, 17, 10), 
                   marker = list(size = 5)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'MDS1'),
                      yaxis = list(title = 'MDS2'),
                      zaxis = list(title = 'MDS3')),
         legend = list(title = list(text = 'Crop setting')),
         title = '3D NMDS Plot')

# Show plot
plot_3d

#-----------------------------------------------------------------
# COCONUT
# Function to calculate convex hull for 3D points
compute_convex_hull <- function(df) {
  ch <- convhulln(as.matrix(df[, c("MDS1", "MDS2", "MDS3")]))
  df[ch, ]
}

# Calculate convex hulls for each habitat
hull_MONO <- compute_convex_hull(NMDS_df[NMDS_df$crop_setting == "MONO", ])
hull_INTERCROP <- compute_convex_hull(NMDS_df[NMDS_df$crop_setting == "INTERCROP", ])

# 3D Plot with convex hulls
plot_3d <- plot_ly(NMDS_df, x = ~MDS1, y = ~MDS2, z = ~MDS3, 
                   color = ~crop_setting, colors = c("MONO" = "#800022", 
                                                "INTERCROP" = "#135222"),
                   #symbol = ~as.factor(site), symbols = c(6, 19, 17, 10), 
                   marker = list(size = 5)) %>%
  add_markers() %>%
  add_trace(data = hull_MONO, x = ~MDS1, y = ~MDS2, z = ~MDS3,
            type = "mesh3d", opacity = 0.2, alphahull = 0, colors = "#800022", name = "Monocrop") %>%
  add_trace(data = hull_INTERCROP, x = ~MDS1, y = ~MDS2, z = ~MDS3,
            type = "mesh3d", opacity = 0.2, alphahull = 0, colors = "#135222", name = "Intercrop") %>%
  layout(scene = list(xaxis = list(title = 'MDS1'),
                      yaxis = list(title = 'MDS2'),
                      zaxis = list(title = 'MDS3'),
                      legend = list(title = list(text = 'Crop setting'))))
# Show plot
plot_3d


#-----------------------------------------------------------------------------



# Example using rgl
library(rgl)
plot3d(NMDS_df$MDS1, NMDS_df$MDS2, NMDS_df$MDS3, col = as.numeric(NMDS_df$habitat), size = 5)

# Add ellipsoids or spheres manually (you would need to define the center and radius based on your data)
# For example:
# spheres3d(center_x, center_y, center_z, radius, color = habitat_color)

library(dbscan)

dbscan_result <- dbscan(NMDS_df[, c("MDS1", "MDS2", "MDS3")], eps = 0.5, minPts = 5)
NMDS_df$DBSCAN_Cluster <- as.factor(dbscan_result$cluster)

plot_3d <- plot_ly(NMDS_df, x = ~MDS1, y = ~MDS2, z = ~MDS3, 
                   color = ~DBSCAN_Cluster, colors = c("#1f77b4", "#ff7f0e", "#2ca02c"), 
                   marker = list(size = 5)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'MDS1'),
                      yaxis = list(title = 'MDS2'),
                      zaxis = list(title = 'MDS3')),
         legend = list(title = list(text = 'DBSCAN Cluster')),
         title = '3D NMDS Plot with DBSCAN Clusters')

plot_3d

