# Load necessary libraries
library(vegan)
library(ggplot2)
library(gridExtra)
library(plyr)
library(pairwiseAdonis)
library(indicspecies)

# Load data (repetition included)
#load sp_site from data_cleaning.R

# Create a distance matrix
data_dist <- vegdist(sp_site, method = "bray")

# Perform NMDS
NMDS <- metaMDS(data_dist, try = 250, k = 2)

# Extract NMDS coordinates and create a data frame
NMDS_df <- merge(data.frame(NMDS$points),
                 df_transformed[,c("LANDSCAPE", "HABITAT")], 
                 by.x = "row.names",by.y = "row.names", all.x = T)
rownames(NMDS_df) <- NMDS_df$Row.names
NMDS_df <- NMDS_df[, -1]
names(NMDS_df)[3:4] <- c("site", "habitat")

# Calculate Q1, Q3, and IQR (due to extreme outlier of MDS1)
Q1 <- quantile(NMDS_df$MDS1, 0.25)
Q3 <- quantile(NMDS_df$MDS1, 0.75)
IQR <- Q3 - Q1

# Determine the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify and remove outliers
NMDS_df <- NMDS_df[NMDS_df$MDS1 >= lower_bound & NMDS_df$MDS1 <= upper_bound, ]

# Function to create convex hulls
find_hull <- function(df) df[chull(df$MDS1, df$MDS2), ]

# Calculate convex hulls for each habitat
hulls <- plyr::ddply(NMDS_df, "habitat", find_hull)

# Plot the first NMDS with convex hulls

aa <- ggplot(NMDS_df, aes(x = MDS1, y = MDS2, col = habitat)) +
  geom_polygon(data = hulls, aes(fill = habitat, group = habitat), alpha = 0.2) +
  geom_point(cex = 4, stroke = 0.5) +
  #scale_shape_manual(values = c(6, 19, 17, 10), name = "Site") +  # Add legend for shapes
  scale_color_manual(values = c("Coconut" = "#800080", "Forest" = "olivedrab", "Coconut near forest" ="#1E90FF"), name = "Habitat type") +
  scale_fill_manual(values = c("Coconut" = "#800080", "Forest" = "olivedrab", "Coconut near forest" ="#1E90FF"), name = "Habitat type") +  # Match fill colors with point colors
  theme_classic() + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = NA, color = "black") +
  theme(axis.text.x = element_text(size = 15),   
        axis.text.y = element_text(size = 15),   
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))  # Adjust legend size

print(aa)

#ADONIS 

adon2<-adonis2(sp_site ~ HABITAT, data=df_transformed)
adon2
pairwise_results <- pairwise.adonis(sp_site, df_transformed$HABITAT)
print(pairwise_results)

##indicatior species analysis
indval<-multipatt(sp_site, df_transformed$HABITAT, func="IndVal.g", duleg=F,
                  restcomb = c(1,2,3,4,6),control=how(nperm=999))

summary(indval)


#--------------------------------------------------------------------------------------------------------
  
  
# Load data (pooled)

# Create a distance matrix
data_dist <- vegdist(sp_site, method = "bray")

# Perform NMDS
NMDS <- metaMDS(data_dist, trymax =250, k = 3)

# Extract NMDS coordinates and create a data frame
NMDS_df <- merge(data.frame(NMDS$points),
                 df_transformed[,c("LANDSCAPE", "HABITAT")], 
                 by.x = "row.names",by.y = "row.names", all.x = T)
rownames(NMDS_df) <- NMDS_df$Row.names
NMDS_df <- NMDS_df[, -1]
names(NMDS_df)[4:5] <- c("site", "habitat")

# Function to create convex hulls
find_hull <- function(df) df[chull(df$MDS1, df$MDS2), ]

# Calculate convex hulls for each habitat
hulls <- plyr::ddply(NMDS_df, "habitat", find_hull)

# Plot the first NMDS with convex hulls

aa <- ggplot(NMDS_df, aes(x = MDS1, y = MDS2, col = habitat)) +
  geom_polygon(data = hulls, aes(fill = habitat, group = habitat), alpha = 0.2) +
  geom_point(cex = 4, stroke = 0.5) +
  #scale_shape_manual(values = c(6, 19, 17, 10), name = "Site") +  # Add legend for shapes
  scale_color_manual(values = c("Coconut" = "#800080", "Forest" = "olivedrab", "Coconut near forest" ="#1E90FF"), name = "Habitat type") +
  scale_fill_manual(values = c("Coconut" = "#800080", "Forest" = "olivedrab", "Coconut near forest" ="#1E90FF"), name = "Habitat type") +  # Match fill colors with point colors
  theme_classic() + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = NA, color = "black") +
  theme(axis.text.x = element_text(size = 15),   
        axis.text.y = element_text(size = 15),   
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))  # Adjust legend size

print(aa)

#ADONIS 

adon2<-adonis2(sp_site ~ HABITAT, data=df_transformed)
adon2
pairwise_results <- pairwise.adonis(sp_site, df_transformed$HABITAT)
print(pairwise_results)

##indicatior species analysis
indval<-multipatt(sp_site, df_transformed$HABITAT, func="IndVal.g", duleg=F,
                  restcomb = c(3,4),
                  control=how(nperm=999))

summary(indval)

#--------------------------------------------------------------------------------------------

#COCONUT

# Create a distance matrix
data_dist <- vegdist(sp_site, method = "bray")

# Perform NMDS
NMDS <- metaMDS(data_dist, trymax =250, k = 3)

# Extract NMDS coordinates and create a data frame
NMDS_df <- merge(data.frame(NMDS$points),
                 df_transformed[,c("LANDSCAPE", "Crop_setting")], 
                 by.x = "row.names",by.y = "row.names", all.x = T)
rownames(NMDS_df) <- NMDS_df$Row.names
NMDS_df <- NMDS_df[, -1]
names(NMDS_df)[4:5] <- c("site", "crop_setting")

# Function to create convex hulls
find_hull <- function(df) df[chull(df$MDS1, df$MDS2), ]

# Calculate convex hulls for each habitat
hulls <- plyr::ddply(NMDS_df, "crop_setting", find_hull)

# Plot the first NMDS with convex hulls

aa <- ggplot(NMDS_df, aes(x = MDS1, y = MDS2, col = crop_setting)) +
  geom_polygon(data = hulls, aes(fill = crop_setting, group = crop_setting), alpha = 0.2) +
  geom_point(cex = 4, stroke = 0.5) +
  #scale_shape_manual(values = c(6, 19, 17, 10), name = "Site") +  # Add legend for shapes
  scale_color_manual(values = c("MONO" = "#800022", "INTERCROP" = "#135222"), name = "Coconut setting") +
  scale_fill_manual(values = c("MONO" = "#800022", "INTERCROP" = "#135222"), name = "Coconut setting") +  # Match fill colors with point colors
  theme_classic() + 
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = NA, color = "black") +
  theme(axis.text.x = element_text(size = 15),   
        axis.text.y = element_text(size = 15),   
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))  # Adjust legend size

print(aa)

#ADONIS 

adon2<-adonis2(sp_site ~ Crop_setting, data=df_transformed)
adon2
pairwise_results <- pairwise.adonis(sp_site, df_transformed$Crop_setting)
print(pairwise_results)

##indicatior species analysis
indval<-multipatt(sp_site, df_transformed$Crop_setting, func="IndVal", duleg=T,
                  #restcomb = c(3,4),
                  control=how(nperm=999))

summary(indval)

