library(iNEXT)
library(iNEXT.3D)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(plyr)
library(tidyverse)

#https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.pdf

#ACROSS HABITAT

# Read in the Excel file
df <- read.xlsx("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/bird_survey_sulawesi_2024.xlsx", 
                sheet = "PC_v3")

# Remove swift (Apodidae)
df_filtered <- df %>% filter(Family != "Apodidae")

# Endemic/non-endemic dataset
#df_filtered <- df_filtered %>% filter(ENDEMIC == "NO")

#pooled by habitat clusters
df_transformed <- df_filtered %>%
  group_by(HABITAT, ENGLISH_NAME) %>%
  dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE)) %>%
  pivot_wider(names_from = HABITAT, values_from = Total_count, values_fill = 0)
# Set rownames as POINTID
df_transformed <- column_to_rownames(df_transformed, var = "ENGLISH_NAME")

df_RE <- df_transformed

#R/E and diversity analysis
#y (species richness for q = 0, Shannon diversity for q = 1, and Simpson diversity for q = 2),

out <- iNEXT(df_RE, q=c(0), datatype="abundance", #size=m
             endpoint = 2500, conf = 0.84)

g <- ggiNEXT(out, type=1, facet.var="None", color.var="Assemblage")

g6 <- g + 
  theme_classic() +
  scale_colour_manual(values=c("brown", "blue", "#228B22")) +
  scale_fill_manual(values=c("brown", "blue", "#228B22")) +
  theme(
    legend.position.inside = c(0.95, 0.05),     # Position the legend inside the plot (right down corner)
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(), # Remove legend background
    #legend.box.background = element_rect(colour = "black"), # Add a border around the legend
    legend.key.size = unit(1.25, "lines"), # Shrink the legend size
    legend.text = element_text(size = 8), # Reduce the text size in the legend
    legend.title = element_text(size = 9), # Reduce the legend title size
    axis.title.y = element_text(size = 12) # Change the y-axis label size
  ) +
  labs(y = "Species richness") # Update the y-axis label

# Print the plot
print(g6)

#-----------------------------------------------------------------------------------------------------------

#ACROSS COCONUT SETTING

# Read in the Excel file
df <- read.xlsx("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/bird_survey_sulawesi_2024_9SEP24.xlsx", 
                sheet = "PC_v3")

# Remove swift (Apodidae)
df_filtered <- df %>% filter(Family != "Apodidae")

# Endemic/non-endemic dataset
#df_filtered <- df_filtered %>% filter(ENDEMIC == "NO")

#pooled by habitat clusters
df_transformed <- df_filtered %>%
  group_by(Crop_setting, ENGLISH_NAME) %>%
  dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE)) %>%
  pivot_wider(names_from = Crop_setting, values_from = Total_count, values_fill = 0)
# Set rownames as POINTID
df_transformed <- column_to_rownames(df_transformed, var = "ENGLISH_NAME")

df_RE <- df_transformed

#R/E and diversity analysis
#y (species richness for q = 0, Shannon diversity for q = 1, and Simpson diversity for q = 2),

out <- iNEXT3D(df_RE, q=c(0,1,2), datatype="abundance",diversity = "TD",
             endpoint = 1000, conf = 0.84)

g <- ggiNEXT3D(out, type=1, facet.var="None", color.var="Assemblage")

g6 <- g + 
  theme_classic() +
  scale_colour_manual(values=c("#135222", "#7050FF","#800022")) +
  scale_fill_manual(values=c("#135222", "#7050FF","#800022")) +
  scale_shape_manual(values = c(16, 15,17 ))+
  scale_x_continuous(breaks = seq(0, 1500,by = 250))+
  theme(
    legend.position.inside = c(0.95, 0.05),     # Position the legend inside the plot (right down corner)
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(), # Remove legend background
    #legend.box.background = element_rect(colour = "black"), # Add a border around the legend
    legend.key.size = unit(1.25, "lines"), # Shrink the legend size
    legend.text = element_text(size = 8), # Reduce the text size in the legend
    legend.title = element_text(size = 9), # Reduce the legend title size
    axis.title.y = element_text(size = 12) # Change the y-axis label size
  ) +
  labs(y = "Species richness") # Update the y-axis label

# Print the plot
print(g6)

#-----------------------------------------------------------------------------------------------

#COMBINED
# Read in the Excel file
df <- read.xlsx("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/bird_survey_sulawesi_2024.xlsx", 
                sheet = "PC_v3")

# Remove swift (Apodidae)
df_filtered <- df %>% filter(Family != "Apodidae")

# Endemic/non-endemic dataset
df_filtered <- df_filtered %>% filter(ENDEMIC == "NO")


# merge column crop setting and cluster
df_filtered$CLUSTER <- ifelse(df_filtered$CLUSTER == 2, "NF", "")
df_filtered$CROP_SETTING <- ifelse(df_filtered$CLUSTER == "NF", 
                                      paste(df_filtered$Crop_setting, df_filtered$CLUSTER), 
                                      df_filtered$CROP_SETTING <- df_filtered$Crop_setting)
df_filtered <- df_filtered[, -which(names(df_filtered) %in% c("CLUSTER"))]


#pooled by combined habitat + crop setting
df_transformed <- df_filtered %>%
  group_by(CROP_SETTING, ENGLISH_NAME) %>%
  dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE)) %>%
  pivot_wider(names_from = CROP_SETTING, values_from = Total_count, values_fill = 0)
# Set rownames as POINTID
df_transformed <- column_to_rownames(df_transformed, var = "ENGLISH_NAME")

df_RE <- df_transformed

#R/E and diversity analysis
#y (species richness for q = 0, Shannon diversity for q = 1, and Simpson diversity for q = 2),

out <- iNEXT(df_RE, q=c(0,1,2), datatype="abundance", #size=m
             endpoint = 2500, conf = 0.95)

g <- ggiNEXT(out, type=1, facet.var="Order.q", color.var="Assemblage")

g6 <- g + 
  theme_classic() +
  scale_colour_manual(values=c("#135222", "#7050FF","#2000FF","#800022", "#584022")) +
  scale_fill_manual(values=c("#135222", "#7050FF","#2000FF","#800022", "#584022")) +
  theme(
    legend.position.inside = c(0.95, 0.05),     # Position the legend inside the plot (right down corner)
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(), # Remove legend background
    #legend.box.background = element_rect(colour = "black"), # Add a border around the legend
    legend.key.size = unit(1.25, "lines"), # Shrink the legend size
    legend.text = element_text(size = 8), # Reduce the text size in the legend
    legend.title = element_text(size = 9), # Reduce the legend title size
    axis.title.y = element_text(size = 12) # Change the y-axis label size
  ) +
  labs(y = "Species richness") # Update the y-axis label

# Print the plot
print(g6)

#------------------------------------------------------------------------------------------------------------------

#BY POINT

# Read in the Excel file
df <- read.xlsx("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/bird_survey_sulawesi_2024_9SEP24.xlsx", 
                sheet = "PC_v3")

# Remove swift (Apodidae)
df_filtered <- df %>% filter(Family != "Apodidae")

# Endemic/non-endemic dataset
df_filtered <- df_filtered %>% filter(ENDEMIC == "NO")

#pooled by habitat clusters
df_transformed <- df_filtered %>%
  group_by(POINTID, ENGLISH_NAME) %>%
  dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE)) %>%
  pivot_wider(names_from = POINTID, values_from = Total_count, values_fill = 0)
# Set rownames as POINTID
df_transformed <- column_to_rownames(df_transformed, var = "ENGLISH_NAME")

df_RE <- df_transformed

#R/E and diversity analysis
#y (species richness for q = 0, Shannon diversity for q = 1, and Simpson diversity for q = 2),

out <- iNEXT3D(df_RE, q=c(0,1,2), datatype="abundance",diversity = "TD",
               endpoint = 50, conf = 0.84)

g <- ggiNEXT3D(out, type=1, facet.var="None", color.var="Assemblage")

g6 <- g + 
  theme_classic() +
  scale_colour_manual(values=c("#135222", "#7050FF","#800022")) +
  scale_fill_manual(values=c("#135222", "#7050FF","#800022")) +
  scale_shape_manual(values = c(16, 15,17 ))+
  scale_x_continuous(breaks = seq(0, 1500,by = 250))+
  theme(
    legend.position.inside = c(0.95, 0.05),     # Position the legend inside the plot (right down corner)
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(), # Remove legend background
    #legend.box.background = element_rect(colour = "black"), # Add a border around the legend
    legend.key.size = unit(1.25, "lines"), # Shrink the legend size
    legend.text = element_text(size = 8), # Reduce the text size in the legend
    legend.title = element_text(size = 9), # Reduce the legend title size
    axis.title.y = element_text(size = 12) # Change the y-axis label size
  ) +
  labs(y = "Species richness") # Update the y-axis label

# Print the plot
print(g6)

