library(ggplot2)
library(dplyr)

df_ADI <- read.csv("/home/rb857/Biodiversity_management/acoustic_diversity_pointid.csv")
df_BIO <- read.csv("/home/rb857/Biodiversity_management/bioacoustic_index_pointid.csv")
df_ACI <- read.csv("/home/rb857/Biodiversity_management/acoustic_complexity_pointid.csv")
#iNDIVIDUAL
    # Create the boxplot
    ggplot(df_ACI, aes(x = CROP_SETTING, y = index)) +
      geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
      labs(title = "Boxplot of Values by Group",
           x = "Group",
           y = "Value") +
      theme_minimal(base_size = 15) +  # Use a minimal theme
      theme(axis.text.x = element_text(size = 12),  # Adjust x-axis text size
            axis.text.y = element_text(size = 12),  # Adjust y-axis text size
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))  # Center and format title


#COMBINED
    # Add a new column to each dataframe to indicate the index type
    df_ADI$index_type <- "ADI"
    df_BIO$index_type <- "BIO"
    df_ACI$index_type <- "ACI"
    
    # Combine the dataframes into one
    df_combined <- bind_rows(df_ADI, df_BIO, df_ACI)
    
    # Create a named vector for custom y-axis labels
    y_labels <- c(ADI = "Acoustic Diversity Index (ADI)", 
                  BIO = "Bioacoustic Index (BIO)", 
                  ACI = "Acoustic Complexity Index (ACI)")
    
    # Create the box plot with facets, independent y-axes, and custom y-axis labels
    ggplot(df_combined, aes(x = CROP_SETTING, y = index, fill = CROP_SETTING)) +
      geom_boxplot(outlier.color = "black", outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = c("red", "green", "blue")) +  # Custom colours for CROP_SETTING
      labs(title = "Boxplots of Acoustic Diversity Indices",
           x = "Crop Setting",
           y = NULL) +  # Remove global y-axis label
      theme_minimal(base_size = 15) +  # Minimal theme with adjusted base size
      theme(axis.text.x = element_text(size = 12),  # Adjust x-axis text size
            axis.text.y = element_text(size = 12),  # Adjust y-axis text size
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and format title
            legend.position = "bottom") +  # Position the legend at the bottom
      facet_wrap(~ index_type, scales = "free_y", nrow = 1, 
                 labeller = labeller(index_type = y_labels))  # Use free y-scales and custom y-labels
