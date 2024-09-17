library(tidyverse)
library(openxlsx)
library(dplyr)
library(plyr)

# Read in the Excel file
df <- read.xlsx("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/bird_survey_sulawesi_2024.xlsx", 
                  sheet = "PC_v3")
  
# Remove swift (Apodidae)
  df_filtered <- df %>% filter(Family != "Apodidae")
  #create a df of records that being eliminated
  removed_swift <- df %>%
    filter(Family == "Apodidae") %>%
    group_by(LATIN_NAME, ENGLISH_NAME) %>%
    dplyr::summarise(Sum_Count = sum(COUNT, na.rm = TRUE))
  
# Remove species with total counts below or equal to 2
  # records of species with low counts
  df_low_counts <- df_filtered %>%
    group_by(LATIN_NAME, ENGLISH_NAME) %>%
    dplyr::summarise(Sum_Count = sum(COUNT, na.rm = TRUE)) %>%
    filter(Sum_Count <= 2)
  # remove species with low counts
  df_filtered <- df_filtered %>%
    filter(!(LATIN_NAME %in% df_low_counts$LATIN_NAME))

# Remove species in flying position
  #create a df of records that being eliminated
  removed_fly <- df_filtered %>%
    filter(STRATUM == "F") %>%
    group_by(LATIN_NAME, ENGLISH_NAME) %>%
    dplyr::summarise(Sum_Count = sum(COUNT, na.rm = TRUE))
  
  df_filtered <- df_filtered %>% filter(STRATUM != "F")
  
# Join all remove records
  removed_records <- bind_rows(removed_swift, df_low_counts, removed_fly)

  # Transform into appropriate format for analysis
  #with repetition
    # Transform the data
    df_transformed <- df_filtered %>%
      group_by(REPETITION, LANDSCAPE,HABITAT, POINTID, ENGLISH_NAME) %>%
      dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE)) %>%
      pivot_wider(names_from = ENGLISH_NAME, values_from = Total_count, values_fill = 0)
    # Set rownames as POINTID
    df_transformed$row.id <- paste(df_transformed$REPETITION, df_transformed$POINTID, sep = "_")
    df_transformed <- column_to_rownames(df_transformed, var = "row.id")
    df_final <- df_transformed[,-c(1:4)]
    
  #pooled by point ids
    df_transformed <- df_filtered %>%
      group_by(LANDSCAPE,HABITAT, POINTID, ENGLISH_NAME) %>%
      dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE)) %>%
      pivot_wider(names_from = ENGLISH_NAME, values_from = Total_count, values_fill = 0)
    # Set rownames as POINTID
    df_transformed <- column_to_rownames(df_transformed, var = "POINTID")
    df_final <- df_transformed[,-c(1:2)]
  
  #pooled by habitat clusters
    df_transformed <- df_filtered %>%
      group_by(LANDSCAPE, TRANSECT, CLUSTER, HABITAT, ENGLISH_NAME) %>%
      dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE)) %>%
      pivot_wider(names_from = ENGLISH_NAME, values_from = Total_count, values_fill = 0)
    # Set rownames as POINTID
    df_transformed$row.id <- paste(df_transformed$LANDSCAPE, df_transformed$TRANSECT,
                                   df_transformed$CLUSTER, sep = "_")
    df_transformed <- column_to_rownames(df_transformed, var = "row.id")
    df_final <- df_transformed[,-c(1:4)]
    
# square root transformation
  df_final <- sqrt(df_final)
    
# Log transformation
  df_final <- log1p(df_final)
    
  sp_site <- df_final
  
#----------------------------------------------------------------------------------------------------------------
  
#Coconut vs Coconut near forest
  
  #subset data coconut only
  df_filtered <- df_filtered %>% filter(HABITAT != "Forest")
    
  # Transform into appropriate format for analysis
  # pooled by crop setting
  df_transformed <- df_filtered %>%
    group_by(LANDSCAPE, TRANSECT, CLUSTER, Crop_setting, ENGLISH_NAME) %>%
    dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE)) %>%
    pivot_wider(names_from = ENGLISH_NAME, values_from = Total_count, values_fill = 0)
  # Set rownames as POINTID
  df_transformed$row.id <- paste(substr(df_transformed$LANDSCAPE, 1, 2), 
                                 df_transformed$TRANSECT,
                                 df_transformed$CLUSTER,
                                 substr(df_transformed$Crop_setting, 1, 2),
                                 sep = "_")
  df_transformed <- column_to_rownames(df_transformed, var = "row.id")
  df_final <- df_transformed[,-c(1:4)]
  
  # Log transformation
  df_final <- log1p(df_final)
  
  sp_site <- df_final
  
