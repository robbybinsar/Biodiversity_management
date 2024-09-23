# Rename ADI to acoustic diversity
    # Example list of file names in your working directory
    file_names <- list.files("/home/rb857/AUDIO/acoustic_indices", pattern = "ADI", full.names = T)  # Lists all files containing "ADI"
    
    # Loop through the file names and rename them
    new_file_names <- gsub("ADI", "acoustic_diversity", file_names)
    file.rename(file_names, new_file_names)
    
    # Check the renamed files
    list.files(pattern = "acoustic_diversity")


#Path to folder
    folder_path <- "/home/rb857/AUDIO/acoustic_indices"
    # List all CSV files in the folder with the pattern *_INDEXNAME.csv
    file_list <- list.files(path = folder_path, pattern = "*T0[1-3]C[1-3]0[1-3]+_acoustic_diversity\\.csv", full.names = T)
    # Create an empty list to store the data frames
    df_list <- list()
    # Loop through each file, read it, and extract POINTID
    for (file in file_list) {
      file_name <- basename(file)
      # Use regular expression to extract POINTID from the file name (everything before the first "_")
      pointid <- sub("_.*", "", file_name)
      # Read the CSV file
      df <- read.csv(file)
      # Add POINTID as a new column
      df$POINTID <- pointid
      # Add the data frame to the list
      df_list[[length(df_list) + 1]] <- df
    }
    
# Combine all data frames into one
merged_data <- do.call(rbind, df_list)
    

# Add axillary variables
library(openxlsx)

habitat <- read.xlsx("/home/rb857/AUDIO/bird_survey_sulawesi_2024_23SEP24.xlsx", sheet = "HABITAT")
df_full <- merge(merged_data, habitat[, c("pointid","CROP_SETTING")], by.x = "POINTID", by.y = "pointid", all.x = T)
    
    
    
    
    
    
    
    #extract pointid
    parts2 <- strsplit(file_list, "_")
    pointid <- trimws(parts2)
    
    first_chars <- sapply(parts2, function(x) x[[1]][1])
    