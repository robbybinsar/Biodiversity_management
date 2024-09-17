# Step 1: List all text files within the folder and its subfolders
folder_path <- "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/PC RECORDER/mp3towav/false_wav"  # Replace with your folder path
file_list <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)

# Step 2: Initialize an empty list to store the data frames
df_list <- list()

# Step 3: Loop over each file, read it, and store the data frame in the list
for (file in file_list) {
  df <- read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE, fill = TRUE, quote = "", comment.char = "")
  df_list[[length(df_list) + 1]] <- df
}

# Step 4: Combine all data frames into one
combined_df <- do.call(rbind, df_list)

# Step 5: Write the combined data frame to a CSV file
write.csv(combined_df, "mp3_to_wav_flse_files.csv", row.names = FALSE)

# Optional: Check the first few rows of the combined data
head(combined_df)
