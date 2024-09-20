library(soundecology)
library(seewave)
library(tuneR)
library(stringr)

#Acoustic Complexity Index (ACI)

  #Load file as an object called soundfile
  soundfile <- readWave("/home/rb857/AUDIO/BINEREAN/BIT01/C1/BIT01C102/20240516/20240516_054521.WAV")
  
  #Delete the downloaded wave file
  #unlink("SM87_20080420_000000_10.wav")
  
  #Run the function on this object and save the results in a new variable called "soundfile.aci"
  soundfile.aci <- acoustic_complexity(soundfile, max_freq = 12000, j= 10)
  
  #Print the ACI value for the left channel of the wav file, stored in soundfile.aci
  print(soundfile.aci$AciTotAll_left)
  
#Normalized Difference Soundscape Index (NDSI)
  soundfile.ndsi <- ndsi(soundfile)
  print(soundfile.ndsi$ndsi_left)
  
  summary(soundfile.ndsi)

#Bioacoustic Index
  bioindex <- bioacoustic_index(soundfile, min_freq = 400, max_freq = 12000)
  print(bioindex$left_area)
  
  summary(bioindex)

#Acoustic Diversity Index (ADI)
  soundfile.ADI <- acoustic_diversity(soundfile, max_freq = 12000)
  print(soundfile.ADI$adi_left)
  
  summary(result)
  
#Acoustic Evenness Index (AEI)
  soundfile.aei <- acoustic_evenness(soundfile, max_freq = 12000)
  print(soundfile.aei$aei_left)
  
  summary(soundfile.aei)
  
  
#check faulty wav files
  # Directory containing the .wav files
  directory <- "/home/rb857/AUDIO/LOLAYAN"
  
  # List all .wav files
  wav_files <- list.files(path = directory, pattern = "*.WAV$", full.names = TRUE, recursive = T)
  
  # Get file sizes
  file_info <- file.info(wav_files)
  file_sizes <- file_info$size
  
  # Define the size limit in bytes
  size_limit <- 100 * 1024  # 100 KB in bytes
  
  # Find files smaller than the size limit
  small_files <- wav_files[file_sizes < size_limit]
  
  # Print the small files
  print(small_files)
  
#Analysis of many files
  
acoustic_indices <- function(index, folder_main) {
  count_levels <- function(path) {
    # Count the number of slashes in the path
    length(str_split(path, "/", simplify = TRUE)) - 1
  }

folder_sub <- list.dirs(folder_main, recursive = T)

# Filter the directories
filtered_folders <- folder_sub[sapply(folder_sub, function(x) count_levels(x) == 7)] #CAREFUL WITH LEVELS, CHECK DIRECTORIES TO REFER TO A CORRECT PATH

for (x in filtered_folders) { 
  parts2 <- strsplit(x, "/")[[1]]
  pointid <- trimws(parts2[length(parts2)])
  
  folders <- list.dirs(x, recursive = F)
  
  for (i in folders) {
  parts <- strsplit(i, "/")[[1]]
  last_part <- trimws(parts[length(parts)])
    multiple_sounds(directory = i,
                resultfile = paste0(i,"/",pointid,"_",last_part,"_",index ,".csv"),
                              soundindex = index, max_freq = 12000, no_cores = "max")
  }
  
  
  # List all CSV files in the output folder
  csv_files <- list.files(path = x, pattern = "*.csv", full.names = TRUE, recursive = T)
  csv_files <- grep(paste0(index, ".csv$"), csv_files, value = T)
  
  # Read and combine all CSV files into one dataframe
  combined_data <- do.call(rbind, lapply(csv_files, read.csv))
  
  # Write the combined data to a new CSV file
  write.csv(combined_data, file = paste0(x,"/",pointid, "_", index,".csv"), row.names = FALSE)
}
}
  
  rm(list = ls())
  
  #Analysis of many files (H entropy)
  
  # Step 1: Get a list of all files in the folder
  files <- list.files(path = "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Audiomoth/Pilot testing/Binerean/Audio 4 Binerean-Agroforestry/20240308", 
                      full.names = TRUE)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Step 2: Apply the function 'f' to each file and store the results
  for (file in files) {
    fileob <- readWave(file)
    result <- H(fileob, f=12000)
    results_list[[file]] <- result
  }
  
  # Step 3: Compile the results into a dataframe
  df <- do.call(rbind, results_list)
  
  # Step 4: Export the dataframe as a CSV file
  write.csv(df, file = "output.csv", row.names = FALSE)
  
  
#Sound raster
soundfl <- readWave("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Audiomoth/Pilot testing/Binerean/Audio 10 Binerean-Research Station/20240308/20240308_152000.WAV")
snd_asc <- sound_raster(wavfile = "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Audiomoth/Pilot testing/Binerean/Audio 10 Binerean-Research Station/20240308/20240308_152000.WAV")

  