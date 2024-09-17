library(soundecology)
library(seewave)

#Acoustic Complexity Index (ACI)

  #Load file as an object called soundfile
  soundfile <- readWave("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Audiomoth/Pilot testing/Binerean/Audio 10 Binerean-Research Station/20240308/20240308_152000.WAV")
  
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
  print(result$aei_left)
  
  summary(result)
  
#Analysis of many files
  multiple_sounds(directory = "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Audiomoth/Pilot testing/Binerean/Audio 4 Binerean-Agroforestry/20240309", 
                              ,resultfile = "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Audiomoth/Pilot testing/Binerean/Audio 4 Binerean-Agroforestry/20240309/NDSI_20240309_agr.csv",
                              soundindex = "ndsi", anthro_min = 500, anthro_max = 1000,
                  bio_min = 1000, bio_max = 12000)

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

  