library(bioacoustics)

# Define the function (if it's custom, otherwise ensure it's loaded)
mp3_to_wav <- function(file, output_dir = dirname(file), delete = FALSE) {
  # Assuming the function exists or is loaded.
  # Placeholder for the actual function code.
}

# Directory where your MP3 files are located
mp3_dir <- "B:/HANDHELD RECORDER/mp3"  # replace with your directory path

# Get the list of all MP3 files in the directory
mp3_files <- list.files(mp3_dir, pattern = "\\.mp3$", full.names = TRUE)

# Convert each MP3 file to WAV
for (mp3_file in mp3_files) {
  mp3_to_wav(file = mp3_file, output_dir = dirname(mp3_file), delete = FALSE)
}
