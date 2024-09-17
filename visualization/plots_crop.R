# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# load data
df <- read.xlsx("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/bird_survey_sulawesi_2024.xlsx", 
                sheet = "HABITAT")


# Count how many times each crop appears in each landscape
df_count <- df %>%
  filter(!is.na(intercrops)) %>%
  separate_rows(intercrops, sep = ",\\s*") %>%
  group_by(landscape, intercrops) %>%
  tally() %>%
  rename(count = n) %>%
  ungroup()

# Step 3: Create a pie chart for each landscape with distinct colors for each crop

# Define a custom color palette for the crops
crop_colors <- c(
  "banana" = "#FFD700",      # Gold for Banana
  "cacao" = "#8B4513",       # SaddleBrown for Cacao
  "clove" = "#9D9900",       # DarkRed for Clove
  "corn" = "#FFFF00",        # Yellow for Corn
  "durian" = "#ADFF2F",      # GreenYellow for Durian
  "langsat" = "#D2691E",     # Chocolate for Langsat
  "nutmeg" = "#1C4999",      # SaddleBrown for Nutmeg
  "citrus" = "#FFA500",      # Orange for Citrus
  "guava" = "#FF69B4",       # HotPink for Guava
  "pineapple" = "#228B22",   # ForestGreen for Pineapple
  "chilli" = "#FF4500",      # OrangeRed for Chilli
  "duku" = "#AFFFFF",        # GoldenRod for Duku
  "matoa" = "#B22222",       # FireBrick for Matoa
  "peanut" = "#AFBFFF",      # Tan for Peanut
  "rambutan" = "#DC143C"     # Crimson for Rambutan
)

# Ensure that the color palette includes all crops mentioned
for (land in unique(df_count$landscape)) {
  # Filter data for the current landscape
  df_land <- df_count %>% filter(landscape == land)
  
  # Order the data by count in descending order
  df_land <- df_land %>% mutate(intercrops = factor(intercrops, levels = df_land$intercrops[order(-df_land$count)]))
  
  # Create the pie chart with ordered slices and custom colors
  pie <- ggplot(df_land, aes(x = "", y = count, fill = intercrops)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    labs(title = paste(land)) +
    theme_void() +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = crop_colors)  # Apply the custom color palette
  
  print(pie)
}


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Create the data frame
df_crops <- data.frame(
  landscape = c("BINEREAN", "LOLAYAN", "AMBANG", "TANGKOKO"),
  monocrop = c(6, 3, 0, 18),
  intercrop = c(12, 15, 18, 0)
)

# Transform the data to a long format for ggplot2
df_long <- df_crops %>%
  pivot_longer(cols = c(monocrop, intercrop), names_to = "crop_type", values_to = "count") %>%
  group_by(landscape) %>%
  mutate(percentage = count / sum(count) * 100)  # Calculate percentage for each landscape

# Create the percentage bar plot
percentage_barplot <- ggplot(df_long, aes(x = landscape, y = percentage, fill = crop_type)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#D3D3D3" ,"#505050"))+
  scale_y_continuous(labels = scales::percent_format()) +  # Show y-axis as percentages
  labs(
    #title = "Proportion of Monocrop and Intercrop in Each Landscape",
    x = NULL,
    y = NULL,
    fill = "Crop Type"
  ) +
  theme_minimal() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(percentage_barplot)

