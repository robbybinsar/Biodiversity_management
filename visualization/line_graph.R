# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

# Define the file path
file_path <- "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/R/Biodiversity_management/data/crop_analysis.xlsx"

# Read the data from the specified sheet and columns
data <- read_excel(file_path, sheet = "figure 3")

# Filter out rows where attribute equals "Total"
filtered_data <- data %>% 
  filter(`Attribute` == "Total")

# Convert the data frame to long format
filtered_data <- filtered_data %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Productivity")

# Create a line graph
# Define breaks for y-axis
y_breaks <- seq(0, 3, by = 0.5)

# Plot the data using ggplot2
p <- ggplot(filtered_data, aes(x = `Year`, y = `Productivity`, color = `Oil seed crops`, group = `Oil seed crops`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Major oil seed crops productivity (ton/ha)",
    x = "Year",
    y = "Productivity (ton/ha)",
    color = "Crop"
  ) +
  scale_y_continuous(breaks = y_breaks, labels = scales::comma(y_breaks)) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold")
  ) +
  scale_color_brewer(palette = "Set2")

print(p)
