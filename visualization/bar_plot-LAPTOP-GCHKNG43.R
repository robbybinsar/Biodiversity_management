# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)

# Define the file path
file_path <- "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/R/Biodiversity_management/data/crop_analysis.xlsx"

#Figure 1 Planted area
# Read the data from the specified sheet and columns
data <- read_excel(file_path, sheet = "figure 1")

# Filter out rows where attribute equals "Total"
filtered_data <- data %>% 
  filter(`Attribute` == "Total")

# Create a bar plot
# Define breaks for y-axis
y_breaks <- seq(0, 200000000, by = 20000000)

# Create the plot
p <- ggplot(filtered_data, aes(x = reorder(`Oil seed crops`, `Max value`, decreasing = TRUE), y = `Max value`)) +
  geom_bar(stat = "identity", fill = "transparent", color = "black") +  # Empty fill with black outline
  labs(title = "Major oilseed crops planted area (ha)",
       x = "Crop Type",
       y = "Planted Area (ha)") +
  scale_y_continuous(breaks = y_breaks, labels = scales::comma(y_breaks)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

# Print the plot
print(p)

#Figure 2 average ton annualy
# Read the data from the specified sheet and columns
data <- read_excel(file_path, sheet = "figure 2")

# Filter out rows where attribute equals "Total"
filtered_data <- data %>% 
  filter(`Attribute` == "Total")

# Create a bar plot
# Define breaks for y-axis
y_breaks <- seq(0, 100000000, by = 10000000)

# Create the plot
p <- ggplot(filtered_data, aes(x = reorder(`Oil seed crops`, `Average`, decreasing = TRUE), y = `Average`)) +
  geom_bar(stat = "identity", fill = "transparent", color = "black") +  # Empty fill with black outline
  labs(title = "Average annual vegetable oil production (ton) between 2013 adn 2021",
       x = "Crop",
       y = "Production (ton)") +
  scale_y_continuous(breaks = y_breaks, labels = scales::comma(y_breaks)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

# Print the plot
print(p)

#Figure 4 

# Read the data from the specified sheet and columns
data <- read_excel(file_path, sheet = "figure 4")

# Filter out rows where attribute equals "Total"
filtered_data <- data %>% 
  filter(`Attribute` == "Total")

# Convert the data to long format for ggplot
data_long <- data %>%
  pivot_longer(cols = c(`Large-scale private/government (Ha)`, `Smallholder (Ha)`), names_to = "Ownership", values_to = "Hectare")

# Define breaks for y-axis
y_breaks <- seq(0, 18000000, by = 2000000)
# Create the plot
p <- ggplot(data_long, aes(x = reorder(Crop, Hectare, decreasing = TRUE), y = Hectare, fill = Ownership)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Large-scale private/government (Ha)" = "brown", "Smallholder (Ha)" = "orange")) +
  labs(
    title = "Estate crops commodity area (Ha) 2022",
    x = NULL,
    y = "Hectare",
    fill = "Ownership"
  ) + scale_y_continuous(breaks = y_breaks, labels = scales::comma(y_breaks), limits = c(0, 18000000)) +
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "Arial"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top",
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) 

print(p)

# Save the plot
ggsave("estate_crops_commodity_area.png", width = 10, height = 6, dpi = 300)


