# Load necessary libraries
library(readxl)
library(ggplot2)

# Define the file path
file_path <- "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/R/Biodiversity_management/data/crop_analysis.xlsx"

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
