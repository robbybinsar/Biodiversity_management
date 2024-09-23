library(ggplot2)

# Create the boxplot
ggplot(df, aes(x = CROP_SETTING, y = LEFT_CHANNEL)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of Values by Group",
       x = "Group",
       y = "Value") +
  theme_minimal(base_size = 15) +  # Use a minimal theme
  theme(axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))  # Center and format title
