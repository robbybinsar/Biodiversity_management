
# Step 3: Find mean acoustic diversity for each group and arrange from highest to lowest
group_means <- df %>%
  group_by(CROP_SETTING) %>%
  summarise(mean_acoustic_diversity = mean(LEFT_CHANNEL)) %>%
  arrange(desc(mean_acoustic_diversity))

# Display group means
print(group_means)

# Step 4: Visualization of the differences between the groups
ggplot(df, aes(x = CROP_SETTING, y = LEFT_CHANNEL, fill = CROP_SETTING)) +
  geom_boxplot() +
  labs(title = "Acoustic Diversity Index Across Different Categories",
       x = "Group",
       y = "Acoustic Diversity Index") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")
