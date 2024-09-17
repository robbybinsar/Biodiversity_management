
df_combined_allspecies <- out$TDAsyEst
df_combined_endemic <- out$TDAsyEst
df_combined_nonendemic <- out$TDAsyEst

df_combined_allspecies$Type <- "All species"
df_combined_endemic$Type <- "Endemic"
df_combined_nonendemic$Type <- "Non-endemic"

combined_df_diversity <- rbind(df_combined_allspecies, df_combined_endemic, df_combined_nonendemic)

# Load ggplot2 package
library(ggplot2)

# Reorder the factor levels for Diversity
combined_df_diversity$Diversity <- factor(combined_df_diversity$qTD, levels = c("Species richness", "Shannon diversity", "Simpson diversity"))

# Create the faceted bar plot with error bars and confidence limits
ggplot(combined_df_diversity, aes(x = Assemblage, y = TD_asy, fill = Assemblage)) +
  scale_fill_manual(values=c("#135222", "#7050FF","#800022")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.1), width = 0.8, alpha = 0.7) + # Bar plot
  geom_errorbar(aes(ymin = TD_asy - s.e., ymax = TD_asy + s.e.), position = position_dodge(width = 0.7), width = 0.25, color = "black", size = 0.7) + # Error bars
  facet_grid(Type ~ Diversity, scales = "free_y") + # Faceting by Type and Diversity
  theme_minimal() +
  labs(
    #title = "Bar Plot of Estimator Values with Error Bars and Confidence Limits",
    x = "Assemblage",
    y = "Estimate",
    fill = "Assemblage"
  ) +
  theme(
    axis.text.x = element_text(size = 7),   
    axis.text.y = element_text(size = 7),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(size = 10),
    strip.text.x = element_text(size = 10),
    panel.grid.major = element_blank(), # Optional: Remove major grid lines
    panel.grid.minor = element_blank(),  # Optional: Remove minor grid lines
  )


#-----------------------------------------------------------------------------------------------------------------

#COMBINED
combined_df_diversity <- rbind(df_combined_allspecies, df_combined_endemic, df_combined_nonendemic)

# Load ggplot2 package
library(ggplot2)

# Reorder the factor levels for Diversity
combined_df_diversity$Diversity <- factor(combined_df_diversity$Diversity, levels = c("Species richness", "Shannon diversity", "Simpson diversity"))

# Create the faceted bar plot with error bars and confidence limits
ggplot(combined_df_diversity, aes(x = Assemblage, y = Estimator, fill = Assemblage)) +
  scale_fill_manual(values=c("#135222", "#7050FF","#2000FF","#800022", "#584022")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.1), width = 0.8, alpha = 0.7) + # Bar plot
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position = position_dodge(width = 0.7), width = 0.25, color = "black", size = 0.7) + # Error bars
  facet_grid(Type ~ Diversity, scales = "free_y") + # Faceting by Type and Diversity
  theme_minimal() +
  labs(
    title = "Bar Plot of Estimator Values with Error Bars and Confidence Limits",
    x = "Assemblage",
    y = "Estimator",
    fill = "Assemblage"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(size = 10),
    strip.text.x = element_text(size = 10),
    panel.grid.major = element_blank(), # Optional: Remove major grid lines
    panel.grid.minor = element_blank()  # Optional: Remove minor grid lines
  )
