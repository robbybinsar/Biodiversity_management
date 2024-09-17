#ACROSS COCONUT SETTING (cluters)

# Read in the Excel file
df <- read.xlsx("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/bird_survey_sulawesi_2024_9SEP24.xlsx", 
                sheet = "PC_v3")

# Remove swift (Apodidae)
df_filtered <- df %>% filter(Family != "Apodidae")

# Endemic/non-endemic dataset
df_filtered <- df_filtered %>% filter(ENDEMIC == "NO")

#pooled by habitat clusters
df_transformed <- df_filtered %>%
  group_by(LANDSCAPE, TRANSECT, CLUSTER, ENGLISH_NAME) %>%
  dplyr::summarise(Total_count = sum(COUNT, na.rm = TRUE))

df_transformed$clustersep <- paste0(substr(df_transformed$LANDSCAPE, 1, 2), "T0",
                                df_transformed$TRANSECT,"C",
                                df_transformed$CLUSTER
                                )
df_transformed <- df_transformed[,-c(1,2,3)]
  
df_transformed <- df_transformed %>% pivot_wider(names_from = clustersep, values_from = Total_count, values_fill = 0)
# Set rownames as POINTID
df_transformed <- column_to_rownames(df_transformed, var = "ENGLISH_NAME")

df_RE <- df_transformed

#R/E and diversity analysis
#y (species richness for q = 0, Shannon diversity for q = 1, and Simpson diversity for q = 2),

out <- iNEXT3D(df_RE, q=c(0,1,2), datatype="abundance",diversity = "TD",
               endpoint = 50, conf = 0.84)

df_combined_allspecies <- out$TDAsyEst
df_combined_endemic <- out$TDAsyEst
df_combined_nonendemic <- out$TDAsyEst


#1 Package: ggplot2, car,FSA, agricolae, dan Rmisc
library(ggplot2)
library(agricolae)
library(car)
library(FSA)
library(Rmisc)
library(nortest)
library(openxlsx)
library(dplyr)

# test data normality and homogeneity
df <- read.xlsx("C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/R/Biodiversity_management/stat_result/diversity_point_covariates.xlsx")
df <- df %>% filter(qTD == "Species richness") #%>% filter(coconut_density != 0) %>% filter(Assemblage != c("AMT01C103", "AMT02C101"))
#dependent.var <- df[,"Distance_to_forest_100m"]
#independent.var <- df[,"TD_asy"]
#kol1 <- colnames(df)[1]
#kol2 <- colnames(df)[2]

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(df$TD_asy_endemic)
print(shapiro_test)

# Histogram and Q-Q plot for visual inspection
hist(df$TD_asy, main = "Histogram of Species Richness", xlab = "Species Richness", col = "lightblue")
qqnorm(df$TD_asy)
qqline(df$TD_asy, col = "red")

#Homogeneity test for continous explanatory variable using Breusch-pagan

library(lmtest)

# Fit a linear model
model <- lm(TD_asy ~ coconut_density, data = df)

# Perform the Breusch-Pagan test
bp_test <- bptest(model)
print(bp_test)

# Plot residuals vs fitted values
plot(model$fitted.values, resid(model),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values Plot")
abline(h = 0, col = "red")

#If the residuals are randomly scattered around the horizontal line (at 0) without forming any specific pattern, it suggests homoscedasticity (constant variance).

#-------------------------------------------------------------------------------------------

#PARAMETRIC TEST (if normally distributed and homogenous)

#linear regresssion

lm_model <- lm(TD_asy ~ coconut_density, data = df)

# Display the summary of the model
summary(lm_model)


#-------------------------------------------------------------------------------------------

#NON-PARAMETRIC TEST

#Non-Parametric Approach Using Spearman's Rank Correlation
# Perform Spearman's rank correlation test
spearman_test <- cor.test(df$TD_asy, df$coconut_density, method = "spearman")

# Print the test result
print(spearman_test)

#nterpretation:
#The output will give you the Spearman's correlation coefficient (rho) and a p-value.
#If the p-value is less than 0.05, there is a significant monotonic relationship between distance_to_forest and species_richness.
#The sign of rho (positive or negative) will indicate the direction of the relationship.



#GLM
# Fit a Gamma GLM
glm_gamma <- glm(TD_asy_endemic ~ Distance_to_forest_100m, family = Gamma(link = "log"), data = df)

# Open a connection to the text file
sink("GLM_simpson_coconutdensity.txt")
# Summarize the model
summary(glm_gamma)
# Close the connection to the text file
sink()

library(ggplot2)
library(broom)
# Generate predicted values and standard errors for confidence intervals
pred_data <- data.frame(
  Distance_to_forest_100m = df$Distance_to_forest_100m,
  TD_asy_endemic = df$TD_asy_endemic
)

# Get predictions and standard errors
pred_data <- cbind(pred_data, predict(glm_gamma, newdata = pred_data, type = "response", se.fit = TRUE))

# Calculate lower and upper confidence intervals
pred_data$lower_ci <- pred_data$fit - 1.96 * pred_data$se.fit
pred_data$upper_ci <- pred_data$fit + 1.96 * pred_data$se.fit

# Create the plot
plot <- ggplot(df, aes(x = Distance_to_forest_100m, y = TD_asy_endemic)) +
  #geom_point(size = 2, alpha = 0.6) +  # Scatter plot of the actual data
  geom_line(data = pred_data, aes(x = Distance_to_forest_100m, y = fit), color = "purple", size = 1) +  # Fitted line from the GLM
  geom_ribbon(data = pred_data, aes(x = Distance_to_forest_100m, ymin = lower_ci, ymax = upper_ci), 
              alpha = 0.4, fill = "#D8BFD8") +  # Confidence interval band
  labs(#title = "Effect of Distance to Forest on Estimated Species Richness",
       x = "Coconut density",
       y = "Asymptotic Simpson Diversity",
       #caption = "Fitted line and 95% Confidence Interval from Gamma GLM"
  )+
  theme_minimal(base_size = 15) +  # Minimal theme for clean look
  theme(plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
        axis.title.x = element_text(size = 11),  # X-axis title style
        axis.title.y = element_text(size = 11),  # Y-axis title style
        axis.text = element_text(size = 10),  # Axis text size
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_blank())

# Print the plot
print(plot)





#Generalized Additive Model (GAM)

library(mgcv)

# Fit a GAM model
gam_model <- gam(TD_asy_endemic ~ s(Distance_to_forest_100m), family = Gamma(link = "log"), data = df)

# Open a connection to the text file
sink("GAM_simpson_distancetoforest.txt")
# Summarize the model
summary(gam_model)
# Close the connection to the text file
sink()

# Extract data for plotting
plot_data <- plot(gam_model, se = TRUE, rug = FALSE, pages = 1, shade = TRUE, shift = coef(gam_model)[1])

# Convert the extracted data to a data frame for use with ggplot2
plot_df <- data.frame(Distance_to_forest_100m = plot_data[[1]]$x, 
                      fit = plot_data[[1]]$fit, 
                      lower = plot_data[[1]]$fit - 2 * plot_data[[1]]$se, 
                      upper = plot_data[[1]]$fit + 2 * plot_data[[1]]$se)

# Improved plot with ggplot2
ggplot(plot_df, aes(x = Distance_to_forest_100m, y = fit)) +
  #geom_point(df, aes(x = Distance_to_forest_100m, y = TD_asy), size = 2, alpha = 0.6) +
  geom_line(color = "purple", linewidth = 1) +  # Plot the fitted line
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#D8BFD8", alpha = 0.4) +  # Add confidence intervals
  theme_minimal(base_size = 14) +  # Minimal theme for a cleaner look
  labs(
    #title = "Effect of Distance to Forest on Species Richness",
    x = "Distance to Forest (m)", 
    y = "Asymptotic Simpson diversity"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
    axis.title.x = element_text(size = 11),  # X-axis title style
    axis.title.y = element_text(size = 11),  # Y-axis title style
    axis.text = element_text(size = 10)  # Axis text size
  )



