library(ggplot2)
library(agricolae)
library(car)
library(FSA)
library(Rmisc)
library(nortest)
library(openxlsx)
library(dplyr)

df <- read.csv("/home/rb857/audiomoth_acoustic_diversity.csv")

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
