library(tidyverse)
library(openxlsx)
library(dplyr)
library(plyr)

#df_filtered from data_cleaning.R

# Visualize with a box plot
boxplot(COUNT ~ HABITAT, df_filtered, main="Box Plot of Bird Counts", ylab="Bird Count", col="lightblue")

# Calculate Q1, Q3, and IQR
Q1 <- quantile(df_filtered$COUNT, 0.25)
Q3 <- quantile(df_filtered$COUNT, 0.75)
IQR <- Q3 - Q1

# Determine the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify and remove outliers
df_filtered <- df_filtered[df_filtered$COUNT >= lower_bound & df_filtered$COUNT <= upper_bound, ]

# Visualize cleaned data with a box plot
boxplot(COUNT ~ HABITAT,df_filtered, main="Box Plot of Bird Counts (Outliers Removed)", ylab="Bird Count", col="lightgreen")
