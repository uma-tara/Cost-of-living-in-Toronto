#### Preamble ####
# Purpose: Explores the cleaned dataset of Western Texas Intermediate crude oil prices.
# Author: Uma Sadhwani
# Date: 20 November 2024
# Contact: uma.sadhwani@mail.utoronto.ca
# License: MIT
# Pre-requisites: The tidyverse package must be installed, and the cleaned dataset (analysis_data.csv) must exist in the specified directory.

#### Workspace setup ####
library(tidyverse)

#### Read data ####
analysis_data <- read_csv("/home/rstudio/finalpaper/data/02-analysis_data/analysis_data.csv")

#### Exploratory Data Analysis ####

# 1. Summary Statistics
cat("Summary of WTI Prices:\n")
summary(analysis_data$wti_price)

cat("\nDate Range:\n")
cat("Start Date:", min(analysis_data$date), "\n")
cat("End Date:", max(analysis_data$date), "\n")

# 2. Check for missing values
cat("\nMissing values in dataset:\n")
analysis_data |> summarise_all(~ sum(is.na(.))) |> print()

# 3. Plot WTI Prices Over Time
ggplot(analysis_data, aes(x = date, y = wti_price)) +
  geom_line() +
  labs(
    title = "WTI Crude Oil Prices Over Time",
    x = "Date",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal()

# 4. Histogram of WTI Prices
ggplot(analysis_data, aes(x = wti_price)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of WTI Crude Oil Prices",
    x = "Price (USD per barrel)",
    y = "Frequency"
  ) +
  theme_minimal()

# 5. Boxplot to Detect Outliers
ggplot(analysis_data, aes(y = wti_price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot of WTI Crude Oil Prices",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal()

# 6. Monthly Average Prices (Optional Aggregation)
analysis_data_monthly <- analysis_data |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(avg_price = mean(wti_price, na.rm = TRUE))

ggplot(analysis_data_monthly, aes(x = month, y = avg_price)) +
  geom_line(color = "blue") +
  labs(
    title = "Monthly Average WTI Crude Oil Prices",
    x = "Month",
    y = "Average Price (USD per barrel)"
  ) +
  theme_minimal()

#### Save Aggregated Data ####
# Save the monthly aggregated data for further analysis
write_csv(analysis_data_monthly, "/home/rstudio/finalpaper/data/02-analysis_data/analysis_data_monthly.csv")

#### Display completion message ####
cat("Exploratory Data Analysis completed. Aggregated data saved to '/home/rstudio/finalpaper/data/02-analysis_data/analysis_data_monthly.csv'\n")
