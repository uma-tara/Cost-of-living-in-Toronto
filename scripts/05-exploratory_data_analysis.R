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

#### Additional Statistical Charts and Tables ####

# 7. Rolling Average WTI Prices (7-day moving average)
analysis_data <- analysis_data %>%
  arrange(date) %>%
  mutate(rolling_avg = zoo::rollmean(wti_price, k = 7, fill = NA, align = "right"))

ggplot(analysis_data, aes(x = date)) +
  geom_line(aes(y = wti_price), color = "grey", alpha = 0.7) +
  geom_line(aes(y = rolling_avg), color = "blue") +
  labs(
    title = "WTI Crude Oil Prices with 7-Day Rolling Average",
    x = "Date",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal()

# 8. Price Volatility (Daily Percentage Change)
analysis_data <- analysis_data %>%
  mutate(daily_pct_change = 100 * (wti_price - lag(wti_price)) / lag(wti_price))

ggplot(analysis_data, aes(x = date, y = daily_pct_change)) +
  geom_line(color = "red", alpha = 0.7) +
  labs(
    title = "Daily Percentage Change in WTI Crude Oil Prices",
    x = "Date",
    y = "Percentage Change (%)"
  ) +
  theme_minimal()

# 9. Correlation Matrix
# Check if other numerical columns exist to calculate correlations
numeric_data <- analysis_data %>%
  select(where(is.numeric)) %>%
  na.omit()  # Remove rows with NA for correlation calculation

correlation_matrix <- cor(numeric_data)
print("Correlation Matrix:")
print(correlation_matrix)

# Heatmap of the Correlation Matrix
heatmap(correlation_matrix, main = "Correlation Heatmap", symm = TRUE, col = colorRampPalette(c("blue", "white", "red"))(100))

# 10. Summary Table: WTI Prices by Election Phase
#### Create Election Phase Column ####
election_date <- as.Date("2024-11-05")

analysis_data <- analysis_data %>%
  mutate(
    election_phase = ifelse(date < election_date, "pre-election", "post-election")
  )

#### Summary Table: WTI Prices by Election Phase ####
summary_table <- analysis_data %>%
  group_by(election_phase) %>%
  summarise(
    min_price = min(wti_price, na.rm = TRUE),
    max_price = max(wti_price, na.rm = TRUE),
    avg_price = mean(wti_price, na.rm = TRUE),
    median_price = median(wti_price, na.rm = TRUE),
    sd_price = sd(wti_price, na.rm = TRUE)
  )
print(summary_table)

summary_table <- analysis_data %>%
  group_by(election_phase) %>%
  summarise(
    min_price = min(wti_price, na.rm = TRUE),
    max_price = max(wti_price, na.rm = TRUE),
    avg_price = mean(wti_price, na.rm = TRUE),
    median_price = median(wti_price, na.rm = TRUE),
    sd_price = sd(wti_price, na.rm = TRUE)
  )
print("Summary Table by Election Phase:")
print(summary_table)

# 11. Boxplot Comparison by Election Phase
ggplot(analysis_data, aes(x = election_phase, y = wti_price, fill = election_phase)) +
  geom_boxplot() +
  labs(
    title = "WTI Crude Oil Prices by Election Phase",
    x = "Election Phase",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#### Display completion message ####
cat("Exploratory Data Analysis completed.\n")
