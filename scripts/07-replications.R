#### Preamble ####
# Purpose: Replicated graphs from exploratory data analysis
# Author: Uma Sadhwani
# Date: 21 November 2024
# Contact: uma.sadhwani@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse library installed

#### Workspace setup ####
library(tidyverse)
library(zoo)  # For rolling averages

#### Ensure the directory exists ####
output_dir <- "/home/rstudio/finalpaper/eda"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  if (dir.exists(output_dir)) {
    cat("Directory successfully created: ", output_dir, "\n")
  } else {
    stop("Failed to create directory: ", output_dir)
  }
}

#### Load data ####
analysis_data <- read_csv("/home/rstudio/finalpaper/data/02-analysis_data/analysis_data.csv")

#### 1. WTI Prices Over Time ####
wti_time_plot <- ggplot(analysis_data, aes(x = date, y = wti_price)) +
  geom_line() +
  labs(
    title = "WTI Crude Oil Prices Over Time",
    x = "Date",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal()

print(wti_time_plot)
ggsave(filename = file.path(output_dir, "wti_prices_over_time.png"), plot = wti_time_plot)

#### 2. Histogram of WTI Prices ####
wti_histogram <- ggplot(analysis_data, aes(x = wti_price)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of WTI Crude Oil Prices",
    x = "Price (USD per barrel)",
    y = "Frequency"
  ) +
  theme_minimal()

print(wti_histogram)
ggsave(filename = file.path(output_dir, "wti_price_histogram.png"), plot = wti_histogram)

#### 3. Boxplot to Detect Outliers ####
wti_boxplot <- ggplot(analysis_data, aes(y = wti_price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot of WTI Crude Oil Prices",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal()

print(wti_boxplot)
ggsave(filename = file.path(output_dir, "wti_price_boxplot.png"), plot = wti_boxplot)

#### 4. Monthly Average Prices ####
analysis_data_monthly <- analysis_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(avg_price = mean(wti_price, na.rm = TRUE))

monthly_avg_plot <- ggplot(analysis_data_monthly, aes(x = month, y = avg_price)) +
  geom_line(color = "blue") +
  labs(
    title = "Monthly Average WTI Crude Oil Prices",
    x = "Month",
    y = "Average Price (USD per barrel)"
  ) +
  theme_minimal()

print(monthly_avg_plot)
ggsave(filename = file.path(output_dir, "monthly_avg_prices.png"), plot = monthly_avg_plot)

#### 5. Rolling Average WTI Prices (7-day moving average) ####
analysis_data <- analysis_data %>%
  arrange(date) %>%
  mutate(rolling_avg = zoo::rollmean(wti_price, k = 7, fill = NA, align = "right"))

rolling_avg_plot <- ggplot(analysis_data, aes(x = date)) +
  geom_line(aes(y = wti_price), color = "grey", alpha = 0.7) +
  geom_line(aes(y = rolling_avg), color = "blue") +
  labs(
    title = "WTI Crude Oil Prices with 7-Day Rolling Average",
    x = "Date",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal()

print(rolling_avg_plot)
ggsave(filename = file.path(output_dir, "rolling_avg_prices.png"), plot = rolling_avg_plot)

#### 6. Daily Percentage Change in Prices ####
analysis_data <- analysis_data %>%
  mutate(daily_pct_change = 100 * (wti_price - lag(wti_price)) / lag(wti_price))

daily_pct_change_plot <- ggplot(analysis_data, aes(x = date, y = daily_pct_change)) +
  geom_line(color = "red", alpha = 0.7) +
  labs(
    title = "Daily Percentage Change in WTI Crude Oil Prices",
    x = "Date",
    y = "Percentage Change (%)"
  ) +
  theme_minimal()

print(daily_pct_change_plot)
ggsave(filename = file.path(output_dir, "daily_pct_change.png"), plot = daily_pct_change_plot)

#### 7. Boxplot by Election Phase ####
election_date <- as.Date("2024-11-05")
analysis_data <- analysis_data %>%
  mutate(
    election_phase = ifelse(date < election_date, "pre-election", "post-election"),
    election_phase = as.factor(election_phase)
  )

election_phase_boxplot <- ggplot(analysis_data, aes(x = election_phase, y = wti_price, fill = election_phase)) +
  geom_boxplot() +
  labs(
    title = "WTI Crude Oil Prices by Election Phase",
    x = "Election Phase",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(election_phase_boxplot)
ggsave(filename = file.path(output_dir, "wti_prices_by_election_phase.png"), plot = election_phase_boxplot)

#### Completion message ####
cat("Graphs replicated and saved in '", output_dir, "'.\n")
