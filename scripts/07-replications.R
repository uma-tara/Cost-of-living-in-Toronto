#### Preamble ####
# Purpose: Replicated graphs from exploratory data analysis
# Author: Uma Sadhwani 
# Date: 21 November 2024 
# Contact: uma.sadhwani@mail.utoronto.ca 
# License: MIT
# Pre-requisites: tidyverse library installed
# Any other information needed? n/a


#### Workspace setup ####
library(tidyverse)

# 1. WTI Prices Over Time
plot1 <- ggplot(analysis_data, aes(x = date, y = wti_price)) +
  geom_line(color = "blue") +
  labs(
    title = "WTI Crude Oil Prices Over Time",
    x = "Date",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal()

ggsave(
  filename = "/home/rstudio/finalpaper/paper/wti_prices_over_time.png",
  plot = plot1,
  width = 10,
  height = 6
)

# 2. Histogram of WTI Prices
plot2 <- ggplot(analysis_data, aes(x = wti_price)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of WTI Crude Oil Prices",
    x = "Price (USD per barrel)",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(
  filename = "/home/rstudio/finalpaper/paper/wti_price_distribution.png",
  plot = plot2,
  width = 10,
  height = 6
)

# 3. Monthly Average Prices
analysis_data_monthly <- analysis_data |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(avg_price = mean(wti_price, na.rm = TRUE))

plot3 <- ggplot(analysis_data_monthly, aes(x = month, y = avg_price)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Monthly Average WTI Crude Oil Prices",
    x = "Month",
    y = "Average Price (USD per barrel)"
  ) +
  theme_minimal()

ggsave(
  filename = "/home/rstudio/finalpaper/paper/monthly_avg_prices.png",
  plot = plot3,
  width = 10,
  height = 6
)

# 4. Boxplot to Detect Outliers
plot4 <- ggplot(analysis_data, aes(y = wti_price)) +
  geom_boxplot(fill = "lightcoral") +
  labs(
    title = "Boxplot of WTI Crude Oil Prices",
    y = "Price (USD per barrel)"
  ) +
  theme_minimal()

ggsave(
  filename = "/home/rstudio/finalpaper/paper/wti_price_boxplot.png",
  plot = plot4,
  width = 10,
  height = 6
)

#### Display Completion Message ####
cat("Graphs replicated and saved to '/home/rstudio/finalpaper/paper/'.\n")
