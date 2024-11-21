#### Preamble ####
# Purpose: Simulates the raw dataset of Western Texas Intermediate crude oil prices
# over a given period, monitoring differences in prices closer and closer to Donald Trump's election.
# Author: Uma Sadhwani
# Date: 18 November 2024
# Contact: uma.sadhwani@mail.utoronto.ca
# License: MIT
# Pre-requisites: The tidyverse package must be installed.
# Ensure you are in the starter_folder R project.

# Load necessary libraries
library(tidyverse)

# Load the raw dataset
raw_data <- read_csv("/home/rstudio/finalpaper/data/01-raw_data/raw_data.csv")

# Set parameters for simulation
set.seed(123)  # Ensure reproducibility
n <- 365 * 5  # Data for 5 years (365 days each)
start_date <- as.Date("2024-10-01")
end_date <- as.Date("2024-11-18")

# Simulate dates
dates <- seq.Date(from = start_date, to = end_date, by = "day")

# Define election date
election_date <- as.Date("2024-11-05")

# Simulate oil prices
# Prices are normally distributed with slight increases and variability after the election
oil_prices <- ifelse(
  dates < election_date,
  rnorm(length(dates), mean = 60, sd = 5),  # Pre-election prices
  rnorm(length(dates), mean = 70, sd = 10) # Post-election prices
)

# Simulate price changes
# Introduce artificial shocks to mimic real-world events
price_shocks <- sample(c(0, -5, 5), n, replace = TRUE, prob = c(0.9, 0.05, 0.05))
oil_prices <- oil_prices + price_shocks

# Ensure prices remain realistic (non-negative)
oil_prices <- pmax(oil_prices, 0)

# Create dataset
simulated_data <- tibble(
  date = dates,
  wti_price = round(oil_prices, 2),
  election_phase = ifelse(dates < election_date, "pre-election", "post-election")
)

# Save the dataset to a CSV file
write_csv(simulated_data, "/home/rstudio/finalpaper/data/00-simulated_data/simulated_data.csv")

# Display the first few rows
print(head(simulated_data))

# Visualization
ggplot(simulated_data, aes(x = date, y = wti_price, color = election_phase)) +
  geom_line() +
  labs(
    title = "Simulated WTI Crude Oil Prices",
    x = "Date",
    y = "Price (USD per barrel)",
    color = "Election Phase"
  ) +
  theme_minimal()
