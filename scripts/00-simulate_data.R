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
election_date <- as.Date("2024-11-05")

# Check the structure of the dataset
print(glimpse(raw_data))

# Ensure the dataset has the expected structure
# Expected: A date column and a price column
colnames(raw_data) <- c("date", "wti_price")

# Convert date to Date format and filter the relevant time frame
raw_data <- raw_data %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2024-10-01") & date <= as.Date("2024-11-18"))

# Categorize into pre-election and post-election phases
simulated_data <- raw_data %>%
  mutate(election_phase = ifelse(date < election_date, "pre-election", "post-election"))

# Display the first few rows of the processed dataset
print(head(simulated_data))

# Save the processed dataset to a CSV file
write_csv(simulated_data, "/home/rstudio/finalpaper/data/00-simulated_data/simulated_data.csv")
