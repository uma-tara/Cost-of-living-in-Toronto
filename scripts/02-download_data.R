#### Preamble ####
# Purpose: Cleans and enhances the simulated WTI crude oil prices dataset for statistical analysis.
# Author: Uma Sadhwani
# Date: 18 November 2024
# Contact: uma.sadhwani@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded.
# - Ensure this script is run in the `starter_folder` R project.
# - The `simulated_data.csv` dataset is available at the specified location.

#### Workspace setup ####
library(tidyverse)

# Define the source and output file paths
file_path <- "/home/rstudio/finalpaper/data/00-simulated_data/simulated_data.csv"  # Path to the simulated dataset
output_path <- "/home/rstudio/finalpaper/data/02-analysis_data/analysis_data.csv"  # Path to save the enhanced dataset

# Check if the source file exists
if (!file.exists(file_path)) {
  stop(paste0("Error: The specified file does not exist. Please ensure the simulated dataset is available at: ", file_path))
}

# Load the simulated dataset
simulated_data <- read_csv(file_path)

# Check the structure of the dataset
glimpse(simulated_data)

# Ensure the dataset has the expected structure
# Expected: A `date` column and a `wti_price` column
# Handle column mismatches if necessary
if (!all(c("date", "wti_price") %in% colnames(simulated_data))) {
  stop("Error: The dataset does not contain the expected columns: 'date' and 'wti_price'.")
}

# Convert date to Date format (if not already) and handle non-numeric `wti_price`
simulated_data <- simulated_data %>%
  mutate(
    date = as.Date(date),
    wti_price = as.numeric(wti_price)
  ) %>%
  filter(!is.na(date) & !is.na(wti_price))

# Data Cleaning and Enhancement
analysis_data <- simulated_data %>%
  arrange(date) %>%
  
  # Add a column for price changes (daily differences)
  mutate(
    price_change = round(wti_price - lag(wti_price, default = NA), 2)
  ) %>%
  
  # Categorize prices into low, medium, and high price ranges
  mutate(
    price_category = case_when(
      wti_price < 68 ~ "Low",
      wti_price >= 68 & wti_price < 70 ~ "Medium",
      wti_price >= 70 ~ "High"
    )
  ) %>%
  
  # Create a flag for significant price changes (threshold: ±10 USD)
  mutate(
    significant_change = ifelse(abs(price_change) >= 2, "Yes", "No")
  ) %>%
  
  # Keep only relevant columns
  select(date, wti_price, price_change, price_category, significant_change)

# Save the enhanced dataset to the specified location
output_dir <- dirname(output_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(analysis_data, output_path)
message("Enhanced dataset saved to: ", output_path)

# Display the first few rows of the enhanced dataset
print(head(analysis_data))

# Summary of the enhanced dataset
summary(analysis_data)
