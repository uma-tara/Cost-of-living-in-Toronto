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
  stop("Error: The specified file does not exist. Please ensure the simulated dataset is available at: ", file_path)
}

# Load the simulated dataset
simulated_data <- read_csv(file_path)

# Check and rename columns if necessary
if (!"wti_price" %in% names(simulated_data)) {
  message("The `wti_price` column is missing. Attempting to rename columns to match expected structure.")
  print("Current column names:")
  print(names(simulated_data))
  
  # Example of renaming (adjust based on your dataset's actual column names)
  simulated_data <- simulated_data %>%
    rename(
      date = division,           # Replace with the actual column for dates
      wti_price = state,         # Replace with the actual column for oil prices
      election_phase = party     # Replace with the actual column for election phases
    )
  message("Column names have been updated.")
}

# Data Cleaning and Enhancement
analysis_data <- simulated_data %>%
  # Handle missing values (if any)
  filter(!is.na(date), !is.na(wti_price), !is.na(election_phase)) %>%
  
  # Add a new column for price changes (daily differences)
  arrange(date) %>%
  mutate(
    price_change = wti_price - lag(wti_price, default = NA),  # Calculate daily price change
    price_change = round(price_change, 2)  # Round for readability
  ) %>%
  
  # Categorize prices into low, medium, and high price ranges
  mutate(
    price_category = case_when(
      wti_price < 50 ~ "Low",
      wti_price >= 50 & wti_price <= 80 ~ "Medium",
      wti_price > 80 ~ "High"
    )
  ) %>%
  
  # Create a flag for significant price changes (threshold: ±10 USD)
  mutate(
    significant_change = ifelse(abs(price_change) >= 10, "Yes", "No")
  ) %>%
  
  # Keep only relevant columns
  select(date, wti_price, price_change, price_category, significant_change, election_phase)

# Save the enhanced dataset to the specified location
write_csv(analysis_data, output_path)
message("Enhanced dataset saved to: ", output_path)

# Display the first few rows of the enhanced dataset
print(head(analysis_data))

# Summary of the enhanced dataset
summary(analysis_data)
