#### Preamble ####
# Purpose: Tests the structure and validity of the simulated WTI crude oil prices dataset.
# Author: Uma Sadhwani
# Date: 19 November 2024
# Contact: uma.sadhwani@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - 00-simulate_data.R must have been run
# - The `starter_folder` R project is set correctly

# Load necessary libraries
library(tidyverse)

# Define file path
file_path <- "/home/rstudio/finalpaper/data/00-simulated_data/simulated_data.csv"

# Check if the file exists
if (!file.exists(file_path)) {
  stop("Error: File not found at the specified path. Please verify the file location.")
}

# Load the dataset
data <- read_csv(file_path)

# Check and rename columns if necessary
if (all(names(data) == c("division", "state", "party"))) {
  data <- data %>%
    rename(
      date = division,           # Replace with the actual column name for dates
      wti_price = state,         # Replace with the actual column name for oil prices
      election_phase = party     # Replace with the actual column name for election phases
    )
  message("Column names have been updated to match the expected structure.")
} else {
  message("Column names do not match expected structure. Please verify the dataset.")
}

# Ensure columns exist before proceeding
required_columns <- c("date", "wti_price", "election_phase")
if (!all(required_columns %in% names(data))) {
  stop("Error: One or more required columns are missing from the dataset.")
}

# Convert data types
data <- data %>%
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),  # Convert `date` to Date format
    wti_price = as.numeric(wti_price),         # Convert `wti_price` to numeric
    election_phase = as.character(election_phase)  # Ensure `election_phase` is character
  )

# Test 1: Check for missing values
missing_values <- sum(is.na(data))
if (missing_values > 0) {
  message("Warning: The dataset contains ", missing_values, " missing values.")
} else {
  message("Test 1 Passed: No missing values detected.")
}

# Test 2: Check column names and structure
if (all(names(data) == required_columns)) {
  message("Test 2 Passed: Column names are as expected.")
} else {
  message("Test 2 Failed: Column names do not match the expected names.")
  print("Found columns:")
  print(names(data))
}

# Test 3: Validate data types
expected_types <- c("Date", "numeric", "character")
actual_types <- sapply(data, class)
if (all(actual_types == expected_types)) {
  message("Test 3 Passed: Data types are as expected.")
} else {
  message("Test 3 Failed: Data types do not match the expected types.")
  print("Actual data types:")
  print(actual_types)
}

# Test 4: Check for realistic ranges in numeric columns
# Use `na.rm = TRUE` to ignore NA values during the range check
price_check <- all(data$wti_price >= 0 & data$wti_price <= 150, na.rm = TRUE)

if (price_check) {
  message("Test 4 Passed: All numeric columns have realistic ranges.")
} else {
  message("Test 4 Failed: Numeric columns have values outside expected ranges.")
  
  # Diagnose issues
  print("Issues with ranges in wti_price:")
  print(data %>% filter(wti_price < 0 | wti_price > 150 | is.na(wti_price)))
}

# Test 5: Check for duplicates in the `date` column
if ("date" %in% names(data)) {
  # Drop rows with missing dates to avoid issues
  if (any(is.na(data$date))) {
    message("Warning: Rows with missing dates found. These will be removed before checking for duplicates.")
    data <- data %>% filter(!is.na(date))
  }
  
  # Check for duplicate dates
  duplicate_dates <- nrow(data) != nrow(data %>% distinct(date))
  if (!duplicate_dates) {
    message("Test 5 Passed: No duplicate dates detected.")
  } else {
    message("Test 5 Failed: Duplicate dates found in the dataset.")
    print(data %>% group_by(date) %>% filter(n() > 1))
  }
} else {
  message("Test 5 Failed: 'date' column is missing from the dataset.")
}

# Test 6: Verify election phase labels
# Test 6: Verify election phase labels
if ("election_phase" %in% names(data)) {
  # Check for invalid labels
  valid_phases <- all(data$election_phase %in% c("pre-election", "post-election"))
  if (valid_phases) {
    message("Test 6 Passed: All rows have valid election phase labels.")
  } else {
    message("Test 6 Failed: Invalid election phase labels found.")
    invalid_labels <- data %>% filter(!election_phase %in% c("pre-election", "post-election"))
    print("Rows with invalid election_phase labels:")
    print(invalid_labels)
    
    # Optional: Remove rows with invalid labels
    data <- data %>% filter(election_phase %in% c("pre-election", "post-election"))
    message("Rows with invalid election_phase labels have been removed.")
  }
} else {
  message("Test 6 Failed: 'election_phase' column is missing from the dataset.")
}
