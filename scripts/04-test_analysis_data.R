#### Preamble ####
# Purpose: Tests the cleaned dataset of Western Texas Intermediate crude oil prices.
# Author: Uma Sadhwani
# Date: 20 November 2024
# Contact: uma.sadhwani@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `analysis_data.csv` file must exist in the `data/02-analysis_data` directory.
# Ensure the tidyverse and testthat packages are installed.

#### Workspace setup ####
library(tidyverse)
library(testthat)

# Load the data
analysis_data <- read_csv("/home/rstudio/finalpaper/data/02-analysis_data/analysis_data.csv")

#### Test data ####

# Test that the dataset contains the required columns
required_columns <- c("date", "wti_price")
test_that("dataset contains required columns", {
  expect_true(all(required_columns %in% colnames(analysis_data)))
})

# Test that the dataset has at least the required number of columns
test_that("dataset has at least required number of columns", {
  expect_gte(ncol(analysis_data), length(required_columns))
})

# Test that the dataset has more than 0 rows
test_that("dataset has more than 0 rows", {
  expect_gt(nrow(analysis_data), 0)
})

# Test that the 'date' column is of Date type
test_that("'date' is of Date type", {
  expect_true(inherits(analysis_data$date, "Date"))
})

# Test that the 'wti_price' column is numeric
test_that("'wti_price' is numeric", {
  expect_type(analysis_data$wti_price, "double")
})

# Test that there are no missing values in the dataset
test_that("no missing values in dataset", {
  expect_true(all(!is.na(analysis_data)))
})

# Test that the 'wti_price' column has no negative values
test_that("'wti_price' has no negative values", {
  expect_true(all(analysis_data$wti_price >= 0))
})

# Test that the 'date' column is sorted in ascending order
test_that("'date' is sorted in ascending order", {
  expect_true(all(analysis_data$date == sort(analysis_data$date)))
})

# Test that the 'date' column covers a valid range
expected_start_date <- as.Date("2019-01-01")
expected_end_date <- as.Date("2024-11-18")
test_that("'date' column covers the expected range", {
  expect_gte(min(analysis_data$date), expected_start_date)
  expect_lte(max(analysis_data$date), expected_end_date)
})

# Test that there are no duplicate rows in the dataset
test_that("no duplicate rows in the dataset", {
  expect_equal(nrow(analysis_data), nrow(distinct(analysis_data)))
})

# Test that the 'wti_price' column contains realistic values (e.g., below $200 per barrel)
test_that("'wti_price' contains realistic values", {
  expect_true(all(analysis_data$wti_price <= 200))
})

#### Display message ####
cat("All tests completed.\n")
