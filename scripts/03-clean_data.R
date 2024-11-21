#### Preamble ####
# Purpose: Cleans the raw dataset of Western Texas Intermediate crude oil prices
# over a given period, monitoring differences in prices closer and closer to Donald Trump's election.
# Author: Uma Sadhwani
# Date: 18 November 2024
# Contact: uma.sadhwani@mail.utoronto.ca
# License: MIT
# Pre-requisites: The tidyverse package must be installed.
# Ensure you are in the starter_folder R project.

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Load raw data ####
# Adjust the file path if necessary
raw_data <- read_csv("/home/rstudio/finalpaper/data/01-raw_data/raw_data.csv")

#### Clean data ####
analysis_data <- raw_data |>
  janitor::clean_names() |> # Standardize column names
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"), # Ensure the date column is in Date format
    price = as.numeric(dcoilwtico) # Convert price column to numeric
  ) |>
  filter(!is.na(price)) |> # Remove rows where the price is NA
  rename(wti_price = price) |> # Rename for clarity
  arrange(date) # Sort by date

#### Save cleaned data ####
# Ensure the output directory exists
if (!dir.exists("/home/rstudio/finalpaper/data/02-analysis_data")) {
  dir.create("/home/rstudio/finalpaper/data/02-analysis_data", recursive = TRUE)
}

write_csv(analysis_data, "/home/rstudio/finalpaper/data/02-analysis_data/analysis_data.csv")

#### Display a message ####
cat("Data cleaning complete. Cleaned data saved to '/home/rstudio/finalpaper/data/02-analysis_data/analysis_data.csv'\n")
