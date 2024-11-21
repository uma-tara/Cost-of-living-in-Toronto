#### Preamble ####
# Purpose: Statistically models WTI crude oil prices to enable further analysis and conclusions for a research paper.
# Author: Uma Sadhwani
# Date: 20 November 2024
# Contact: uma.sadhwani@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `analysis_data.csv` file must exist in the `data/02-analysis_data` directory.
# Ensure the tidyverse and rstanarm packages are installed.

#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
analysis_data <- read_csv("/home/rstudio/finalpaper/data/02-analysis_data/analysis_data.csv")

#### Model data ####

# Fit a Bayesian regression model
# Predicts WTI prices based on trends and other possible predictors
wti_price_model <-
  stan_glm(
    formula = wti_price ~ date,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )

#### Save model ####
# Ensure the directory exists before saving the model
if (!dir.exists("/home/rstudio/finalpaper/models")) {
  dir.create("/home/rstudio/finalpaper/models", recursive = TRUE)
}

saveRDS(
  wti_price_model,
  file = "/home/rstudio/finalpaper/models/wti_price_model.rds"
)

#### Summary of the model ####
# Display summary statistics for the model
model_summary <- summary(wti_price_model)
print(model_summary)

# Save the summary to a text file for further reference
write(
  capture.output(model_summary),
  file = "/home/rstudio/finalpaper/models/wti_price_model_summary.txt"
)

#### Display completion message ####
cat("Modeling complete. Model and summary saved in '/home/rstudio/finalpaper/models'.\n")
