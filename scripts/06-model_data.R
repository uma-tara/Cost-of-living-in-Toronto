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
library(bayesplot)

#### Read and preprocess data ####
analysis_data <- read_csv("/home/rstudio/finalpaper/data/02-analysis_data/analysis_data.csv") %>%
  mutate(
    date = as.Date(date)  # Ensure `date` is in Date format
  ) %>%
  arrange(date) %>%
  mutate(
    date_numeric = as.numeric(date - min(date)),  # Convert date to numeric
    daily_pct_change = 100 * (wti_price - lag(wti_price)) / lag(wti_price),  # Daily % change
    price_change = wti_price - lag(wti_price),  # Absolute price change
    election_phase = ifelse(date < as.Date("2024-11-05"), "pre-election", "post-election")  # Election phase
  ) %>%
  mutate(election_phase = as.factor(election_phase)) %>%
  filter(!is.na(wti_price) & !is.na(daily_pct_change) & !is.na(price_change))  # Remove missing data

#### Model data ####
wti_price_model <- stan_glm(
  formula = wti_price ~ date_numeric + election_phase + daily_pct_change + price_change,
  data = analysis_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_aux = exponential(rate = 1, autoscale = TRUE),
  seed = 853
)

#### Save model ####
if (!dir.exists("/home/rstudio/finalpaper/models")) {
  dir.create("/home/rstudio/finalpaper/models", recursive = TRUE)
}
saveRDS(wti_price_model, file = "/home/rstudio/finalpaper/models/wti_price_model.rds")

#### Model diagnostics and results ####
model_summary <- summary(wti_price_model)
print(model_summary)

write(
  capture.output(model_summary),
  file = "/home/rstudio/finalpaper/models/wti_price_model_summary.txt"
)

pp_check_plot <- pp_check(wti_price_model)
ggsave(
  filename = "/home/rstudio/finalpaper/models/wti_price_model_pp_check.png",
  plot = pp_check_plot
)

# Retrieve and print parameter names
parameter_names <- colnames(as.matrix(wti_price_model))
print(parameter_names)

# Use correct parameter names in mcmc_areas
plot_coefficients <- mcmc_areas(
  as.matrix(wti_price_model),
  pars = c("(Intercept)", "date_numeric", "election_phasepre-election", "daily_pct_change", "price_change")
)
ggsave(
  filename = "/home/rstudio/finalpaper/models/wti_price_model_coefficients.png",
  plot = plot_coefficients
)

model_coefficients <- summary(wti_price_model)
write.csv(
  model_coefficients,
  file = "/home/rstudio/finalpaper/models/wti_price_model_coefficients.csv"
)

cat("Modeling complete. Model, diagnostics, and summary outputs saved in '/home/rstudio/finalpaper/models'.\n")
