#### Preamble ####
# Purpose: Executes hierarchical Bayesian logistic regression using Stan to 
#          analyze mortgage denial rates, including posterior predictive 
#          simulations (y_rep) for validation.
# Author: Arusan Surendiran
# Date: 29 April 2026


#### Workspace Setup ####
library(tidyverse)
library(rstan)
library(here)
library(beepr)


#### Helper Function ####
# Notification function for long-running models
done <- function(msg = "Code finished!") {
  beep(10)
  # Requires terminal-notifier for macOS
  try(system(paste0("terminal-notifier -message '", msg, "' -title 'RStudio Notification'")), silent = TRUE)
}


# Optimize Stan execution globally
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load pre-processed, scaled data
model_data <- readRDS(here("data", "02-analysis_data", "model_data.rds"))

### Stan Data Preparation

set.seed(2053)

# Create the fixed-effects model matrix (excluding the intercept)
X_matrix <- model.matrix(
  ~ race + sex + age + dti + log_income_z + ltv_z + 
    minority_pct_z + msa_income_pct_z, 
  data = model_data
)[, -1]

# Vectors for random intercepts (county and bank)
county_int <- as.integer(as.factor(model_data$county_code))
bank_int <- as.integer(as.factor(model_data$lender_id))

# Compile the final data list for Stan
stan_data <- list(
  N = nrow(model_data),
  K = ncol(X_matrix),
  J = length(unique(county_int)),
  L = length(unique(bank_int)),
  X = X_matrix,
  county_id = county_int,
  bank_id = bank_int,
  y = model_data$denied
)

### Bayesian Inference Model

cat("Starting Stan sampling...\n")
start_time <- Sys.time()

final_bayes_model <- stan(
  file = here("models/02-stan_code/mortgage_model.stan"),
  data = stan_data,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  seed = 2053
)

cat("Sampling complete. Saving model...\n")
saveRDS(final_bayes_model, file = here("models", "01-hierarchical_models", "final_bayes_model.rds"))

end_time <- Sys.time()
print(end_time - start_time)
done()



### Bayesian Validation Fit with Generated Quantities: Y_REP

cat("Starting Stan sampling...\n")
start_time <- Sys.time()

final_stan_fit <- stan(
  file = here("models/02-stan_code/mortgage_validate.stan"),
  data = stan_data,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  seed = 2053
)

cat("Sampling complete. Saving model...\n")
saveRDS(final_stan_fit, file = here("models", "01-hierarchical_models", "final_stan_fit.rds"))

end_time <- Sys.time()
print(end_time - start_time)
done()


### Saving Y_REP Simulations in Compressed RDS 

# Extract y_rep separately
y_rep <- rstan::extract(final_stan_fit, pars = "y_rep")$y_rep
saveRDS(y_rep, file = here("models", "01-hierarchical_models", "y_rep_simulations.rds"))

rm(final_stan_fit, y_rep)
gc()
done()

