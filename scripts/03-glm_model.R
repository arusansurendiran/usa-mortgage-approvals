#### Preamble ####
# Purpose: Fits a binomial logistic mixed-effects model (GLMM) to predict 
#          mortgage denial, accounting for random effects at the lender 
#          and county levels.
# Author: Arusan Surendiran
# Date: 29 April 2026



#### Workspace Setup ####
library(tidyverse)
library(lme4)
library(performance)
library(beepr)
library(here)


#### Helper Function ####
# Notification function for long-running models
done <- function(msg = "Code finished!") {
  beep(10)
  # Requires terminal-notifier for macOS
  try(system(paste0("terminal-notifier -message '", msg, "' -title 'RStudio Notification'")), silent = TRUE)
}

# Load the pre-processed, scaled data from the previous script
model_data <- readRDS(here("data", "02-analysis_data", "model_data.rds"))


### Mixed-Effects Modeling

cat("Fitting GLMER model... \n")
start_time <- Sys.time()

final_glmer_model <- glmer(
  denied ~ race + sex + age + dti + 
    log_income_z + ltv_z + minority_pct_z + msa_income_pct_z +
    (1 | county_code) + (1 | lender_id),
  data = model_data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

end_time <- Sys.time()
cat("Model fitting complete. Run time:\n")
print(end_time - start_time)

saveRDS(final_glmer_model, here("models", "01-hierarchical_models", "final_glmer_model.rds"))


## Summary of Results

final_glmer_model <- readRDS(here("models", "01-hierarchical_models", "final_glmer_model.rds"))

cat("Model Summary \n")
summary(final_glmer_model)









