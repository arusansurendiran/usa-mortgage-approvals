#### Preamble ####
# Purpose: Prepares cleaned New York mortgage data for modeling by scaling 
#          continuous variables and assessing multicollinearity.
# Author: Arusan Surendiran
# Date: 29 April 2026


#### Workspace Setup ####
library(tidyverse)
library(arrow)
library(here)


#### Load Data ####
loans_NY <- read_parquet(here("data", "02-analysis_data", "loans_NY_clean.parquet"))

# SELECT MODELING FEATURES, isolate only the variables required for the model

loans_model_data <- loans_NY |>
  select(
    denied, race, applicant_poc, sex, age_bracket, 
    log_income, dti_bracket, combined_loan_to_value_ratio, 
    log_loan_amount, tract_minority_population_percent, 
    tract_to_msa_income_percentage, lender_id, county_code, census_tract)


#### Preprocessing

# SET FACTOR BASELINES & STANDARDIZE CONTINUOUS VARIABLES

model_data <- loans_model_data |>
  mutate(
    age = factor(age_bracket, ordered = FALSE),
    dti = factor(dti_bracket, ordered = FALSE),
    
    # Standardize all continuous predictors
    log_income_z = scale(log_income)[, 1],
    log_loan_amount_z = scale(log_loan_amount)[, 1],
    ltv_z = scale(combined_loan_to_value_ratio)[, 1],
    minority_pct_z = scale(tract_minority_population_percent)[, 1],
    msa_income_pct_z = scale(tract_to_msa_income_percentage)[, 1])

## Correlation Analysis

continuous_vars <- model_data |>
  select(log_income_z, log_loan_amount_z, ltv_z, minority_pct_z, msa_income_pct_z)

cor_matrix <- cor(continuous_vars, use = "complete.obs")
round(cor_matrix, 2)

# Dropping redundant covariate log_loan_amount_z
model_data_final <- model_data |>
  select(-log_loan_amount_z)

#### EXPORT MODEL-READY DATA
saveRDS(model_data_final, here("data", "02-analysis_data", "model_data.rds"))






