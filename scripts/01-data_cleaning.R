#### Preamble ####
# Purpose: Cleans and prepares New York mortgage loan data for analysis, 
#          including feature engineering and missing data reporting.
# Author: Arusan Surendiran
# Date: 29 April 2026
# Data Source: https://ffiec.cfpb.gov/data-browser/data/2023?category=states&items=NY&actions_taken=1,2,3&loan_purposes=1&getDetails=1
# Pre-requisites: 
  # - Access to "data/01-raw_data/loans_NY.parquet"


#### Workspace Setup ####
library(tidyverse)
library(here)
library(arrow)
library(scales)


### LOAD DATA
loans <- read_parquet(here("data", "01-raw_data", "loans_NY.parquet"))

# Define target columns
cols_of_interest <- c(
  "lei", "census_tract", "county_code", "state_code", 
  "action_taken", 
  
  "income", "debt_to_income_ratio", "combined_loan_to_value_ratio", 
  "loan_amount", 
  
  "derived_race", "derived_sex", "applicant_age", 
  
  "tract_minority_population_percent","tract_to_msa_income_percentage")

### DATA CLEANING & FEATURE ENGINEERING

# Isolate for standard mortgages, rename LEI, and build initial features
loans_step1 <- loans |>
  filter(
    action_taken %in% c(1, 2, 3),
    derived_loan_product_type == "Conventional:First Lien",
    loan_purpose == 1, 
    occupancy_type == 1,
    reverse_mortgage == 2, 
    business_or_commercial_purpose == 2,
    total_units %in% c(1,2)) |>
  select(all_of(cols_of_interest)) |>
  mutate(
    combined_loan_to_value_ratio = readr::parse_number(combined_loan_to_value_ratio))

# Drop Missing/Withheld Demographics
loans_step2 <- loans_step1 |>
  filter(
    !derived_race %in% c("Race Not Available", "Free Form Text Only"),
    derived_sex != "Sex Not Available")

# Drop Other NAs
loans_step3 <- loans_step2 |>
  filter(applicant_age != "8888") |>
  drop_na()

# Outlier Removal by calculating bounds based on data
cltv_99th <- quantile(loans_step3$combined_loan_to_value_ratio, 0.99, na.rm = TRUE)
income_99th <- quantile(loans_step3$income, 0.99, na.rm = TRUE)
loan_high <- 1e7

loans_step4 <- loans_step3 |>
  filter(
    combined_loan_to_value_ratio > 0, 
    combined_loan_to_value_ratio <= cltv_99th,
    income > 0, 
    income <= income_99th,
    loan_amount < loan_high)

# Filter Small Lenders
loans_step5 <- loans_step4 |>
  rename(lender_id = lei) |>
  group_by(lender_id) |>
  filter(n() >= 50) |> 
  ungroup()

# VARIABLE TRANSFORMATIONS AND LEVELS
loans_final <- loans_step5 |>
  mutate(
    denied = ifelse(action_taken == 3, 1, 0),
    log_income = log(income),
    log_loan_amount = log(loan_amount),
    
    dti_bracket = case_when(
      debt_to_income_ratio %in% c("<20%", "20%-<30%", "30%-<36%") ~ "<36%",
      debt_to_income_ratio %in% as.character(36:49) ~ "36%-49%",
      debt_to_income_ratio == "50%-60%" ~ "50%-60%",
      debt_to_income_ratio == ">60%" ~ ">60%",
      TRUE ~ NA_character_
    ),
    
    race = case_when(
      derived_race == "White" ~ "White",
      derived_race == "Black or African American" ~ "Black",
      derived_race == "Asian" ~ "Asian",
      derived_race == "Joint" ~ "Joint",
      derived_race %in% c("American Indian or Alaska Native", 
                          "2 or more minority races", 
                          "Native Hawaiian or Other Pacific Islander") ~ "Other/Multiracial"
    ),
    
    race = factor(race, levels = c("White", "Black", "Asian", "Joint", "Other/Multiracial")),
    age_bracket = factor(applicant_age, levels = c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", ">74"), ordered = TRUE),
    sex = factor(derived_sex, levels = c("Male", "Female", "Joint")),
    status = factor(ifelse(denied == 1, "Denied", "Approved"), levels = c("Approved", "Denied")),
    dti_bracket = factor(dti_bracket, levels = c("<36%", "36%-49%", "50%-60%", ">60%"), ordered = TRUE),
    applicant_poc = ifelse(race == "White", "White", "POC/Non-White")
  )

# EXPORT
write_parquet(loans_final, here("data", "02-analysis_data", "loans_NY_clean.parquet"))


### MISSING DATA ANALYSIS

## Missingness among Columns

n_step1 <- nrow(loans_step1)

# Calculate missing data and outliers on the base population
missing_data <- loans_step1 |>
  summarise(
    `Race---Withheld (NA)` = sum(derived_race %in% c("Race Not Available", "Free Form Text Only"), na.rm = TRUE),
    `Sex---Withheld (NA)`  = sum(derived_sex == "Sex Not Available", na.rm = TRUE),
    `Age---Missing (NA)`      = sum(applicant_age == "8888", na.rm = TRUE),
    
    `Loan Amount---Outlier (High)`    = sum(loan_amount > loan_high, na.rm = TRUE),
    `Income---Invalid Value (<= 0)`   = sum(income <= 0, na.rm = TRUE),
    `Income---Outlier (High)`         = sum(income > income_99th, na.rm = TRUE),
    `Combined Loan To Value Ratio---Outlier (High)`           = sum(combined_loan_to_value_ratio > cltv_99th, na.rm = TRUE),
    
    # Standard NAs across all columns of interest
    across(all_of(cols_of_interest), ~sum(is.na(.)), .names = "{.col}---Missing (NA)")) |>
  
  pivot_longer(everything(), names_to = "Raw_Name", values_to = "Count") |>
  filter(Count > 0) |> 
  separate(Raw_Name, into = c("Variable", "Reason"), sep = "---") |>
  mutate(
    Percent_of_Total = round((Count / n_step1) * 100, 2),
    Variable = str_to_title(str_replace_all(Variable, "_", " "))) |>
  arrange(desc(Percent_of_Total)) |>
  select(Variable, Reason, Count, Percent_of_Total)


## Summary of Data Removed 

n_step1 <- nrow(loans_step1)
n_step2 <- nrow(loans_step2)
n_step3 <- nrow(loans_step3)
n_step4 <- nrow(loans_step4)
n_step5 <- nrow(loans_step5)

data_removed <- tibble(
  Filter_Applied = c(
    "Missing Race & Sex",
    "NAs in Other Columns",
    "Extreme Outliers",
    "Small Lenders (n < 50)"),
  Observations_Removed = c(
    n_step1 - n_step2,
    n_step2 - n_step3,
    n_step3 - n_step4,
    n_step4 - n_step5),
  Percent_of_Base = c(
    round(((n_step1 - n_step2) / n_step1) * 100, 2),
    round(((n_step2 - n_step3) / n_step1) * 100, 2),
    round(((n_step3 - n_step4) / n_step1) * 100, 2),
    round(((n_step4 - n_step5) / n_step1) * 100, 2)))


# Total starting rows
total_n <- nrow(loans_step1)

# Calculate records dropped from missing Race
data_after_race <- loans_step1 |>
  filter(!derived_race %in% c("Race Not Available", "Free Form Text Only"))

race_loss_count <- total_n - nrow(data_after_race)

# Calculate records dropped from missing Sex (from the remaining data)
data_after_sex <- data_after_race |>
  filter(derived_sex != "Sex Not Available")

sex_loss_count <- nrow(data_after_race) - nrow(data_after_sex)

# Create the simplified table
demographic_loss_table <- tibble(
  Filter_Applied = c("Missing Race", "Missing Sex (Remaining)", "Total Loss"),
  Records_Dropped = c(race_loss_count, sex_loss_count, race_loss_count + sex_loss_count),
  Percent_of_Total = c(
    round((race_loss_count / total_n) * 100, 2),
    round((sex_loss_count / total_n) * 100, 2),
    round(((race_loss_count + sex_loss_count) / total_n) * 100, 2)))


## Profile of Applicants Missing Racial Demographics

# Build missingness table
missing_race_summary <- loans_step1 |>
  mutate(
    Race_Status = ifelse(
      derived_race %in% c("Race Not Available", "Free Form Text Only"), 
      "Missing", "Disclosed")) |>
  group_by(Race_Status) |>
  summarise(
    Total_Applicants = n(),
    Mean_Income = round(mean(income, na.rm = TRUE), 0),
    Mean_Loan = round(mean(loan_amount, na.rm = TRUE), 0),
    Denial_Rate = paste0(round(mean(action_taken == 3, na.rm = TRUE) * 100, 1), "%"),
    Missing_Income_Pct = paste0(round(mean(is.na(income)) * 100, 2), "%")) |>
  mutate(
    Percent_of_Base = paste0(round((Total_Applicants / sum(Total_Applicants)) * 100, 1), "%")) |>
  select(Race_Status, Total_Applicants, Percent_of_Base, Mean_Income, Mean_Loan, Denial_Rate, Missing_Income_Pct)

## Lender Analysis

# Aggregate applications per lender
lender_stats <- loans_step4 |>
  group_by(lei) |>
  summarise(total_apps = n()) |>
  mutate(
    status = ifelse(total_apps >= 50, "Kept (>= 50)", "Dropped (< 50)")
  )

lender_summary <- lender_stats |>
  group_by(status) |>
  summarise(
    Total_Lenders = n(),
    Total_Apps_Lost_or_Kept = sum(total_apps),
    Median_Apps = median(total_apps),
    Mean_Apps = round(mean(total_apps), 1),
    Max_Apps = max(total_apps)) |>
  arrange(status)
