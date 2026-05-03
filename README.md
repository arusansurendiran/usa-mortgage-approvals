# Investigating Mortgage Approval Discrimination in New York, U.S.A. (2023)

## Overview

This paper investigates the financial, demographic and geographic determinants of mortgage denial in New York. Using a Bayesian hierarchical logistic regression model, the results show statistically credible racial disparities between minorities and White applicants in loan outcomes, despite controlling for credit factors. Furthermore, biases were observed in the variation of denial rates across individual lenders and counties. The evidence suggests current fair lending enforcement is lacking on a statewide level and requires targeted interventions to address systematic mortgage lending discrimination.

## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from the (HMDA platform)[https://ffiec.cfpb.gov/data-browser/data/2023?category=states&items=NY&actions_taken=1,2,3&loan_purposes=1&getDetails=1].
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted Bayesian hierarchical models. 
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to clean data and model using `rstan`.