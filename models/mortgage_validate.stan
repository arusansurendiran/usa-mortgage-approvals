// -------------------------------------------------------------------------
// File: mortgage_validate.stan
// Description: Full Bayesian hierarchical logistic regression model.
//              Includes generated quantities for Leave-One-Out Cross 
//              Validation (LOO-CV) and Posterior Predictive Checks (PPC).
// -------------------------------------------------------------------------

data {
  int<lower=1> N; // Total number of applicants
  int<lower=1> K; // Number of fixed effect predictors (columns in model matrix)
  int<lower=1> J; // Number of counties
  int<lower=1> L; // Number of banks (LEI)

  matrix[N, K] X; // The predictor matrix
  
  array[N] int<lower=1, upper=J> county_id; // Array mapping applicant to county
  array[N] int<lower=1, upper=L> bank_id;   // Array mapping applicant to bank
  array[N] int<lower=0, upper=1> y;         // Outcome: Denied (1) or Approved (0)
}

parameters {
  real alpha;          // Global intercept
  vector[K] beta;      // Fixed effects coefficients

  // Non-centered parameterization for Counties
  vector[J] eta_county;
  real<lower=0> sigma_county;

  // Non-centered parameterization for Banks
  vector[L] eta_bank;
  real<lower=0> sigma_bank;
}

transformed parameters {
  // Reconstruct the actual random intercepts by scaling the standard normal 
  // eta by the standard deviation sigma
  vector[J] a_county = sigma_county * eta_county;
  vector[L] a_bank = sigma_bank * eta_bank;
}

model {
  // Priors, using weakly informative priors to regularize
  alpha ~ normal(0, 2.5);
  beta ~ normal(0, 2.5); 

  // Priors for the non-centered components
  eta_county ~ normal(0, 1);
  eta_bank ~ normal(0, 1);
  
  // Half-normal(0,1) priors for scale parameters
  sigma_county ~ normal(0, 1); 
  sigma_bank ~ normal(0, 1);

  // Likelihood
  // Construct the linear predictor, logit_p
  vector[N] logit_p = alpha + X * beta + a_county[county_id] + a_bank[bank_id];
  
  // Bernoulli logit links the probability to the linear predictor directly
  y ~ bernoulli_logit(logit_p);
}

generated quantities {
  vector[N] log_lik;
  array[N] int y_rep; 

  {
    // Calculate the linear predictor once per draw
    vector[N] logit_p = alpha + X * beta + a_county[county_id] + a_bank[bank_id];
    
    // Generate both the LOO-CV metric and the PPC draw
    for (n in 1:N) {
      log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p[n]);
      y_rep[n]   = bernoulli_logit_rng(logit_p[n]);
    }
  }
}
