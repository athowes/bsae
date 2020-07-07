// model6.stan: BYM3

data {
  int<lower=1> n; // Number of regions
  int y[n]; // Vector of responses
  int m[n]; // Vector of sample sizes
  
  vector[n] mu; // Prior mean vector
  matrix[n, n] Sigma; // Prior covariance matrix (avoid double validation)
  
  real<lower=0> scaling_factor; // Scales variance of structured spatial effects
}

parameters {
  real beta_0; // Intercept
  vector[n] u; // Structured spatial effects
  vector[n] v; // Unstructured spatial effects
  real<lower=0, upper=1> pi; // Proportion unstructured vs. structured variance
  real<lower=0> sigma_phi; // Standard deviation of spatial effects
}

transformed parameters {
  real tau_phi = 1 / sigma_phi^2; // Precision of spatial effects
  vector[n] phi = sqrt(1 - pi) * v + sqrt(pi / scaling_factor) * u; // Spatial effects
}

model {
  y ~ binomial_logit(m, beta_0 + sigma_phi * phi);
  u ~ multi_normal(mu, Sigma); 
  v ~ normal(0, 1);
  pi ~ beta(0.5, 0.5); 
  beta_0 ~ normal(-2, 1);
  sigma_phi ~ normal(0, 2.5); // Weakly informative prior
}

generated quantities {
  vector[n] rho = inv_logit(beta_0 + sigma_phi * phi);
}
