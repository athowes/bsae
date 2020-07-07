// model5.stan: MVN

data {
  int<lower=1> n; // Number of regions
  int y[n]; // Vector of responses
  int m[n]; // Vector of sample sizes
  vector[n] mu; // Prior mean vector
  matrix[n, n] Sigma; // Prior covariance matrix (avoid double validation)
}

parameters {
  real beta_0; // Intercept
  vector[n] phi; // Spatial effects
  real<lower=0> sigma_phi; // Standard deviation of spatial effects
}

transformed parameters {
  real tau_phi = 1 / sigma_phi^2; // Precision of spatial effects
}

model {
  y ~ binomial_logit(m, beta_0 + sigma_phi * phi);
  phi ~ multi_normal(mu, Sigma);
  beta_0 ~ normal(-2, 1);
  sigma_phi ~ normal(0, 2.5); // Weakly informative prior
}

generated quantities {
  vector[n] rho = inv_logit(beta_0 + sigma_phi * phi);
}
