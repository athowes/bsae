// model2.stan: ICAR

data {
  int<lower=1> n; // Number of regions
  int y[n]; // Vector of responses
  int m[n]; // Vector of sample sizes
  
  // Data structure for graph input
  int<lower=1> n_edges;
  int<lower=1, upper=n> node1[n_edges];
  int<lower=1, upper=n> node2[n_edges];
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
  
  target += -0.5 * dot_self(phi[node1] - phi[node2]); // Spatial prior with unit variance
  sum(phi) ~ normal(0, 0.001 * n); // Soft sum-to-zero constraint
  
  beta_0 ~ normal(-2, 1);
  sigma_phi ~ normal(0, 2.5); // Weakly informative prior
}

generated quantities {
  vector[n] rho = inv_logit(beta_0 + sigma_phi * phi);
}
