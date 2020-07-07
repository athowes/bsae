// model1dev.stan

functions {
  real gen_binomial_lpdf (vector n, real N, vector theta) {
    return sum(n * log(theta) + (N - n) * log(1 - theta)); // Why is this giving me an error? :(
  }
  // To fix..
}

// Data block
data {
  int<lower=1> n; // Number of regions
  int<lower=1> n_obs; // Number of regions observed
  int<lower=1> n_mis; // Number of regions unobserved (n_obs + n_mis = n)

  int m[n]; // Vector of sample sizes
  // Could be that in the unobserved case one would still know the number at risk
  
  int<lower=0> y_obs[n_obs]; // Vector of observed responses
}

// The parameters accepted by the model
parameters {
  vector[n_mis] y_mis; // Vector of missing responses, treated as parameters
  
  real beta_0; // Intercept
  vector[n] phi; // Spatial effects
  real<lower=0> sigma_phi; // Standard deviation of spatial effects
}

transformed parameters {
  real<lower=0> tau_phi = inv_square(sigma_phi); // Precision of spatial effects
  vector[n] eta = beta_0 + sigma_phi * phi; // Linear predictor
  vector<lower=0, upper = 1>[n] rho = inv_logit(eta); // Inverse logit transformation
}

// The model to be estimated
model {
  y ~ binomial_logit(m, eta);
  
  phi ~ normal(0, 1); // phi has variance one
  beta_0 ~ normal(-2, 1);
  sigma_phi ~ normal(0, 2.5); // Weakly informative prior
}

// Generated quantities
generated quantities {
  vector[n] rho = inv_logit(beta_0 + sigma_phi * phi);
}
