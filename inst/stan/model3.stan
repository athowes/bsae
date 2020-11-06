// model3.stan: BYM2

functions {
  real xbinomial_logit_lpdf(real y, real m, real eta) {
    return(y * log(inv_logit(eta)) + (m - y) * log(1 - inv_logit(eta)));
  }
}

data {
  int<lower=1> n; // Number of regions
  vector[n] y; // Vector of responses
  vector[n] m; // Vector of sample sizes

  // Data structure for graph input
  int<lower=1> n_edges;
  int<lower=1, upper=n> node1[n_edges];
  int<lower=1, upper=n> node2[n_edges];

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
  vector[n] phi = sqrt(1 - pi) * v + sqrt(pi / scaling_factor) * u; // Spatial effects
  vector[n] eta = beta_0 + sigma_phi * phi;
}

model {
  for(i in 1:n) {
   y[i] ~ xbinomial_logit(m[i], eta[i]); 
  }

  target += -0.5 * dot_self(u[node1] - u[node2]); // Spatial prior when sigma_phi = 1
  // i.e. this is the covariance matrix we compute the GV of when scaling
  sum(u) ~ normal(0, 0.001 * n); // Soft sum-to-zero constraint

  v ~ normal(0, 1);
  pi ~ beta(0.5, 0.5);
  beta_0 ~ normal(-2, 1);
  sigma_phi ~ normal(0, 2.5); // Weakly informative prior
}

generated quantities {
  real tau_phi = 1 / sigma_phi^2; // Precision of spatial effects
  vector[n] rho = inv_logit(beta_0 + sigma_phi * phi);
  vector[n] log_lik;
  for (i in 1:n) {
    log_lik[i] = xbinomial_logit_lpdf(y[i] | m[i], eta[i]);
  }
}
