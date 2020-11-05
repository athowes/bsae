// model0.stan: Constant

data {
  int<lower=1> n; // Number of regions
  int y[n]; // Vector of responses
  int m[n]; // Vector of sample sizes
}

parameters {
  real beta_0; // Intercept
}

transformed parameters {
  vector[n] eta = rep_vector(beta_0, n);
}

model {
  y ~ binomial_logit(m, eta);
  beta_0 ~ normal(-2, 1);
}

generated quantities {
  vector[n] rho = inv_logit(eta);
  vector[n] log_lik;
  for (i in 1:n) {
    log_lik[i] = binomial_logit_lpmf(y[i] | m[i], eta[i]);
  }
}
