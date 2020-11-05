// model0.stan: Constant

functions {
  real gen_binomial_lpdf(real y, real m, real rho) {
    return(y * log(rho) + (m - y) * log(1 - rho));
  }
}

data {
  int<lower=1> n; // Number of regions
  vector[n] y; // Vector of responses
  vector[n] m; // Vector of sample sizes
}

parameters {
  real beta_0; // Intercept
}

transformed parameters {
  vector[n] eta = rep_vector(beta_0, n);
  vector[n] rho = inv_logit(eta);
}

model {
  for(i in 1:n) {
   y[i] ~ gen_binomial(m[i], rho[i]); 
  }
  beta_0 ~ normal(-2, 1);
}

generated quantities {
  vector[n] log_lik;
  for (i in 1:n) {
    log_lik[i] = gen_binomial_lpdf(y[i] | m[i], rho[i]);
  }
}
