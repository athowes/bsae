// model1.stan: IID

functions {
  real xbinomial_logit_lpdf(real y, real m, real eta) {
    return(y * log(inv_logit(eta)) + (m - y) * log(1 - inv_logit(eta)));
  }
}

data {
  int<lower=1> n; // Number of regions
  vector[n] y; // Vector of responses
  vector[n] m; // Vector of sample sizes
}

parameters {
  real beta_0; // Intercept
  vector[n] phi; // Spatial effects
  real<lower=0> sigma_phi; // Standard deviation of spatial effects
}

transformed parameters {
  vector[n] eta = beta_0 + sigma_phi * phi;
}

model {
  for(i in 1:n) {
   y[i] ~ xbinomial_logit(m[i], eta[i]); 
  }
  phi ~ normal(0, 1); // phi has variance one
  beta_0 ~ normal(-2, 1);
  sigma_phi ~ normal(0, 2.5); // Weakly informative prior
}

generated quantities {
  real tau_phi = 1 / sigma_phi^2; // Precision of spatial effects
  vector[n] rho = inv_logit(eta);
  vector[n] log_lik;
  for (i in 1:n) {
    log_lik[i] = xbinomial_logit_lpdf(y[i] | m[i], beta_0 + sigma_phi * phi[i]);
  }
}
