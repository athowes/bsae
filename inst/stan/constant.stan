// constant.stan: Constant plus CV

functions {
  real xbinomial_lpdf(real y, real m, real rho) {
    return(lchoose(m, y) + y * log(rho) + (m - y) * log(1 - rho));
  }
}

data {
  int<lower=0> n_obs; // Number of observed regions
  int<lower=0> n_mis; // Number of missing regions
  
  int<lower = 1, upper = n_obs + n_mis> ii_obs[n_obs];
  int<lower = 1, upper = n_obs + n_mis> ii_mis[n_mis];

  int<lower=0> n; // Number of regions n_obs + n_mis
  vector[n_obs] y_obs; // Vector of observed responses
  vector[n] m; // Vector of sample sizes
}

parameters {
  vector<lower=0>[n_mis] y_mis; // Vector of missing responses
  real beta_0; // Intercept
}

transformed parameters {
  vector[n] eta = rep_vector(beta_0, n);
  vector[n] rho = inv_logit(eta);
  
  vector[n] y;
  y[ii_obs] = y_obs;
  y[ii_mis] = y_mis;
}

model {
  for(i in 1:n) {
   y[i] ~ xbinomial(m[i], rho[i]);
  }
  beta_0 ~ normal(-2, 1);
}

generated quantities {
  vector[n] log_lik;
  for (i in 1:n) {
    log_lik[i] = xbinomial_lpdf(y[i] | m[i], rho[i]);
  }
}
