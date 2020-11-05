// model0.stan: Constant with CV

// data {
//   int<lower=0> n_obs; // Number of observed regions
//   int<lower=0> n_mis; // Number of missing regions
//   int<lower=0> n; // Number of regions n_obs + n_mis
//   vector[n_obs] y_obs; // Vector of observed responses
//   vector[n] m; // Vector of sample sizes
// }
// 
// parameters {
//   vector[n_mis] y_mis; // Vector of missing responses
//   real beta_0; // Intercept
// }
// 
// transformed parameters {
//   vector[n] eta = beta_0;
// }
// 
// model {
//   y ~ binomial_logit(m, eta);
//   beta_0 ~ normal(-2, 1);
// }
// 
// generated quantities {
//   vector[n] rho = inv_logit(eta);
//   vector[n] log_lik;
//   for (i in 1:n) {
//     log_lik[i] = binomial_logit_lpmf(y[i] | m[i], eta[i]);
//   }
// }
