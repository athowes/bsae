// model1.stan: IID

// functions {
//   real gen_binomial_lpdf (vector n, real N, vector theta) {
//     return sum(n * log(theta) + (N - n) * log(1 - theta)); // Why is this giving me an error?
//   }
//   // To fix..
// }

data {
  int<lower=1> n; // Number of regions
  int y[n]; // Vector of responses
  int m[n]; // Vector of sample sizes
}

parameters {
  real beta_0; // Intercept
  vector[n] phi; // Spatial effects
  real<lower=0> sigma_phi; // Standard deviation of spatial effects
}

transformed parameters {
  real tau_phi = 1 / sigma_phi^2; // Precision of spatial effects
  vector[n] eta = beta_0 + sigma_phi * phi;
}

model {
  y ~ binomial_logit(m, eta);
  phi ~ normal(0, 1); // phi has variance one
  beta_0 ~ normal(-2, 1);
  sigma_phi ~ normal(0, 2.5); // Weakly informative prior
}

generated quantities {
  vector[n] rho = inv_logit(beta_0 + sigma_phi * phi);
}