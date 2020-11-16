// Fast disconnected CAR

data {
  int<lower=1> n; // Number of regions
  int y[n]; // Vector of responses
  int m[n]; // Vector of sample sizes
  
  // Data structure for connected components input
  int<lower=1> nc;
  int group_sizes[nc];    
  vector[n] scales;
  
  // Data structure for graph input
  int<lower=1> n_edges;
  int<lower=1, upper=n> node1[n_edges];
  int<lower=1, upper=n> node2[n_edges];
}

parameters {
  real beta_0; // Intercept
  vector[n] u; // Unscaled spatial effects
  real<lower=0> sigma_phi; // Standard deviation of spatial effects
}

transformed parameters {
  real tau_phi = 1 / sigma_phi^2; // Precision of spatial effects
}

model {
  int pos;
  pos = 1;
  for (k in 1:nc) {
    if(group_sizes[k] > 1){
      // Soft sum-to-zero constraint on each connected component
      sum(segment(u, pos, group_sizes[k])) ~ normal(0, 0.001 * n);
    }
    pos = pos + group_sizes[k];
  }
  
  target += -0.5 * dot_self(u[node1] - u[node2]); // Unit spatial prior
  
  y ~ binomial_logit(m, beta_0 + sigma_phi * sqrt(1 ./ scales) .* u);
  beta_0 ~ normal(-2, 1);
  sigma_phi ~ normal(0, 2.5); // Weakly informative prior
}

generated quantities {
  vector[n] rho = inv_logit(beta_0 + sigma_phi * sigma_phi * sqrt(1 ./ scales) .* u);
}
