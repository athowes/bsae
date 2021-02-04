functions {
  real xbinomial_logit_lpdf(real y, real m, real eta) {
    real dens = lchoose(m, y) + y * log(inv_logit(eta)) + (m - y) * log(1 - inv_logit(eta));
    real jacobian = log(inv_logit(eta)) + log(inv_logit(-eta)); // Alternative: eta - 2 * log(1 + exp(eta))
    return dens + jacobian;
  }
}

data {
  real eta;
  real m;
}

parameters {
  real<lower=0> y;
}

model {
  y ~ xbinomial_logit(m, eta); 
}
