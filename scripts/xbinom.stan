functions {
  real xbinomial_lpdf(real y, real m, real rho) {
    return(lchoose(m, y) + y * log(rho) + (m - y) * log(1 - rho));
  }
}

data {
  real rho;
  real m;
}

parameters {
  real<lower=0> y;
}

model {
  y ~ xbinomial(m, rho); 
}
