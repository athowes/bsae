// model2.cpp: ICAR

#include <TMB.hpp>

template <class Type>
Type objective_function<Type>::operator()()
{
  // Data block
  DATA_INTEGER(n); // Number of regions
  DATA_VECTOR(y); // Vector of responses
  DATA_VECTOR(m); // Vector of sample sizes
  DATA_MATRIX(Q); // Precision matrix
  
  // Parameter block
  PARAMETER(beta_0); // Intercept
  PARAMETER_VECTOR(phi); // Spatial effects
  PARAMETER(sigma_phi); // Standard deviation of spatial effects
  
  // Transformed parameters block
  vector<Type> eta(beta_0 + sigma_phi * phi);
  vector<Type> rho(invlogit(eta));
  
  // Initialise negative log-likelihood
  Type nll;
  nll = Type(0.0);
  
  // Likelihood from priors
  nll -= dnorm(sigma_phi, Type(0), Type(100), true); // Approximating the uniform prior
  nll -= dnorm(beta_0, Type(-2), Type(5), true); // NB: true puts the likelihood on the log-scale
  
  nll -= Type(-0.5) * (phi * (Q * phi)).sum(); // ICAR prior
  nll -= dnorm(sum(phi), Type(0.0), Type(0.001) * phi.size(), true); // Soft sum-to-zero constraint on phi
  
  for(int i = 0; i < n; i++)
  {
    nll -= dnorm(phi[i], Type(0), sigma_phi, true);
    nll -= dbinom_robust(y[i], m[i], eta[i], true);
  }
  
  ADREPORT(rho); // Would like to see posterior prevalence estimates
  
  return(nll);
}
