# Model 6: Indep + MVN

m6_stan <- function(save_name = "stan_fit6", save = FALSE){
  
  # Create covariance matrix based on centroid distances
  cent <- st_centroid(df$geometry)
  dist <- st_distance(cent, cent)
  cov <- apply(dist, c(1, 2), FUN = matern, l = 2, nu = 1.5) # No hyper-parameter tuning
  
  # Currently having divergent transition issues (hence chaging adapt_delta) - to be investigated further
  dat <- list(n = nrow(df),
              y = round(df$y),
              m = df$n_obs,
              Sigma = cov,
              mu = rep(0, nrow(df)),
              scaling_factor = 1) # Think the scaling factor for cov is 1 equally could be omitted
  
  fit <- stan(file = "stan/model6.stan", 
              data = dat, 
              warmup = nsim_warm, 
              iter = nsim_iter,
              control = list(adapt_delta = 0.99))
  
  if(save) saveRDS(fit, paste0("results/", survey, "/", save_name, ".rds"))
  
  return(fit)
}

m6_inla <- function(save_name = "inla_fit6", save = FALSE){
  
  # Create covariance matrix based on centroid distances
  cent <- st_centroid(df$geometry)
  dist <- st_distance(cent, cent)
  cov <- apply(dist, c(1, 2), FUN = matern, l = 2, nu = 1.5) # No hyper-parameter tuning
  C <- Matrix::solve(cov) # Precision version of cov for INLA
  
  dat <- list(id = 1:nrow(df),
              y = round(df$y),  
              m = df$n_obs)
  
  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2), 
                                initial = 0, fixed = FALSE))
  
  # pi ~ Beta(0.5, 0.5); inital in terms of logit(pi), so 0 corresponds to pi = 0.5
  pi_prior <- list(phi = list(prior = "logitbeta", param = c(0.5, 0.5), 
                              initial = 0, fixed = FALSE))
  
  formula <- y ~ 1 + f(id) # To-do
}

m6 <- list(m6_stan = m6_stan, m6_inla = m6_inla)
