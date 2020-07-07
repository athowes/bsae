# Model 5: Centroid MVN

  m5_stan <- function(save_name = "stan_fit5", save = FALSE){
  
    # Create covariance matrix based on centroid distances
    cent <- st_centroid(df$geometry)
    dist <- st_distance(cent, cent)
    cov <- apply(dist, c(1, 2), FUN = matern, l = 2, nu = 1.5) # No hyper-parameter tuning
    
    dat <- list(n = nrow(df),
                y = round(df$y),
                m = df$n_obs,
                Sigma = cov,
                mu = rep(0, nrow(df)))
    
    fit <- stan(file = "stan/model5.stan", 
                data = dat, 
                warmup = nsim_warm, 
                iter = nsim_iter)
    
    if(save) saveRDS(fit, paste0("results/", survey, "/", save_name, ".rds"))
    
    return(fit)
  }

m5_inla <- function(save_name = "inla_fit5", save = FALSE){
  
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
  
  # See inla.doc("generic0")
  formula <- y ~ 1 + f(id, model = "generic0", Cmatrix = C, hyper = tau_prior)
  
  fit <- inla(formula, 
              family = "binomial",
              control.family = list(control.link = list(model = "logit")),
              data = dat, 
              Ntrials = m,
              control.predictor = list(compute = TRUE, link = 1),
              control.compute = list(dic = TRUE, waic = TRUE, 
                                     cpo = TRUE, config = TRUE))
  
  if(save) saveRDS(fit, paste0("results/", survey, "/", save_name, ".rds"))
  
  return(fit)
}

m5 <- list(m5_stan = m5_stan, m5_inla = m5_inla)
