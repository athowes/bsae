#' Fit Bayesian Integrated MVN Small Area Estimation model using `rstan`.
#'
#' @inheritParams constant_stan
#' @inheritParams integrated_covariance
#' @param bym2 Logical indicating if the spatial random effects should be convoluted 
#' with unstructured IID noise, defaults to `FALSE`.
#' @examples
#' ik_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
ik_stan <- function(sf, bym2 = FALSE, L = 10, type = "hexagonal", nsim_warm = 100, nsim_iter = 1000, chains = 4, cores = parallel::detectCores()){
  
  n <- nrow(sf)
  samples <- sf::st_sample(sf, type = type, exact = TRUE, size = rep(L, n))
  S <- sf::st_distance(samples, samples)
  
  # Data structure for unequal number of points in each area
  sample_index <- sf::st_intersects(sf, samples)
  sample_lengths <- lengths(sample_index)
  start_index <- sapply(sample_index, function(x) x[1])

  ii_obs <- which(!is.na(sf$y))
  ii_mis <- which(is.na(sf$y))
  n_obs <- length(ii_obs)
  n_mis <- length(ii_mis)
  
  dat <- list(n_obs = n_obs,
              n_mis = n_mis,
              ii_obs = array(ii_obs),
              ii_mis = array(ii_mis),
              n = nrow(sf),
              y_obs = sf$y[ii_obs],
              m = sf$n_obs,
              mu = rep(0, nrow(sf)),
              sample_lengths = sample_lengths,
              total_samples = sum(sample_lengths),
              start_index = start_index,
              S = S)
  
  if(bym2){
    fit <- rstan::sampling(stanmodels$bym2_integrated,
                           data = dat,
                           warmup = nsim_warm,
                           iter = nsim_iter,
                           chains = chains,
                           cores = cores)
  }
  else{
    fit <- rstan::sampling(stanmodels$integrated,
                           data = dat,
                           warmup = nsim_warm,
                           iter = nsim_iter,
                           chains = chains,
                           cores = cores)
  }
  
  return(fit)
}