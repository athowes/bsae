#' Fit Bayesian Integrated MVN Small Area Estimation model using `stan`.
#'
#' @inheritParams m1_stan
#' @inheritParams sampling_covariance
#' @examples
#' m8_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
m8_stan <- function(sf, L = 50, type = "random", nsim_warm = 100, nsim_iter = 1000){
  
  n <- nrow(sf)
  samples <- sf::st_sample(sf, type = type, size = rep(L, n))
  S <- sf::st_distance(samples, samples)
  
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
              L = L,
              S = S)
  
  fit <- rstan::sampling(stanmodels$integrated,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)
  
  return(fit)
}