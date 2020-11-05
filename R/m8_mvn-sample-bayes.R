#' Fit Bayesian Sampling MVN Small Area Estimation model using `stan`.
#'
#' @inheritParams m1_stan
#' @inheritParams sampling_covariance
#' @examples
#' m8_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
m8_stan <- function(sf, L = 50, nsim_warm = 100, nsim_iter = 1000){
  
  n <- nrow(sf)
  samples <- sf::st_sample(sf, size = rep(L, n))
  S <- sf::st_distance(samples, samples)
  
  dat <- list(n = n,
              y = sf$y,
              m = sf$n_obs,
              mu = rep(0, nrow(sf)),
              L = L,
              S = S)
  
  fit <- rstan::sampling(stanmodels$model8,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)
  
  return(fit)
}