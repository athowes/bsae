#' Fit Bayesian Centroid MVN Small Area Estimation model using `stan`.
#'
#' Random effects have a multivariate Gaussian distribution with covariance
#' matrix calculated using [`centroid_covariance`]. Unlike `m5_stan`, this
#' version has a fully Bayesian treatment of kernel hyperparameters.
#'
#' @inheritParams m1_stan
#' @examples
#' m7_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
m7_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000){

  D <- centroid_distance(sf)

  dat <- list(n = nrow(sf),
              y = round(sf$y),
              m = sf$n_obs,
              mu = rep(0, nrow(sf)),
              D = D)

  fit <- rstan::sampling(stanmodels$model7,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)

  return(fit)
}