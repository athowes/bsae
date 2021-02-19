#' Fit Bayesian Centroid MVN Small Area Estimation model using `stan`.
#'
#' Random effects have a multivariate Gaussian distribution with covariance
#' matrix calculated using [`centroid_covariance`]. Unlike `m5_stan`, this
#' version has a fully Bayesian treatment of kernel hyperparameters.
#'
#' @inheritParams constant_stan
#' @param bym2 Logical indicating if the spatial random effects should be convoluted 
#' with unstructured IID noise, defaults to `FALSE`.
#' @examples
#' ck_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
ck_stan <- function(sf, bym2 = FALSE, nsim_warm = 100, nsim_iter = 1000){

  D <- centroid_distance(sf)
  
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
              D = D)

  if(bym2){
    fit <- rstan::sampling(stanmodels$bym2_centroid,
                           data = dat,
                           warmup = nsim_warm,
                           iter = nsim_iter)
  }
  else{
    fit <- rstan::sampling(stanmodels$centroid,
                           data = dat,
                           warmup = nsim_warm,
                           iter = nsim_iter)
  }

  return(fit)
}