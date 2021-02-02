#' Fit constant Small Area Estimation model using `stan`.
#'
#' Simply fits a constant (the mean). This is useful as a benchmark
#' for other models.
#'
#' @param sf A simple features object with some geometry.
#' @param nsim_warm Number of warmup samples, passed to `stan`.
#' @param nsim_iter Number of samples, passed to `stan`.
#' @examples
#' m0_stan(mw)
#' @export
m0_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000){
  
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
              m = sf$n_obs)

  fit <- rstan::sampling(stanmodels$constant,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)
  
  return(fit)
}

#' Fit constant Small Area Estimation model using `R-INLA`.
#'
#' Simply fits a constant (the mean). This is useful as a benchmark
#' for other models.
#'
#' @inheritParams m1_inla
#' @examples
#' fit <- m0_inla(mw)
#' mean <- logistic(fit$summary.fixed$mean)
#' @export
m0_inla <- function(sf, verbose = FALSE){
  
  dat <- list(id = 1:nrow(sf),
              y = sf$y,
              m = sf$n_obs)
  
  formula <- y ~ 1
  
  fit <- INLA::inla(formula,
                    family = "xbinomial",
                    control.family = list(control.link = list(model = "logit")),
                    data = dat,
                    Ntrials = m, # Picks up the correct column in the dataframe dat
                    control.predictor = list(compute = TRUE, link = 1),
                    control.compute = list(dic = TRUE, waic = TRUE,
                                           cpo = TRUE, config = TRUE),
                    verbose = verbose)
  
  return(fit)
}