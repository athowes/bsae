#' Fit Centroid MVN Small Area Estimation model using `stan`.
#'
#' Random effects have a multivariate Gaussian distribution with covariance
#' matrix calculated using [`centroid_covariance`]. Kernel hyper-parameters
#' are fixed.
#'
#' @inheritParams m1_stan
#' @inheritParams centroid_covariance
#' @examples
#' m5_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
m5_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000, kernel = matern,
                    ...){

  cov <- centroid_covariance(sf, kernel, ...)
  cov <- cov / riebler_gv(cov) # Standardise so tau prior is right

  dat <- list(n = nrow(sf),
              y = round(sf$y),
              m = sf$n_obs,
              mu = rep(0, nrow(sf)),
              Sigma = cov)

  fit <- rstan::sampling(stanmodels$model4to6,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)

  return(fit)
}

#' Fit Centroid MVN Small Area Estimation model using `R-INLA`.
#'
#' Random effects have a multivariate Gaussian distribution with covariance
#' matrix calculated using [`centroid_covariance`].
#'
#' @inheritParams m1_inla
#' @inheritParams centroid_covariance
#' @examples
#' m5_inla(mw)
#' @export
m5_inla <- function(sf, kernel = matern, ...){

  cov <- centroid_covariance(sf, kernel, ...)
  cov <- cov / riebler_gv(cov) # Standardise so tau prior is right
  C <- Matrix::solve(cov) # Precision matrix

  dat <- list(id = 1:nrow(sf),
              y = round(sf$y),
              m = sf$n_obs)

  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2),
                                initial = 0, fixed = FALSE))

  # See inla.doc("generic0")
  formula <- y ~ 1 + f(id, model = "generic0", Cmatrix = C, hyper = tau_prior)

  fit <- INLA::inla(formula,
                    family = "binomial",
                    control.family = list(control.link = list(model = "logit")),
                    data = dat,
                    Ntrials = m,
                    control.predictor = list(compute = TRUE, link = 1),
                    control.compute = list(dic = TRUE, waic = TRUE,
                                           cpo = TRUE, config = TRUE))

  return(fit)
}