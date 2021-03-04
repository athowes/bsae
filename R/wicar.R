#' Fit weighted ICAR Small Area Estimation model using `rstan`.
#'
#' @inheritParams constant_stan
#' @examples
#' wicar_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
wicar_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000, chains = 4, cores = parallel::detectCores()){

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
              Q = border_precision(sf),
              mu = rep(0, nrow(sf)))

  fit <- rstan::sampling(stanmodels$mvn_precision,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter,
                         chains = chains,
                         cores = cores)

  return(fit)
}

#' Fit weighted ICAR Small Area Estimation model using `R-INLA`.
#'
#' @inheritParams constant_inla
#' @examples
#' wicar_inla(mw)
#' @export
wicar_inla <- function(sf, verbose = FALSE){

  m <- NULL
  
  C <- border_precision(sf)
  C <- scale_gmrf_precision(C)$Q # Could use scale.model = TRUE in f() instead?
  
  dat <- list(id = 1:nrow(sf),
              y = sf$y,
              m = sf$n_obs)

  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2),
                                initial = 0, fixed = FALSE))

  # See inla.doc("generic0")
  formula <- y ~ 1 + f(id, 
                       model = "generic0", 
                       Cmatrix = C,
                       constr = TRUE,
                       hyper = tau_prior)

  fit <- INLA::inla(formula,
                    family = "xbinomial",
                    control.family = list(control.link = list(model = "logit")),
                    data = dat,
                    Ntrials = m,
                    control.predictor = list(compute = TRUE, link = 1),
                    control.compute = list(dic = TRUE, waic = TRUE,
                                           cpo = TRUE, config = TRUE),
                    verbose = verbose)

  return(fit)
}
