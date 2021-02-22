#' Fit IID Small Area Estimation model using `stan`.
#'
#' Random effects are independent and identically distributed.
#'
#' @inheritParams constant_stan
#' @examples
#' iid_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
iid_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000){

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

  fit <- rstan::sampling(stanmodels$iid,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)

  return(fit)
}

#' Fit IID Small Area Estimation model using `R-INLA`.
#'
#' Random effects are independent and identically distributed.
#'
#' @inheritParams constant_inla
#' @examples
#' iid_inla(mw)
#' @export
iid_inla <- function(sf, verbose = FALSE){

  dat <- list(id = 1:nrow(sf),
              y = sf$y,
              m = sf$n_obs)

  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2),
                                initial = 0, fixed = FALSE))

  formula <- y ~ 1 + f(id, model = "iid", hyper = tau_prior)

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

#' #' Fit IID Small Area Estimation model using `TMB`.
#' #'
#' #' Random effects are independent and identically distributed.
#' #'
#' #' @param sf A simple features object with some geometry.
#' #' @param its Number of iterations in outer loop optimisation, passed to
#' #' `nlminb`.
#' #' @examples
#' #' iid_tmb(mw, its = 100)
#' iid_tmb <- function(sf, its = 1000){
#' 
#'   dat <- list(n = nrow(sf),
#'               y = sf$y,
#'               m = sf$n_obs)
#' 
#'   # Initialisation
#'   param <- list(beta_0 = 0,
#'                 phi = rep(0, nrow(sf)),
#'                 l_sigma_phi = 0)
#' 
#'   obj <- MakeADFun(data = dat,
#'                    parameters = param,
#'                    DLL = "model1")
#' 
#'   opt <- nlminb(start = obj$par,
#'                 objective = obj$fn,
#'                 gradient = obj$gr,
#'                 control = list(iter.max = its, trace = 0))
#'   # outer mgc (maximum gradient component)
#' 
#'   # How does TMB know what the optimal parameters without
#'   # sd_report taking them as input?
#'   sd_out <- sdreport(obj,
#'                      par.fixed = opt$par,
#'                      getJointPrecision = TRUE)
#' 
#'   return(sd_out)
#' }