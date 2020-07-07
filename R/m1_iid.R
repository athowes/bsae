#' Fit IID Small Area Estimation model using \code{stan}.
#'
#' Random effects are independent and identically distributed.
#'
#' @param sf A simple features object with some geometry.
#' @param nsim_warm Number of warmup samples, passed to \code{stan}.
#' @param nsim_iter Number of samples, passed to \code{stan}.
#' @examples
#' m1_stan(mw, nsim_warm = 0, nsim_iter = 100)
m1_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000){

  dat <- list(n = nrow(sf),
              y = round(sf$y),
              m = sf$n_obs)

  fit <- rstan::sampling(stanmodels$model1,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)

  return(fit)
}

#' Fit IID Small Area Estimation model using \code{R-INLA}.
#'
#' Random effects are independent and identically distributed.
#'
#' @param sf A simple features object with some geometry.
#' @examples
#' m1_inla(mw)
m1_inla <- function(sf){

  dat <- list(id = 1:nrow(sf),
              y = round(sf$y),
              m = sf$n_obs)

  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2),
                                initial = 0, fixed = FALSE))

  formula <- y ~ 1 + f(id, model = "iid", hyper = tau_prior)

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

#' #' Fit IID Small Area Estimation model using \code{TMB}.
#' #'
#' #' Random effects are independent and identically distributed.
#' #'
#' #' @param sf A simple features object with some geometry.
#' #' @param its Number of iterations in outer loop optimisation, passed to
#' #' \code{nlminb}.
#' #' @examples
#' #' m1_tmb(mw, its = 100)
#' m1_tmb <- function(sf, its = 1000){
#'
#'   compile("tmb/model1.cpp")
#'   dyn.load(dynlib("tmb/model1"))
#'
#'   dat <- list(n = nrow(sf),
#'               y = round(sf$y),
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
