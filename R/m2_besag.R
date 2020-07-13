#' Fit Besag Small Area Estimation model using \code{stan}.
#'
#' Random effects have an improper conditional autoregressive (ICAR)
#' distribution with (generalised) precision matrix produced using
#' the \code{\link{nb_to_precision}} function with input \code{nb},
#' the neighbourhood structure of \code{sf}.
#'
#' @inheritParams m1_stan
#' @examples
#' m2_stan(mw, nsim_warm = 0, nsim_iter = 100)
m2_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000){

  nb <- neighbours(sf)

  # Stan pairwise Besag/ICAR implementation based upon graph object
  g <- nb_to_graph(nb)

  dat <- list(n = nrow(sf),
              y = round(sf$y),
              m = sf$n_obs,
              n_edges = g$n_edges,
              node1 = g$node1,
              node2 = g$node2,
              scaling_factor = get_scale(nb))

  fit <- rstan::sampling(stanmodels$model2,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)

  return(fit)
}

#' Fit Besag Small Area Estimation model using \code{R-INLA}.
#'
#' Random effects have an improper conditional autoregressive (ICAR)
#' distribution. This is implemented by \code{R-INLA} using the
#' option \code{model = "besag"}.
#'
#' @inheritParams m1_inla
#' @examples
#' m2_inla(mw)
m2_inla <- function(sf){

  nb <- neighbours(sf)

  dat <- list(id = 1:nrow(sf),
              y = round(sf$y),
              m = sf$n_obs)

  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2),
                                initial = 0, fixed = FALSE))

  # constr = TRUE is a sum-to-zero constraint else +/- constant to all leaves density unchanged
  formula <- y ~ 1 + f(id,
                       model = "besag",
                       graph = nb,
                       scale.model = TRUE,
                       constr = TRUE,
                       hyper = tau_prior)

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

#' #' Fit Besag Small Area Estimation model using \code{TMB}.
#' #'
#' #' Random effects have an improper conditional autoregressive (ICAR)
#' #' distribution.
#' #'
#' #' @param sf A simple features object with some geometry.
#' #' @param its Number of iterations in outer loop optimisation, passed to
#' #' \code{nlminb}.
#' #' @examples
#' #' m2_tmb(mw, its = 100)
#' m2_tmb <- function(sf, its = 1000){
#'
#'   compile("tmb/model2.cpp")
#'   dyn.load(dynlib("tmb/model2"))
#'
#'   nb <- neighbours(sf)
#'   Q <- nb_to_precision(nb)
#'
#'   dat <- list(n = nrow(sf),
#'               y = round(sf$y),
#'               m = sf$n_obs,
#'               Q = Q)
#'
#'   param <- list(beta_0 = 0,
#'                 phi = rep(0, dat$n),
#'                 l_sigma_phi = 0)
#'
#'   obj <- MakeADFun(data = dat,
#'                    parameters = param,
#'                    DLL = "model2")
#'
#'   opt <- nlminb(start = obj$par,
#'                 objective = obj$fn,
#'                 gradient = obj$gr,
#'                 control = list(iter.max = its, trace = 0))
#'
#'   sd_out <- sdreport(obj,
#'                      par.fixed = opt$par,
#'                      getJointPrecision = TRUE)
#'
#'   return(sd_out)
#' }
