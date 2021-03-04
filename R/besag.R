#' Fit Besag Small Area Estimation model using `rstan`.
#'
#' Random effects have an improper conditional autoregressive (ICAR)
#' distribution with (generalised) precision matrix produced using
#' the [`nb_to_precision`] function with input `nb`,
#' the neighbourhood structure of `sf`.
#'
#' @inheritParams constant_stan
#' @param method One of `"default"` or `"morris"`.
#' @examples
#' besag_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
besag_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000, chains = 4, cores = parallel::detectCores(), method = "default"){

  ii_obs <- which(!is.na(sf$y))
  ii_mis <- which(is.na(sf$y))
  n_obs <- length(ii_obs)
  n_mis <- length(ii_mis)
  
  nb <- neighbours(sf)
  Q <- nb_to_precision(nb)
  scale <- get_scale(Q)
  
  dat <- list(n_obs = n_obs,
              n_mis = n_mis,
              ii_obs = array(ii_obs),
              ii_mis = array(ii_mis),
              n = nrow(sf),
              y_obs = sf$y[ii_obs],
              m = sf$n_obs)
  
  if(method == "default") {
    
    warning("Does not impose correct constraints!")  
    
    Q_scaled <- scale_gmrf_precision(Q)$Q
    
    dat <- rlist::list.append(dat,
                              mu = rep(0, nrow(sf)),
                              Q = Q_scaled
    ) 

    fit <- rstan::sampling(stanmodels$mvn_precision,
                           data = dat,
                           warmup = nsim_warm,
                           iter = nsim_iter,
                           chains = chains,
                           cores = cores)
  }
  
  if(method == "morris") {
    
  warning("Assumes that the adjacency graph is connected!")  
  
  g <- nb_to_graph(nb)

  dat <- rlist::list.append(dat,
                            n_edges = g$n_edges,
                            node1 = g$node1,
                            node2 = g$node2,
                            scaling_factor = scale
  ) 
  
  fit <- rstan::sampling(stanmodels$besag_morris,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter,
                         chains = chains,
                         cores = cores)
  }
  
  return(fit)
}

#' Fit Besag Small Area Estimation model using `R-INLA`.
#'
#' Random effects have an improper conditional autoregressive (ICAR)
#' distribution. This is implemented by `R-INLA` using the
#' option `model = "besag"`.
#'
#' @inheritParams constant_inla
#' @examples
#' besag_inla(mw)
#' @export
besag_inla <- function(sf, verbose = FALSE){

  nb <- neighbours(sf)

  dat <- list(id = 1:nrow(sf),
              y = sf$y,
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

#' #' Fit Besag Small Area Estimation model using `TMB`.
#' #'
#' #' Random effects have an improper conditional autoregressive (ICAR)
#' #' distribution.
#' #'
#' #' @param sf A simple features object with some geometry.
#' #' @param its Number of iterations in outer loop optimisation, passed to
#' #' \code{nlminb}.
#' #' @examples
#' #' besag_tmb(mw, its = 100)
#' besag_tmb <- function(sf, its = 1000){
#'
#'   compile("tmb/model2.cpp")
#'   dyn.load(dynlib("tmb/model2"))
#'
#'   nb <- neighbours(sf)
#'   Q <- nb_to_precision(nb)
#'
#'   dat <- list(n = nrow(sf),
#'               y = sf$y,
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
