#' Fit BYM2 Small Area Estimation model using `rstan`.
#'
#' @inheritParams constant_stan
#' @param method One of `"default"` or `"morris"`.
#' @examples
#' bym2_stan(mw, nsim_warm = 0, nsim_iter = 100, cores = 2)
#' @export
bym2_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000, chains = 4, cores = parallel::detectCores(), method = "default"){
  
  warning("Doesn't take non-connectedness into account correctly!")
  
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
    
    fit <- rstan::sampling(stanmodels$bym2_precision,
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
    
    fit <- rstan::sampling(stanmodels$bym2_morris,
                           data = dat,
                           warmup = nsim_warm,
                           iter = nsim_iter,
                           chains = chains,
                           cores = cores)
  }
  
  return(fit)
}

#' Fit BYM2 Small Area Estimation model using `R-INLA`.
#'
#' @inheritParams constant_inla
#' @examples
#' bym2_inla(mw)
#' @export
bym2_inla <- function(sf, verbose = FALSE){

  m <- NULL
  
  nb <- neighbours(sf)

  dat <- list(id = 1:nrow(sf),
              y = sf$y,
              m = sf$n_obs)

  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2),
                                initial = 0, fixed = FALSE))

  # pi ~ Beta(0.5, 0.5); inital in terms of logit(pi), so 0 corresponds to pi = 0.5
  pi_prior <- list(phi = list(prior = "logitbeta", param = c(0.5, 0.5),
                              initial = 0, fixed = FALSE))

  formula <- y ~ 1 + f(id,
                       model = "bym2",
                       graph = nb,
                       scale.model = TRUE,
                       constr = TRUE,
                       hyper = list(tau_prior, pi_prior))

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
