#' Fit BYM2 Small Area Estimation model using `stan`.
#'
#' @inheritParams m1_stan
#' @examples
#' m3_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' @export
m3_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000, method = "default"){
  
  ii_obs <- which(!is.na(sf$y))
  ii_mis <- which(is.na(sf$y))
  n_obs <- length(ii_obs)
  n_mis <- length(ii_mis)
  
  nb <- neighbours(sf)
  Q <- nb_to_precision(nb)
  scale <- get_scale(Q)
  
  if(method == "default") {
    warning("Currently broken, trying to invert non-inevitable matrix!")
    Qscaled <- Q / scale
    Sigma <- Matrix::solve(Qscaled)
    
    dat <- list(n_obs = n_obs,
                n_mis = n_mis,
                ii_obs = array(ii_obs),
                ii_mis = array(ii_mis),
                n = nrow(sf),
                y_obs = sf$y[ii_obs],
                m = sf$n_obs,
                mu = rep(0, nrow(sf)),
                Sigma = Sigma,
                scaling_factor = scale)
    
    fit <- rstan::sampling(stanmodels$model4to6,
                           data = dat,
                           warmup = nsim_warm,
                           iter = nsim_iter)
  }
  
  if(method == "morris") {
    warning("Doesn't non-connectedness into account!")
    g <- nb_to_graph(nb)
    
    dat <- list(n_obs = n_obs,
                n_mis = n_mis,
                ii_obs = array(ii_obs),
                ii_mis = array(ii_mis),
                n = nrow(sf),
                y_obs = sf$y[ii_obs],
                m = sf$n_obs,
                n_edges = g$n_edges,
                node1 = g$node1,
                node2 = g$node2,
                scaling_factor = scale)
    
    fit <- rstan::sampling(stanmodels$model2,
                           data = dat,
                           warmup = nsim_warm,
                           iter = nsim_iter)
  }
  
  return(fit)
}

#' Fit BYM2 Small Area Estimation model using `R-INLA`.
#'
#' @inheritParams m1_inla
#' @examples
#' m3_inla(mw)
#' @export
m3_inla <- function(sf){

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
                    family = "binomial",
                    control.family = list(control.link = list(model = "logit")),
                    data = dat,
                    Ntrials = m,
                    control.predictor = list(compute = TRUE, link = 1),
                    control.compute = list(dic = TRUE, waic = TRUE,
                                           cpo = TRUE, config = TRUE))

  return(fit)
}

#' Fit BYM2 Small Area Estimation model using `TMB`.
#'
#' @inheritParams m1_tmb
#' @examples
#' m3_tmb(mw, its = 100)
