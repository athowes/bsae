#' Fit BYM2 Small Area Estimation model using \code{stan}.
#'
#' @inheritParams m1_stan
#' @examples
#' m3_stan(mw, nsim_warm = 0, nsim_iter = 100)
m3_stan <- function(sf, nsim_warm = 100, nsim_iter = 1000){

  # Stan pairwise Besag implementation
  nb <- spdep::poly2nb(df)
  g <- nb_to_graph(nb)

  dat <- list(n = nrow(df),
              y = round(df$y),
              m = df$n_obs,
              n_edges = g$n_edges,
              node1 = g$node1,
              node2 = g$node2,
              scaling_factor = get_scale(nb))

  fit <- rstan::sampling(stanmodels$model3,
                         data = dat,
                         warmup = nsim_warm,
                         iter = nsim_iter)

  return(fit)
}

#' Fit BYM2 Small Area Estimation model using \code{R-INLA}.
#'
#' @inheritParams m1_inla
#' @examples
#' m3_inla(mw)
m3_inla <- function(sf){

  nb <- neighbours(df)

  dat <- list(id = 1:nrow(df),
              y = round(df$y),
              m = df$n_obs)

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

#' Fit BYM2 Small Area Estimation model using \code{TMB}.
#'
#' @param sf A simple features object with some geometry.
#' @param its Number of iterations in outer loop optimisation, passed to
#' \code{nlminb}.
#' @examples
#' m3_tmb(mw, its = 100)
