# Model 4: BYM2

m4_stan <- function(save_name = "stan_fit4", save = FALSE){
  
  # Stan pairwise Besag/ICAR implementation
  nb <- spdep::poly2nb(df)
  g <- nb_to_graph(nb)
  
  dat <- list(n = nrow(df),
              y = round(df$y),  
              m = df$n_obs,
              n_edges = g$n_edges,
              node1 = g$node1,
              node2 = g$node2,
              scaling_factor = get_scale(nb))
  
  fit <- stan(file = "stan/model4.stan", 
              data = dat, 
              warmup = nsim_warm, 
              iter = nsim_iter)
  
  if(save) saveRDS(fit, paste0("results/", survey, "/", save_name, ".rds"))
  
  return(fit)
}

m4_inla <- function(save_name = "inla_fit4", save = FALSE){
  
  nb <- spdep::poly2nb(df)
  
  dat <- list(id = 1:nrow(df),
              y = round(df$y),  
              m = df$n_obs)
  
  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2), 
                                initial = 0, fixed = FALSE))
  
  # pi ~ Beta(0.5, 0.5); inital in terms of logit(pi), so 0 corresponds to pi = 0.5
  pi_prior <- list(phi = list(prior = "logitbeta", param = c(0.5, 0.5), 
                              initial = 0, fixed = FALSE))
  
  # Should constr = TRUE here? Does it know to apply to the improper structured part
  formula <- y ~ 1 + f(id, 
                       model = "bym2", 
                       graph = nb, 
                       scale.model = TRUE, 
                       constr = TRUE, 
                       hyper = list(tau_prior, pi_prior))
  
  fit <- inla(formula, 
              family = "binomial",
              control.family = list(control.link = list(model = "logit")),
              data = dat, 
              Ntrials = m,
              control.predictor = list(compute = TRUE, link = 1),
              control.compute = list(dic = TRUE, waic = TRUE, 
                                     cpo = TRUE, config = TRUE))
  
  if(save) saveRDS(fit, paste0("results/", survey, "/", save_name, ".rds"))
  
  return(fit)
}

m4 <- list(m4_stan = m4_stan, m4_inla = m4_inla)