# Model 3: Border ICAR

m3_inla <- function(save_name = "inla_fit3", save = FALSE){

  # Precision based on shared border lengths (INLA sparse version)
  Q <- border_precision(df) %>% inla.as.dgTMatrix() 
  
  dat <- list(id = 1:nrow(df),
              y = round(df$y),  
              m = df$n_obs)
  
  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2), 
                                initial = 0, fixed = FALSE))
  
  formula <- y ~ 1 + f(id, 
                       model = "generic0", 
                       Cmatrix = Q, 
                       hyper = tau_prior) # See inla.doc("generic0")
  
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

m3 <- list(m3_inla = m3_inla)