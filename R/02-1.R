# Model 1: IID

m1_stan <- function(save_name = "stan_fit1", save = FALSE){
  
  dat <- list(n = nrow(df),
              y = round(df$y), # To fix...
              m = df$n_obs)
  
  fit <- stan(file = "stan/model1.stan", 
              data = dat, 
              warmup = nsim_warm, 
              iter = nsim_iter)
  
  if(save) saveRDS(fit, paste0("results/", survey, "/", save_name, ".rds"))

  return(fit)
}

m1_inla <- function(save_name = "inla_fit1", save = FALSE){
  
  dat <- list(id = 1:nrow(df),
              y = round(df$y),  
              m = df$n_obs)
  
  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2), 
                                initial = 0, fixed = FALSE))
  
  formula <- y ~ 1 + f(id, model = "iid", hyper = tau_prior)
  
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

m1_tmb <- function(save_name = "tmb_fit1", save = FALSE){
  
  compile("tmb/model1.cpp")
  dyn.load(dynlib("tmb/model1"))
  
  dat <- list(n = nrow(df),
              y = round(df$y),  
              m = df$n_obs)
  
  # These are initialisation
  param <- list(beta_0 = 0,
                phi = rep(0, nrow(df)),
                l_sigma_phi = 0)
  
  obj <- MakeADFun(data = dat, 
                   parameters = param,
                   DLL = "model1")
  
  its <- 1000 # May converge before this
  opt <- nlminb(start = obj$par, 
                objective = obj$fn, 
                gradient = obj$gr, 
                control = list(iter.max = its, trace = 0))
  # outer mgc (maximum gradient component)
  
  
  # How does TMB know what the optimal parameters without 
  # sd_report taking them as input?
  sd_out <- sdreport(obj, 
                     par.fixed = opt$par, 
                     getJointPrecision = TRUE)
  
  if(save) saveRDS(sd_out, paste0("results/", survey, "/", save_name, ".rds"))
  
  return(sd_out)
}

m1 <- list(m1_stan = m1_stan, m1_inla = m1_inla, m1_tmb = m1_tmb)