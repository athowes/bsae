# Model 2: Besag

m2_stan <- function(save_name = "stan_fit2", save = FALSE){
  
  # Stan pairwise Besag/ICAR implementation
  nb <- poly2nb(df)
  g <- nb_to_graph(nb)
  
  dat <- list(n = nrow(df),
              y = round(df$y),  
              m = df$n_obs,
              n_edges = g$n_edges,
              node1 = g$node1,
              node2 = g$node2)
  
  fit <- stan(file = "stan/model2.stan", 
              data = dat, 
              warmup = nsim_warm, 
              iter = nsim_iter)
  
  if(save) saveRDS(fit, paste0("results/", survey, "/", save_name, ".rds"))
  
  return(fit)
}

m2_inla <- function(save_name = "inla_fit2", save = FALSE){

  nb <- neighbours(df)
  
  dat <- list(id = 1:nrow(df),
              y = round(df$y),  
              m = df$n_obs)
  
  # sigma ~ N(0. 2.5^2); initial in terms of log(tau) so 0 corresponds to tau = 1
  tau_prior <- list(prec = list(prior = "logtnormal", param = c(0, 1/2.5^2), 
                                initial = 0, fixed = FALSE))

  formula <- y ~ 1 + f(id, 
                       model = "besag", 
                       graph = nb, 
                       scale.model = TRUE, 
                       constr = TRUE, 
                       hyper = tau_prior)
  
  # constr = TRUE is a sum-to-zero constraint else +/- constant to all leaves density unchanged
  
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

m2_tmb <- function(save_name = "tmb_fit2", save = FALSE){
  
  compile("tmb/model2.cpp")
  dyn.load(dynlib("tmb/model2"))
  
  nb <- neighbours(df)
  Q <- nb_to_precision(nb)
  
  dat <- list(n = nrow(df),
              y = round(df$y),  
              m = df$n_obs,
              Q = Q)
  
  param <- list(beta_0 = 0,
                phi = rep(0, nrow(df)),
                l_sigma_phi = 0)
  
  obj <- MakeADFun(data = dat, 
                   parameters = param,
                   DLL = "model2")
  
  its <- 1000
  opt <- nlminb(start = obj$par, 
                objective = obj$fn, 
                gradient = obj$gr, 
                control = list(iter.max = its, trace = 0))
  
  sd_out <- sdreport(obj, 
                     par.fixed = opt$par,
                     getJointPrecision = TRUE)
  
  if(save) saveRDS(sd_out, paste0("results/", survey, "/", save_name, ".rds"))
  
  return(sd_out)
}

m2 <- list(m2_stan = m2_stan, m2_inla = m2_inla, m2_tmb = m2_tmb)