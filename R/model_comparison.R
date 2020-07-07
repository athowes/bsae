# input: df
# returns: a list where training_set[[i]] has the ith region and its neighbours set to NA
lobo <- function(df){
  
  n <- nrow(df)
  nb <- poly2nb(df)
  training_sets <- vector(mode = "list", length = n)  
  
  for(i in 1:nrow(df)) {
    df_new <- df
    
    # The neighbours of region i
    i_neighbours <- nb[[i]]
    held_out <- c(i, i_neighbours)
          
    # Replace the y and est entries by NA
    df_new[c(i, i_neighbours), c("y", "est")] <- rep(NA, length(i_neighbours) + 1)
    
    training_sets[[i]] <- list(data = df_new, centre = i, held_out = held_out)
  }
  
  return(training_sets)
}

# input: set, an item of the list training_sets defined above
# returns: a fitted sae model
train_run_sae <- function(set) {
  df <- filter(set$data) 
  run_sae(survey, method, model, save = FALSE, df = df)
}

# input
# returns
inla_evaluate_model <- function(model, S = 5000){
  i <- model$centre
  # model[[4]] is where the fit is stored, e.g. cv[[1]][[4]]
  samples <- inla.posterior.sample(n = S, result = model[[4]], selection = list(Predictor = i))
  eta_samples = sapply(samples, function(x) x$latent)
  rho_samples <- logistic(eta_samples)
  y <- df$y[[i]]
  n_obs <- df$n_obs[[i]]
  lds <- log(sum(dbinom(round(df$y[i]), df$n_obs[i], rho_samples)) / S)
  return(lds)
}