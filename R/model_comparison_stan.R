#' Compute forecast assessments for `stan` model at a single held-out point.
#'
#' @param sf A simple features data frame.
#' @param fit A fitted `stan` model.
#' @param i The index of the held-out data point to predict on. Should be an
#' integer in the range `1:nrow(sf)`.
#' @return A list containing `mse` (mean square error), 
#' `mae` (mean absolute error), `crps` (continuous ranked probability score),
#' and `lds` (log density score).
#' @export
eval_stan_model <- function(sf, fit, i){
  
  # Using rounding for now
  y <- round(sf$y[[i]])
  n_obs <- round(sf$n_obs[[i]])
  
  rho_samples <- rstan::extract(fit)$rho[, i]
  y_samples <- rbinom(n = length(rho_samples), 
                      size = n_obs,
                      prob = rho_samples)
  error_samples <- (y_samples - y)
  
  mse <- mean(error_samples^2) # Mean square error
  mae <- mean(abs(error_samples)) # Mean absolute error
  crps <- crps(y_samples, y) # Continuous ranked probability score
  lds <- log(mean(dbinom(y, n_obs, rho_samples))) # Log density score
  
  return(list(mse = mse,
              mae = mae,
              crps = crps,
              lds = lds))
}

#' Compute DIC and WAIC for `stan` models.
#'
#' @param fit A fitted `stan` model.
#' @return A list containing the DIC and WAIC.
#' @export
stan_info_criteria <- function(fit) {
  return(list(
    "DIC" = round(stan_dic(fit), digits = 2),
    "WAIC" = round(stan_waic(fit)$estimates["waic", "Estimate"], digits = 2),
    "CPO" = NA
  ))
}

stan_dic <- function(fit){
  pointwise_log_lik <- rstan::extract(fit, 'log_lik')$log_lik
  log_lik <- rowSums(pointwise_log_lik)
  mean_deviance <- -2 * mean(log_lik)
  deviance_mle <- -2 * max(log_lik)
  #p_dic <- mean_deviance - deviance_mle
  dic <- 2*mean_deviance - deviance_mle
  return(dic)
}

stan_waic <- function(fit){
  log_lik <- loo::extract_log_lik(fit)
  waic <- loo::waic(log_lik)
  return(waic) 
}