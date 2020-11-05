#' Compute log density score from `stan` model at a single held-out point.
#'
#' @param sf A simple features data frame.
#' @param fit A fitted `stan` model.
#' @param i The index of the held-out data point to predict on. Should be an
#' integer in the range `1:nrow(sf)`.
#' approximate posterior distribution over the latent field.
#' @return A scalar log density score.
#' @export
eval_stan_model <- function(sf, fit, i){
  rho_samples <- rstan::extract(fit)$rho[, i]
  y <- sf$y[[i]]
  n_obs <- sf$n_obs[[i]]
  lds <- log(mean(stats::dbinom(round(y), n_obs, rho_samples)))
  return(lds)
}

#' Compute DIC and WAIC for `stan` models.
#'
#' @param fit A fitted `stan` model.
#' @return A list containing the DIC and WAIC.
#' @export
stan_info_criteria <- function(fit) {
  return(list(
    "DIC" = round(stan_dic(fit), digits = 2),
    "WAIC" = round(stan_waic(fit)$estimates["waic", "Estimate"], digits = 2)
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
  log_lik <- loo::extract_log_lik(temp)
  waic <- loo::waic(log_lik)
  return(waic) 
}