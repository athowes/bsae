#' Compute forecast assessments for `R-INLA` model at a single held-out point.
#'
#' @template sf
#' @param fit Fitted model.
#' @param i The index (in `1:nrow(sf)`) of the held-out point.
#' @param S The number of Monte Carlo samples to draw from the approximate posterior.
#' @export
held_out_metrics <- function(fit, sf, i, S = 4000){
  y <- round(sf$y[[i]])
  n_obs <- round(sf$n_obs[[i]])
  s <- sample_marginal(fit, i, n_obs, S)
  error_samples <- (s$y_samples - y)
  y_bar <- mean(s$y_samples)

  mse <- mean(error_samples^2)
  mae <- mean(abs(error_samples))
  mse_mean <- (y_bar - y)^2
  mae_mean <- abs(y_bar - y)
  crps <- crps(s$y_samples, y)
  lds <- log(mean(dbinom(y, n_obs, s$rho_samples)))
  
  return(list(mse = mse, mae = mae, 
         mse_mean = mse_mean, mae_mean = mae_mean, 
         crps = crps, lds = lds))
}
