#' Compute forecast assessments for model at a single held-out point.
#'
#' @template sf
#' @param fit Fitted model.
#' @param i The index (in `1:nrow(sf)`) of the held-out point.
#' @param S The number of Monte Carlo samples to draw from the approximate posterior.
#' @export
held_out_metrics <- function(fit, sf, i, S = 4000){
  y <- sf$y[[i]]
  n_obs <- sf$n_obs[[i]]
  s <- sample_marginal(fit, i, n_obs, S)
  error_samples <- (s$y_samples - y)
  y_bar <- mean(s$y_samples)
  
  # dxbinom is not vectorised currently
  pred_dens <- sapply(
    s$rho_samples, 
    FUN = function(sample) dxbinom(y, n_obs, prob = sample, log = FALSE)
  )

  return(list(
      y = y,
      y_bar = y_bar,
      mse = mean(error_samples^2),
      mae = mean(abs(error_samples)), 
      mse_mean = (y_bar - y)^2, 
      mae_mean = abs(y_bar - y), 
      crps = crps(s$y_samples, y), 
      lds = log(mean(pred_dens)),
      t = get_time(fit)
  ))
}
