#' Compute forecast assessments for `R-INLA` model at a single held-out point.
#'
#' @param sf A simple features data frame.
#' @param fit A fitted `R-INLA` model.
#' @param i The index of the held-out data point to predict on. Should be an
#' integer in the range `1:nrow(sf)`.
#' @param S The number of Monte Carlo samples to draw from the `R-INLA`
#' approximate posterior distribution over the latent field.
#' @return A list containing `mse` (mean square error), 
#' `mae` (mean absolute error), `crps` (continuous ranked probability score),
#' and `lds` (log density score).
#' @examples
#' fit <- m1_inla(mw)
#' eval_inla_model(mw, fit, i = 1, S = 1000)
#' @export
eval_inla_model <- function(sf, fit, i, S = 5000){
  
  # Using rounding for now
  y <- round(sf$y[[i]])
  n_obs <- round(sf$n_obs[[i]])
  
  samples <- INLA::inla.posterior.sample(n = S, fit, selection = list(Predictor = i))
  eta_samples = sapply(samples, function(x) x$latent)
  rho_samples <- plogis(eta_samples)
  y_samples <- rbinom(n = S, 
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

#' Compute DIC, WAIC and CPO for `R-INLA` models.
#'
#' @param fit A fitted `R-INLA` model.
#' @return A list containing the DIC, WAIC and CPO.
#' @export
inla_info_criteria <- function(fit) {
  return(list(
    "DIC" = round(fit$dic$dic, digits = 2),
    "WAIC" = round(fit$waic$waic, digits = 2),
    "CPO" = round(2 * sum(-log(fit$cpo$cpo)), digits = 2)
  ))
}
