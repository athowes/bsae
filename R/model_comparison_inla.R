#' Compute log density score from `R-INLA` model at a single held-out point.
#'
#' @param sf A simple features data frame.
#' @param fit A fitted `R-INLA` model.
#' @param i The index of the held-out data point to predict on. Should be an
#' integer in the range `1:nrow(sf)`.
#' @param S The number of Monte Carlo samples to draw from the `R-INLA`
#' approximate posterior distribution over the latent field.
#' @return A scalar log density score.
#' @examples
#' fit <- m1_inla(mw)
#' eval_inla_model(mw, fit, i = 1, S = 1000)
#' @export
eval_inla_model <- function(sf, fit, i, S = 5000){
  samples <- INLA::inla.posterior.sample(n = S, fit, selection = list(Predictor = i))
  eta_samples = sapply(samples, function(x) x$latent)
  rho_samples <- plogis(eta_samples)
  y <- sf$y[[i]]
  n_obs <- sf$n_obs[[i]]
  lds <- log(sum(stats::dbinom(round(y), n_obs, rho_samples)) / S)
  return(lds)
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
