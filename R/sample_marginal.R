#' Marginal samples.
#'
#' @param fit Fitted model.
#' @export
sample_marginal <- function(fit, ...) {
  UseMethod("sample_marginal")
}

#' @rdname sample_marginal
#' @param i The index of the marginal to sample from.
#' @param n_obs The sample size.
#' @param S The number of draws.
#' @export
sample_marginal.inla <- function(fit, i, n_obs, S = 4000) {
  full_samples <- INLA::inla.posterior.sample(n = S, fit, selection = list(Predictor = i))
  eta_samples = sapply(full_samples, function(x) x$latent)
  rho_samples <- stats::plogis(eta_samples)
  y_samples <- stats::rbinom(n = S, size = n_obs, prob = rho_samples)
  return(list(rho_samples = rho_samples, y_samples = y_samples))
}

#' @rdname sample_marginal
#' @param i The index of the marginal to sample from.
#' @param n_obs The sample size.
#' @param S The number of draws.
#' @export
sample_marginal.stanfit <- function(fit, i, n_obs, S = 4000) {
  rho_samples <- rstan::extract(fit)$rho[, i]
  iter <- length(rho_samples)
  if(iter < S) {
    warning(paste0("Fewer than S samples are available! Making do with ", iter, "."))
    S <- iter
  }
  y_samples <- stats::rbinom(n = S, size = n_obs, prob = rho_samples)
  return(list(rho_samples = rho_samples, y_samples = y_samples))
}
