#' Watanabe–Akaike (Widely-Applicable) information criterion generic.
#'
#' @param fit Fitted model.
#' @export
waic <- function(fit, ...) {
  UseMethod("waic")
}

#' @rdname waic
#' @export
waic.inla <- function(fit) {
  fit$waic$waic
}

#' @rdname waic
#' @export
waic.stanfit <- function(fit) {
  log_lik <- loo::extract_log_lik(fit)
  waic <- loo::waic(log_lik)
  return(waic) 
}