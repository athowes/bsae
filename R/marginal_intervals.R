#' Intervals for posterior marginals.
#'
#' @param fit Fitted model.
#' @export
marginal_intervals <- function(fit, ...) {
  UseMethod("marginal_intervals")
}

#' @rdname marginal_intervals
#' @export
marginal_intervals.inla <- function(fit) {
  df <- fit$summary.fitted.values
  return(dplyr::select(df, mean, sd, lower = "0.025quant", upper = "0.975quant"))
}

#' @rdname marginal_intervals
#' @export
marginal_intervals.stanfit <- function(fit, parameter) {
  df <- data.frame(rstan::summary(fit)$summary)
  df <- tibble::rownames_to_column(df, "param")
  df <- dplyr::filter(df, substr(param, 1, nchar(name)) == parameter)
  return(dplyr::select(df, mean, sd, lower = X2.5., upper = X97.5.))
}
