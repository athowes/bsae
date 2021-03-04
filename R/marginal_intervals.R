#' Intervals for posterior marginals.
#'
#' @param fit Fitted model.
#' @param ... Additional arguments passed to `marginal_intervals`.
#' @export
marginal_intervals <- function(fit, ...) {
  UseMethod("marginal_intervals")
}

#' @rdname marginal_intervals
#' @export
marginal_intervals.inla <- function(fit, ...) {
  df <- fit$summary.fitted.values
  return(dplyr::select(df, .data$mean, .data$sd, lower = .data$0.025quant, upper = .data$0.975quant))
}

#' @rdname marginal_intervals
#' @param parameter String containing the parameter name.
#' @export
marginal_intervals.stanfit <- function(fit, parameter, ...) {
  param <- NULL
  df <- data.frame(rstan::summary(fit)$summary)
  df <- tibble::rownames_to_column(df, "param")
  df <- dplyr::filter(df, substr(param, 1, nchar(parameter)) == parameter)
  return(dplyr::select(df, .data$mean, .data$sd, lower = .data$X2.5., upper = .data$X97.5.))
}
