#' Report the time taken to fit the model.
#'
#' @param fit Fitted model.
#' @param ... Additional arguments passed to `get_time`.
#' @export
get_time <- function(fit, ...) {
  UseMethod("get_time")
}

#' @rdname get_time
#' @export
get_time.inla <- function(fit, ...) {
  fit$cpu.used[["Total"]]
}

#' @rdname get_time
#' @export
get_time.stanfit <- function(fit, ...) {
  sum(rstan::get_elapsed_time(fit))
}