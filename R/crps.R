#' Compute an approximation to the continuous ranked probability score.
#'
#' @param samples A collection of draws from a probability distribution.
#' @param true_value The underlying true value that is being compared to.
#' @return The continuous ranked probability score (CRPS)
#' @export
crps <- function(samples, true_value){
  mean(abs(samples - true_value)) - 0.5 * mean(outer(samples, samples, FUN = function(x, y) abs(x - y)))
}