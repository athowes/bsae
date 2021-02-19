#' Density of the generalised binomial distribution.
#' 
#' @param x A real number less than `size`.
#' @param size A real number of trials.
#' @param prob The probability of success.
#' @param log Should the returned probability be on the log scale?
#' @export
dxbinom <- function(x, size, prob, log = FALSE) {
  constant <- lgamma(size + 1) - lgamma(x + 1) - lgamma(size - x + 1)
  lpdf <- constant + x * log(prob) + (size - x) * log(1 - prob)
  return(ifelse(log, lpdf, exp(lpdf)))
}