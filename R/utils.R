#' The logistic transformation.
#'
#' Compute the logistic (inverse logit) transformation, given by
#' \eqn{x \mapsto \exp(x) / (1 + \exp(x))}.
#'
#' @param x A number.
#' @return The logistic tranformation of \code{x}.
#' @export
logistic <- function(x) {
  exp(x) / (1 + exp(x))
}

#' Matern 3/2 covariance.
#'
#' The Matern covariance function with smoothness parameter 3/2.
#'
#' @param r A distance between two points.
#' @param l A lengthscale, defaults to `l = 1`.
#' @return The Matern covariance.
#' @export
matern <- function(r, l = 1) {
  (1 + sqrt(3)*r/l) * exp(-sqrt(3) * r/l)
}

# (1 + sqrt(5)*r/l + 5*r^2/(3*l^2)) * exp(-sqrt(5) * r/l)) # 2.5 case

#' Compute the Riebler generalised variance of a covariance matrix.
#' 
#' Let \eqn{A} be a square matrix, then the Riebler
#' generalised variance is defined as the geometric mean of the marginal 
#' variances, given by
#' \deqn{\sigma_{\mathrm{GV}}^2(A) = \exp \left( \frac{1}{n} \sum_{i = 1}^n \log A_{ii} \right).}
#'
#' @param A A square matrix.
#' @return A scalar generalised variance.
#' @export
riebler_gv <- function(A) {
  exp(mean(log(diag(A))))
}

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