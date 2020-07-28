#' The logistic transformation.
#'
#' Compute the logistic (inverse logit) transformation, given by
#' \eqn{x \mapsto \exp(x) / (1 + \exp(x))}.
#'
#' @param x A number.
#' @return The logistic tranformation of \code{x}.
logistic <- function(x) {
  exp(x) / (1 + exp(x))
}

#' Matern covariance.
#'
#' The Matern covariance function with smoothness parameter settings limited to
#' either 1.5 or 2.5.
#'
#' @param r A distance between two points.
#' @param l A lengthscale, defaults to `l = 1`.
#' @param nu A smoothness parameter, either `nu = 1.5` (the default)
#' or `nu = 2.5`.
#' @return The Matern covariance.
matern <- function(r, l = 1, nu = 1.5){
  if(!nu %in% c(1.5, 2.5)){
    errorCondition("Choose nu = 1.5 or nu = 2.5")
  }
  ifelse(nu == 1.5,
         (1 + sqrt(3)*r/l) * exp(-sqrt(3) * r/l), # 1.5 case
         (1 + sqrt(5)*r/l + 5*r^2/(3*l^2)) * exp(-sqrt(5) * r/l)) # 2.5 case
}

#' Compute the Riebler generalised variance of a covariance matrix.
#' 
#' Let \eqn{A} be a square matrix, then the Riebler
#' generalised variance is defined as the geometric mean of the marginal 
#' variances, given by
#' \deqn{\sigma_{\mathrm{GV}}^2(A) = \exp \left( \frac{1}{n} \sum_{i = 1}^n \log A_{ii} \right).}
#'
#' @param A A square matrix.
#' @return A scalar generalised variance.
riebler_gv <- function(A) {
  exp(mean(log(diag(A))))
}
