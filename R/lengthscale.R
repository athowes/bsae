#' Compute length-scale such that averagely distant points have correlation `p`.
#'
#' See Best (1999) "Bayesian models for spatially correlated disease and exposure data".
#'
#' @param D Matrix of distances between points.
#' @param kernel A kernel function, defaults to `matern`.
#' @param p A percentage, defaults to 0.01.
#' @param ... Additional arguments to `kernel`.
#' @export
best_average <- function(D, kernel = matern, p = 0.01, ...) {
  m <- mean(D)
  l_opt <- stats::uniroot(
    f = function(l) kernel(m, l, ...) - p, 
    lower = 1e-8,
    upper = max(D) # Length-scale unlikely to be greater than maximum distance
  )
  return(l_opt$root)
}
