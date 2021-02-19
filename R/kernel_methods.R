#' The Matern covariance function with smoothness parameter 3/2.
#'
#' @param r A distance between two points.
#' @param l A length-scale, defaults to `l = 1`.
#' @export
matern <- function(r, l = 1) {
  if (r < 0) {stop("r out of valid range.")}
  if (l <= 0) {stop("l out of valid range.")}
  (1 + sqrt(3) * r/l) * exp(-sqrt(3) * r/l)
}

#' Compute distances between area centroids.
#' 
#' @param sf A simple features object with some geometry.
#' @examples
#' centroid_distance(mw)
#' @export
centroid_distance <- function(sf) {
  cent <- sf::st_centroid(sf)
  D <- sf::st_distance(cent, cent)
  return(D)
}

#' Compute centroid kernel gram matrix.
#'
#' @param sf A simple features object with some geometry.
#' @param control How should the length-scale be computed? `"mean"` sets the covariance
#' of the average distance between centroids to be 0.01.
#' @param kernel A kernel function, defaults to `matern`.
#' @param ... Additional arguments to `kernel`.
#' @examples
#' centroid_covariance(mw)
#' @export
centroid_covariance <- function(sf, control = "mean", kernel = matern, ...){
  D <- centroid_distance(sf)
  if(control == "mean"){
    mean <- mean(D)
    solve <- stats::uniroot(f = function(l) kernel(mean, l) - 0.01, lower = 1e-8, upper = max(D))
    l <- solve$root
  }
  K <- kernel(D, ...)
  return(as.matrix(K))
}

#' Compute integrated kernel gram matrix.
#'
#' Draws `S` samples from each area of `sf` and averages `kernel` over each pair of draws.
#'
#' @inheritParams centroid_covariance
#' @param L The number of Monte Carlo samples to draw from each area.
#' @param type The `type` argument of `sf::st_sample`, defaults to `"hexagonal"`
#' @examples
#' sampling_covariance(mw)
#' @export
sampling_covariance <- function(sf, control = "mean", L = 10, kernel = matern, type = "hexagonal", ...){
  n <- nrow(sf)
  samples <- sf::st_sample(sf, type = type, exact = TRUE, size = rep(L, n))
  
  # "OGR: Corrupt data Error in CPL_gdal_dimension(st_geometry(x), NA_if_empty) : OGR error"
  # r-spatial/lwgeom/issues/6
  # r-spatial/sf/issues/1443
  sf::st_crs(samples) <- NA
  
  # Exact = TRUE is not exact
  sample_index <- sf::st_intersects(sf, samples)
  D <- sf::st_distance(samples, samples)
  
  if(control == "mean"){
    mean <- mean(centroid_distance(sf))
    solve <- stats::uniroot(f = function(l) kernel(mean, l) - 0.01, lower = 1e-8, upper = max(D))
    l <- solve$root
  }
  
  kD <- kernel(D, ...)
  K <- matrix(nrow = n, ncol = n)
  
  # Diagonal entries
  for(i in 1:(n - 1)) {
    K[i, i] <- mean(kD[sample_index[[i]], sample_index[[i]]])
    for(j in (i + 1):n) {
      # Off-diagonal entries
      K[i, j] <- mean(kD[sample_index[[i]], sample_index[[j]]]) # Fill the upper triangle
      K[j, i] <- K[i, j] # Fill the lower triangle
    }
  }
  K[n, n] <- mean(kD[sample_index[[n]], sample_index[[n]]])
  
  return(K)
}