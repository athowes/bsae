#' The Matern covariance function with smoothness parameter 3/2.
#'
#' @param r A distance between two points.
#' @param l A length-scale, defaults to `l = 1`.
#' @export
matern <- function(r, l = 1) {
  if (any(r < 0)) {stop("r out of valid range.")}
  if (l <= 0) {stop("l out of valid range.")}
  (1 + sqrt(3) * r/l) * exp(-sqrt(3) * r/l)
}

#' Compute distances between area centroids.
#' 
#' @template sf
#' @examples
#' centroid_distance(mw)
#' @export
centroid_distance <- function(sf) {
  cent <- sf::st_centroid(sf)
  D <- sf::st_distance(cent, cent)
  return(D)
}

#' Compute centroid kernel Gram matrix.
#' 
#' If a lengthscale is not provided then the `best_average` method is used.
#'
#' @template sf
#' @param kernel A kernel function, defaults to `matern`.
#' @param l A lengthscale.
#' @param ... Additional arguments to `kernel`.
#' @examples
#' centroid_covariance(mw)
#' @export
centroid_covariance <- function(sf, kernel = matern, l = NULL, ...){
  D <- centroid_distance(sf)
  
  # Use the best_average if l is not provided
  if(is.null(l)){
    l <- best_average(D, kernel = kernel, p = 0.01)
  }
  
  K <- as.matrix(kernel(D, l = l, ...))
  return(K)
}

#' Compute integrated kernel Gram matrix.
#'
#' Draws `S` samples from each area of `sf` and averages `kernel` over each pair of draws.
#' If a lengthscale is not provided then the `best_average` method is used.
#'
#' @inheritParams centroid_covariance
#' @param L The number of Monte Carlo samples to draw from each area.
#' @param type The `type` argument of `sf::st_sample`, defaults to `"hexagonal"`
#' @examples
#' integrated_covariance(mw)
#' @export
integrated_covariance <- function(sf, L = 10, kernel = matern, type = "hexagonal", l = NULL, ...){
  n <- nrow(sf)
  samples <- sf::st_sample(sf, type = type, exact = TRUE, size = rep(L, n))
  
  # "OGR: Corrupt data Error in CPL_gdal_dimension(st_geometry(x), NA_if_empty) : OGR error"
  # r-spatial/lwgeom/issues/6
  # r-spatial/sf/issues/1443
  sf::st_crs(samples) <- NA
  
  # Exact = TRUE is not exact
  sample_index <- sf::st_intersects(sf, samples)
  
  D <- sf::st_distance(samples, samples)
  
  # Use the best_average if l is not provided
  if(is.null(l)){
    l <- best_average(D, kernel = kernel, p = 0.01)
  }
  
  kD <- kernel(D, l = l, ...)
  
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