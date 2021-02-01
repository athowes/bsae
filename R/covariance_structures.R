#' Create neighbourlood list from from `sf` object.
#'
#' Wrapper function for `spdep::poly2nb`.
#'
#' @param sf A simple features object with some geometry.
#' @return A neighbourhood list object.
#' @examples
#' nb <- neighbours(mw)
#' @export
neighbours <- function(sf){
  spdep::poly2nb(as(sf, "Spatial"))
}

#' Create a graph list object from a neighbourhood list object.
#'
#' @param nb A neighbourhood list object.
#' @return A list containing
#' * `n` The number of nodes in the graph
#' * `n_edges` To-do
#' * `node1` To-do
#' * `node2` To-do
#' @source From \href{https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R}{code} by Mitzi Morris.
#' @examples
#' nb <- neighbours(mw)
#' nb_to_graph(nb)
#' @export
nb_to_graph <- function(nb){
  n <- length(nb)
  n_links <- 0
  for (i in 1:n) {
    if (nb[[i]][1] != 0) {
      n_links <- n_links + length(nb[[i]])
    }
  }
  n_edges <- n_links / 2;
  node1 <- vector(mode = "numeric", length = n_edges)
  node2 <- vector(mode = "numeric", length = n_edges)
  idx <- 0
  for (i in 1:n) {
    if (nb[[i]][1] > 0) {
      for (j in 1:length(nb[[i]])) {
        n2 <- unlist(nb[[i]][j])
        if (i < n2) {
          idx <- idx + 1
          node1[idx] <- i
          node2[idx] <- n2
        }
      }
    }
  }
  return(list("n" = n, "n_edges" = n_edges, "node1" = node1, "node2" = node2))
}

#' Create ICAR precision matrix from `nb`
#'
#' This function creates the Besag model ICAR precision matrix \eqn{Q}.
#' \eqn{Q} has the entries \eqn{Q_{ij} = -1} if \eqn{i \sim j} (where
#' \eqn{\sim} is an adjacency operator), \eqn{Q_{ii}} is the number of
#' neighbours of area \eqn{i} and \eqn{Q_{ij} = 0} otherwise.
#'
#' @param nb A neighbourhood list object.
#' @return An ICAR precision matrix based upon `nb`.
#' @examples
#' nb <- neighbours(mw)
#' nb_to_precision(nb)
#' @export
nb_to_precision <- function(nb){
  n <- length(nb)
  Q <- matrix(data = 0, nrow = n, ncol = n) # Empty matrix
  for(i in 1:n){
    if(nb[[i]][1] == 0){
      Q[i, i] = 0 
    }
    else{
      Q[i, i] <- length(nb[[i]]) 
    }
    Q[i, nb[[i]]] <- -1
  }
  return(Q)
}

#' Create object containing lengths of borders from `sf` object.
#'
#' The function [`border_lengths`] creates a dataframe for each area of 
#' `sf` containing information about the shared borders with other areas.
#' Within this package the primary use is within [`border_precision`].
#'
#' @param sf A simple features object with some geometry.
#' @return A list of `nrow(sf)` data frames which each have columns:
#' * `origin` The origin node.
#' * `perimeter` The perimeter of the origin node.
#' * `touching` The index of a node having shared border with the origin
#' node.
#' * `length` The length of border between `origin` and
#' `touching`.
#' * `weight` The proportion of `origin`'s border which is with
#' `touching`.
#' @examples
#' border_lengths(mw)
#' @export
border_lengths <- function(sf){
  gm <- sf::st_geometry(sf)
  touch <- sf::st_touches(gm) # Adjacency information
  perim_strings <- sf::st_cast(gm, "MULTILINESTRING")
  perim <- sf::st_length(perim_strings) # Area perimeters
  
  # f adapted from Sébastien Rochette SO answer
  f <- function(from){
    if(length(touch[[from]]) != 0) {
      lengths <- sf::st_length(
        sf::st_intersection(gm[from], gm[touch[[from]]])
        )
      data.frame(origin = from,
                 perimeter = perim[[from]],
                 touching = touch[[from]],
                 length = lengths,
                 weight =  lengths / perim[[from]]) # Non-symmetric matrix!
    } else {
      return(NA)
    }
  }
  all_lengths <- lapply(1:length(touch), f)
  return(all_lengths)
}

#' Create border ICAR precision matrix from `sf`.
#'
#' This function creates the a weighted ICAR precision matrix \eqn{Q}.
#' In \eqn{Q} each entry \eqn{Q_{ij}} is equal to the shared length of
#' border between the two areas (and so zero if they are not
#' adjacent). The diagonal elements \eqn{Q_{ii}} equal the total border
#' of each area.
#'
#' @param sf A simple features object with some geometry.
#' @return An ICAR precision matrix based upon `sf`.
#' @examples
#' border_precision(mw)
#' @export
border_precision <- function(sf){
  all_lengths <- border_lengths(sf)
  n <- length(all_lengths)
  Q <- matrix(data = 0, nrow = n, ncol = n)
  for(i in 1:nrow(sf)) {
    if(!anyNA(all_lengths[[i]])) {
      Q[i, i] <- all_lengths[[i]]$perimeter[1]
      Q[i, all_lengths[[i]]$touching] <- -all_lengths[[i]]$length
    }
  }
  return(Q)
}

#' Compute scale of a precision matrix using `R-INLA`.
#' 
#' Here scale refers to the Riebler generalised variance (see 
#' [`riebler_gv`]). This is computed by first inverting
#' the precision matrix, which may first require imposing 
#' constraints. Note that if using this function directly on
#' `nb_to_precision(nb)` for some neighbourhood structure `nb`
#' then it is assumed that the graph is fully connected. 
#' 
#' @param Q A (square, symmetric) precision matrix.
#' @param constraint A list with arguments `A` and `e` which imposes
#' the constraint `Au = e` (where the precision of `u` is `Q`). 
#' See the `?INLA::f` argument `extraconstr`. If `constraint` is the default
#' value then this is a sum-to-zero constraint.
#' @return A scalar representing the generalised variance of the inverse of `Q`.
#' @examples
#' nb <- neighbours(mw)
#' Q <- nb_to_precision(nb)
#' get_scale(Q)
#' @export
get_scale <- function(Q, constraint = list(A = matrix(1, 1, nrow(Q)), e = 0)){
  n <- nrow(Q)
  # Add jitter to the diagonal for numerical stability
  Q_prt <- Q + Matrix::Diagonal(n) * max(diag(Q)) * sqrt(.Machine$double.eps)
  # Inversion of sparse matrix
  Q_inv <- INLA::inla.qinv(Q_prt, constr = constraint)
  # Compute the generalised variance on the covariance matrix
  return(riebler_gv(as.matrix(Q_inv)))
}

#' Scales the precision matrix of a Gaussian Markov random field.
#' 
#' Implements the same thing as `INLA::inla.scale.model`.
#' 
#' @inheritParams get_scale
#' @param A See the `constraint` argument of [`get_scale`].
#' @return A list containing a matrix of the same dimension as `Q` where 
#' the entries have been scaled according to the paper "A note on intrinsic conditional 
#' autoregressive models for disconnected graphs" by Freni-Sterrantino, 
#' Ventrucci and Rue, as well as `scales` a vector of the scale used for each component.
#' @source From \href{https://github.com/mrc-ide/naomi/blob/master/R/car.R}{code} by Jeff Eaton.
#' @examples
#' nb <- neighbours(mw)
#' Q <- nb_to_precision(nb)
#' scale_gmrf_precision(Q)
#' @export
scale_gmrf_precision <- function(Q, A = matrix(1, 1, nrow(Q))){
  nb <- spdep::mat2listw(abs(Q))$neighbours
  comp <- spdep::n.comp.nb(nb)
  scales <- rep(NA, comp$nc)
  for (k in seq_len(comp$nc)) {
    idx <- which(comp$comp.id == k)
    Qc <- Q[idx, idx, drop = FALSE]
    if (length(idx) == 1) {
      scales[k] <- 1 
      Qc[1, 1] <- 1 # Set marginal variance for islands to be 1
    } else {
      Ac <- A[ , idx, drop = FALSE]
      scale <- get_scale(Qc, constraint = list(A = Ac, e = 0))
      scales[k] <- scale
      Qc <- scale * Qc
    }
    Q[idx, idx] <- Qc
  }
  return(list(Q = Q, scales = scales))
}

#' Compute distances between area centroids.
#' 
#' Wrapper function for `sf::centroid` and `sf::distance`.
#'
#' @param sf A simple features object with some geometry.
#' @return A `nrow(sf)` by `nrow(sf)` matrix.
#' @examples
#' centroid_distance(mw)
#' @export
centroid_distance <- function(sf) {
  cent <- sf::st_centroid(sf)
  D <- sf::st_distance(cent, cent)
  return(D)
}

#' Compute areal covariance matrix using kernel on centroids.
#'
#' @param sf A simple features object with some geometry.
#' @param kernel A kernel function, defaults to `matern`.
#' @param ... Additional arguments to `kernel`.
#' @return A `nrow(sf)` by `nrow(sf)` matrix.
#' @examples
#' centroid_covariance(mw)
#' @export
centroid_covariance <- function(sf, kernel = matern, ...){
  D <- centroid_distance(sf)
  K <- kernel(D, ...)
  return(as.matrix(K))
}

#' Compute areal covariance matrix using sample-based kernel.
#'
#' `sampling_covariance` draws `S` samples uniformly from inside each 
#' area of `sf`. The kernel function `kernel` (with additional
#' arguments `...`) is averaged over each pair of draws between areas
#' to produce the entries of the covariance matrix.
#'
#' @param sf A simple features object with some geometry.
#' @param kernel A kernel function, defaults to `matern`.
#' @param ... Additional arguments to `kernel`.
#' @param L The number of Monte Carlo samples to draw from each area.
#' @param type String passed to the `type` argument of `sf::st_sample`, defaults to `"hexagonal"`
#' @return A `nrow(sf)` by `nrow(sf)` matrix.
#' @examples
#' sampling_covariance(mw)
#' @export
sampling_covariance <- function(sf, L = 10, kernel = matern, type = "hexagonal", ...){
  n <- nrow(sf)
  samples <- sf::st_sample(sf, type = type, exact = TRUE, size = rep(L, n))
  
  # Exact = TRUE is not exact
  sample_index <- sf::st_intersects(mw, samples)
  
  D <- sf::st_distance(samples, samples)
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