#' Create neighbourlood list from from \code{sf} object.
#'
#' Wrapper function for \code{spdep::poly2nb}.
#'
#' @param sf A simple features object with some geometry.
#' @return A neighbourhood list object.
#' @examples
#' nb <- neighbours(mw)
neighbours <- function(sf){
  spdep::poly2nb(as(sf, "Spatial"))
}

#' Create a graph list object from a neighbourhood list object.
#'
#' @param nb A neighbourhood list object.
#' @return A list containing
#' * \code{n} The number of nodes in the graph
#' * \code{n_edges} To-do
#' * \code{node1} To-do
#' * \code{node2} To-do
#' @source From \href{https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R}{code} by Mitzi Morris.
#' @examples
#' nb <- neighbours(mw)
#' nb_to_graph(nb)
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

#' Create ICAR precision matrix from \code{nb}
#'
#' This function creates the Besag model ICAR precision matrix \eqn{Q}.
#' \eqn{Q} has the entries \eqn{Q_{ij} = -1} if \eqn{i \sim j} (where
#' \eqn{\sim} is an adjacency operator), \eqn{Q_{ii}} is the number of
#' neighbours of region \eqn{i} and \eqn{Q_{ij} = 0} otherwise.
#'
#' @param nb A neighbourhood list object.
#' @return An ICAR precision matrix based upon \code{nb}.
#' @examples
#' nb <- neighbours(mw)
#' nb_to_precision(nb)
nb_to_precision <- function(nb){
  n <- length(nb)
  Q <- matrix(data = 0, nrow = n, ncol = n) # Empty matrix
  for(i in 1:n){
    Q[i, i] <- length(nb[[i]])
    Q[i, nb[[i]]] <- -1
  }
  return(Q)
}

#' Create object containing lengths of borders from \code{sf} object.
#'
#' The function \code{border_precision} creates a dataframe for each area of 
#' \code{sf} containing information about the shared borders with other areas.
#' Within this package the primary use is within \code{\link{border_precision}}.
#'
#' @param sf A simple features object with some geometry.
#' @return A list of \code{nrow(sf)} data frames which each have columns:
#' * \code{origin} The origin node.
#' * \code{perimeter} The perimeter of the origin node.
#' * \code{touching} The index of a node having shared border with the origin
#' node.
#' * \code{length} The length of border between \code{origin} and
#' \code{touching}.
#' * \code{weight} The proportion of \code{origin}'s border which is with
#' \code{touching}.
#' @examples
#' border_lengths(mw)
border_lengths <- function(sf){
  gm <- sf::st_geometry(sf)
  touch <- sf::st_touches(gm) # Adjacency information
  perim <- gm %>%
    sf::st_cast("MULTILINESTRING") %>%
    sf::st_length() # Region perimeters
  # f adapted from Sébastien Rochette SO answer
  f <- function(from){
    if(length(touch[[from]]) != 0) {
      lengths <- sf::st_intersection(gm[from], gm[touch[[from]]]) %>%
        sf::st_length()
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

#' Create border ICAR precision matrix from \code{sf}.
#'
#' This function creates the a weighted ICAR precision matrix \eqn{Q}.
#' In \eqn{Q} each entry \eqn{Q_{ij}} is equal to the shared length of
#' border between the two regions (and so zero if they are not
#' adjacent). The diagonal elements \eqn{Q_{ii}} equal the total border
#' of each region.
#'
#' @param sf A simple features object with some geometry.
#' @return An ICAR precision matrix based upon \code{sf}.
#' @examples
#' border_precision(mw)
border_precision <- function(sf){
  all_lengths <- border_lengths(sf)
  n <- length(all_lengths) # More informative name?
  Q <- matrix(data = 0, nrow = n, ncol = n)
  for(i in 1:nrow(sf)) {
    if(!anyNA(all_lengths[[i]])) {
      Q[i, i] <- all_lengths[[i]]$perimeter[1]
      Q[i, all_lengths[[i]]$touching] <- -all_lengths[[i]]$length
    }
  }
  return(Q)
}

#' Compute scale of the Besag model using \code{R-INLA}.
#' 
#' See \code{INLA::inla.scale.model}.
#' 
#' @param nb A neighbourhood list object.
#' @section Warning:
#' Have not taken into account connectedness, this is a to do.
#' @examples
#' nb <- neighbours(mw)
#' get_scale(nb)
get_scale <- function(nb){

  # Besag precision matrix
  Q <- nb_to_precision(nb)
  n <- nrow(Q)

  # Add jitter to the diagonal for numerical stability
  Q_prt <- Q + Matrix::Diagonal(n) * max(diag(Q)) * sqrt(.Machine$double.eps)

  # Inversion of sparse matrix
  constraint <- list(A = matrix(1, 1, n, e = 0))
  Q_inv <- as.matrix(INLA::inla.qinv(Q_prt, constr = constraint))
  
  # Compute the generalised variance on the covariance matrix
  return(riebler_gv(Q_inv))
}

#' Compute regional covariance matrix using kernel on centroids.
#'
#' @param sf A simple features object with some geometry.
#' @param kernel A kernel function, defaults to \code{matern}.
#' @param ... Additional arguments to \code{kernel}.
#' @return A \code{nrow(sf)} by \code{nrow(sf)} matrix.
#' @examples
#' centroid_covariance(mw)
centroid_covariance <- function(sf, kernel = matern, ...){
  cent <- sf::st_centroid(sf$geometry)
  dist <- sf::st_distance(cent, cent)
  cov <- apply(dist, c(1, 2), FUN = kernel, ...)
  return(cov)
}

#' Compute regional covariance matrix using sample-based kernel.
#'
#' \code{sampling_covariance} draws \code{S} samples uniformly from inside each 
#' region of \code{sf}. The kernel function \code{kernel} (with additional
#' arguments \code{...}) is averaged over each pair of draws between regions
#' to produce the entries of the covariance matrix.
#'
#' @param sf A simple features object with some geometry.
#' @param kernel A kernel function, defaults to \code{matern}.
#' @param ... Additional arguments to \code{kernel}.
#' @param S The number of Monte Carlo samples to draw from each region.
#' @return A \code{nrow(sf)} by \code{nrow(sf)} matrix.
#' @examples
#' sampling_covariance(mw)
sampling_covariance <- function(sf, kernel = matern, ..., S = 100){
  n <- nrow(sf)
  samples <- sf::st_sample(sf, size = rep(S, n))
  D <- sf::st_distance(samples, samples)
  cov <- matrix(nrow = n, ncol = n)
  for(i in 1:n) {
    for(j in 1:n) {
      i_range <- ((i - 1) * S + 1):(i * S)
      j_range <- ((j - 1) * S + 1):(j * S)
      relevant_sample <- D[i_range, j_range]
      d <- mean(relevant_sample)
      cov[i, j] <- kernel(d, ...)
    }
  }
  return(cov)
}
