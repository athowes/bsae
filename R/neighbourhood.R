#' Wrapper function for `spdep::poly2nb`.
#'
#' @param sf A simple features object with some geometry.
#' @return A neighbourhood list object.
#' @examples
#' nb <- neighbours(mw)
#' @export
neighbours <- function(sf){
  nb <- spdep::poly2nb(as(sf, "Spatial"))
  return(nb)
}

#' Create a graph from `nb`.
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
    if (nb[[i]][1] > 0) {
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

#' Create Besag precision matrix from `nb`.
#'
#' Creates the Besag model ICAR precision matrix \eqn{Q} with entries 
#' \eqn{Q_{ij} = -1} if \eqn{i \sim j} (where \eqn{\sim} is an adjacency operator),
#' \eqn{Q_{ii}} is the number of neighbours of area \eqn{i} and \eqn{Q_{ij} = 0} otherwise.
#'
#' @param nb A neighbourhood list object.
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