#' Create object containing lengths of borders from `sf` object.
#'
#' The function [`border_lengths`] creates a dataframe for each area of 
#' `sf` containing information about the shared borders with other areas.
#' Within this package the primary use is within [`border_precision`].
#'
#' @template sf
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
#' @template sf
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