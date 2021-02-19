#' Create cross-validation training data sets.
#'
#' This function creates a list of training datasets each of which has
#' certain entries of `remove_cols` replaced by `NA` according to the
#' type of cross-validation chosen. If `type = "LOO"` one entry of each
#' training dataset is replaced by `NA`, if `type = "SLOO"` one entry and
#' the entries of the neighbours of the area it corresponds to are replaced
#' by `NA`.
#'
#' @template sf
#' @param remove_cols A vector of named columns which are to have entries
#' replaced by `NA` in the training data sets. Defaults to
#' `c("y")`.
#' @param type One of `"LOO"` or `"SLOO"`.
#' @return A list of `nrow(sf)` training set lists.
#' Each training set list contains:
#' * `data` The training data set with left-out entries.
#' * `held_out` The indices of all held-out regions.
#' * `predict_on` The indices of regions to be predicted upon.
#' @examples
#' create_folds(mw, remove_cols = c("y", "est"))
#' @export
create_folds <- function(sf, remove_cols = c("y"), type = "LOO"){
  n <- nrow(sf)
  training_sets <- vector(mode = "list", length = n)
  
  if(type == "SLOO"){
    nb <- neighbours(sf)
    nb <- lapply(
      nb, 
      FUN = function(region) {
        if(region[1] == 0) { 
          return(NULL) 
        } else { 
          return(region) 
        }
      }
    )
    for(i in 1:nrow(sf)) {
      sf_new <- sf
      i_neighbours <- nb[[i]]
      held_out <- c(i, i_neighbours)
      sf_new[c(i, i_neighbours), remove_cols] <- NA
      training_sets[[i]] <- list(data = sf_new, held_out = held_out, predict_on = i)
    }
  }
  if(type == "LOO"){
    for(i in 1:nrow(sf)){
      sf_new <- sf
      sf_new[i, remove_cols] <- NA
      training_sets[[i]] <- list(data = sf_new, held_out = i, predict_on = i)
    }
  }
  return(training_sets)
}