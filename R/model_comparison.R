#' Create spatial leave-one-block-out training data sets.
#'
#' In leave-one-block-out (LOBO) cross-validation, in each fold one spatially
#' contigous block is left out in order to be predicted upon.
#' This function creates a list of training data sets where the `i`th item
#' leaves out region `i` and its neighbours.
#'
#' @param sf A simple features data frame.
#' @param remove_cols A vector of named columns which are to have entries
#' replaced by `NA` in the training data sets. Defaults to
#' `c("y")`.
#' @return A list of `nrow(sf)` training set lists.
#' Each training set list contains:
#' * `data` The training data set with left-out entries.
#' * `centre` The index of the central held-out region.
#' * `held_out` The indices of all held-out regions.
#' @examples
#' lobo(mw, remove_cols = c("y", "est"))
lobo <- function(sf, remove_cols = c("y")){
  n <- nrow(sf)
  nb <- neighbours(sf)
  training_sets <- vector(mode = "list", length = n)
  for(i in 1:nrow(sf)) {
    sf_new <- sf
    i_neighbours <- nb[[i]] # The neighbours of region i
    held_out <- c(i, i_neighbours)
    # Replace the entries in remove_cols by NA
    sf_new[c(i, i_neighbours), remove_cols] <- NA
    training_sets[[i]] <- list(data = sf_new, centre = i, held_out = held_out)
  }
  return(training_sets)
}

#' Compute log density score from `R-INLA} model at a single held-out
#' point.
#'
#' @param sf A simple features data frame.
#' @param fit A fitted `R-INLA` model.
#' @param i The index of the held-out data point to predict on. Should be an
#' integer in the range `1:nrow(sf)`.
#' @param S The number of Monte Carlo samples to draw from the `R-INLA`
#' approximate posterior distribution over the latent field.
#' @return A scalar log density score.
#' @examples
#' fit <- m1_inla(mw)
#' eval_inla_model(mw, fit, i = 1, S = 1000)
eval_inla_model <- function(sf, fit, i, S = 5000){
  samples <- INLA::inla.posterior.sample(n = S, fit,
                                         selection = list(Predictor = i))
  eta_samples = sapply(samples, function(x) x$latent)
  rho_samples <- logistic(eta_samples)
  y <- sf$y[[i]]
  n_obs <- sf$n_obs[[i]]
  lds <- log(sum(dbinom(round(y), n_obs, rho_samples)) / S)
  return(lds)
}

#' Cross-validation using [`lobo`] and [`eval_inla_model`].
#'
#' @param sf A simple features data frame.
#' @param fiting_fun An `R-INLA` model fitting function such as
#' `m1_inla`.
#' @param ... Additinal arguments to `fitting_fun`.
#' @param S The number of Monte Carlo samples to draw from the `R-INLA`
#' approximate posterior distribution over the latent field.
#' @return A vector of log density scores (one entry for training data set
#' produced by [`lobo`].
#' @examples
#' cross_validate(mw, fitting_fun = m1_inla)
cross_validate <- function(sf, fitting_fun, ..., S = 5000) {
  n <- nrow(sf)
  training_sets <- lobo(sf, remove_cols = c("y", "est"))
  fits <- lapply(training_sets,
                 FUN = function(set) fitting_fun(set$data, ...))

  tsfs <- training_sets
  for(i in 1:nrow(sf)) {
    tsfs[[i]]$fit <- fits[[i]]
  }

  scores <- lapply(tsfs,
                   FUN = function(tsf)
                     eval_inla_model(sf, tsf$fit, tsf$centre, S = S))

  message("Completed cross-validation")
  
  return(unlist(scores))
}
