#' Cross-validation using `create_folds` and evaluation using `held_out_metrics`.
#'
#' @template sf
#' @param fn A model fitting function.
#' @param ... Additional arguments to `fn`.
#' @param S The number of Monte Carlo samples to draw from the approximate posterior.
#' @param output Should `cv` output the training data and fitted models?
#' @inheritParams create_folds
#' @export
cross_validate <- function(sf, type = "LOO", fn, S = 4000, output = FALSE, ...){
  training_sets <- create_folds(sf, remove_cols = c("y", "est"), type = type)
  
  complete <- 0
  total <- length(training_sets)
  fits <- lapply(
    training_sets, 
    FUN = function(training_set) {
      fit <- fn(training_set$data, ...)
      complete <<- complete + 1
      print(paste0(complete, "/", total, " models fit."))
      return(fit)
    }
  )

  for(i in 1:nrow(sf)) training_sets[[i]]$fit <- fits[[i]]
  
  scores <- data.frame(t(sapply(
    training_sets,
    FUN = function(x) held_out_metrics(fit = x$fit, sf = sf, i = x$predict_on, S = S)
  )))

  if(output){
    return(list(scores = scores, training_sets = training_sets))
  }
  else{
    return(list(scores = scores))
  }
}