#' Function to fit single model.
#'
#' @param sf A simple features data frame.
#' @param fn A fitting function.
#' @return A fitted model.
fit_model <- function(sf, fn, ...) {
  fn(sf, ...)  
}

#' Function to fit list of models.
#'
#' @param sf A simple features data frame.
#' @param fns A list of fitting functions.
#' @return A list of fitted models.
all_fit <- function(sf, fns) {
  res <- lapply(fns, fit_model, sf = sf)
  return(res)
}

all_cv <- function(sf, type = "LOO") {
  fns <- list(m0_inla, m1_inla, m2_inla, m3_inla, m4_inla)
  res <- lapply(fns, cv, sf = sf, type = type)
  return(res)
}

run_comparison <- function(id, fns){
  sf <- hiv_surveys %>% filter(survey_id == id)
  
  fits_unnamed <- all_fit(sf, fns)
  # names <- as.list(head(as.character(lookup_model$model), n = 5))
  fits <- Map(c, fits_unnamed, name = names)
  message("Completed model fitting")
  
  criteria <- lapply(fits, inla_info_criteria)
  # names(criteria) <- names
  df <- do.call(rbind, lapply(criteria, data.frame))
  message("Completed model comparison")
  
  loo_results <- all_cv(sf, type = "LOO")
  df$LOOCV <- unlist(lapply(loo_results, function(x) -2 * sum(x)))
  message("Completed LOO")
  
  sloo_results <- all_cv(sf, type = "SLOO")
  df$SLOOCV <- unlist(lapply(sloo_results, function(x) -2 * sum(x)))
  message("Completed SLOO")
  
  return(
    list(fits = fits,
         df = df,
         loo_results = loo_results,
         sloo_results = sloo_results)
    )
}