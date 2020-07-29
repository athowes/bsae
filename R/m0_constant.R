#' Fit constant Small Area Estimation model using `R-INLA`.
#'
#' Simply fits a constant (the mean). This is useful as a benchmark
#' for other models.
#'
#' @param sf A simple features object with some geometry.
#' @examples
#' fit <- m0_inla(mw)
#' mean <- logistic(fit$summary.fixed$mean)
m0_inla <- function(sf){
  
  dat <- list(id = 1:nrow(sf),
              y = round(sf$y),
              m = sf$n_obs)
  
  formula <- y ~ 1
  
  fit <- INLA::inla(formula,
                    family = "binomial",
                    control.family = list(control.link = list(model = "logit")),
                    data = dat,
                    Ntrials = m,
                    control.predictor = list(compute = TRUE, link = 1),
                    control.compute = list(dic = TRUE, waic = TRUE,
                                           cpo = TRUE, config = TRUE))
  
  return(fit)
}