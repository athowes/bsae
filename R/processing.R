#' Mean, standard deviation and posterior interval for `stan` and
#' `R-INLA` models.
#'
#' @param fit Either a `stan` or `R-INLA` model.
#' @param type One of `c("stan", "inla")`.
#' @param name The name of the parameter of interest
#' (only for `type = "stan"`).
#' @return A data frame with containing the posterior mean, standard deviation
#' together with lower (2.5%) and upper (97.5%) quantiles.
#' @examples
#' fit1 <- m1_inla(mw)
#' fit2 <- m1_stan(mw, nsim_warm = 0, nsim_iter = 100)
#' report_interval(fit1, type = "inla")
#' report_interval(fit2, type = "stan", name = "rho")
#' @export
report_interval <- function(fit, type, name = NULL){
  if(!type %in% c("stan", "inla")){
    errorCondition("Choose either stan or inla")
  }
  if(type == "inla"){
    df <- fit$summary.fitted.values %>%
      dplyr::select(mean, sd, lower = "0.025quant", upper = "0.975quant")
  }
  if(type == "stan"){
    df <- data.frame(rstan::summary(fit)$summary) %>%
      tibble::rownames_to_column("param") %>%
      dplyr::filter(substr(param, 1, nchar(name)) == name) %>%
      dplyr::select(mean, sd, lower = X2.5., upper = X97.5.)
  }
  return(df)
}

#' Helper function to extract model fitted and methodology used from a string.
#'
#' @param string A string containing the method name followed by the model
#' name, seperated by a "-". For example `"stan_2"`.
#' @return A list containing `method_name` and `model_name`.
#' @examples
#' get_model_method("inla_1")
#' @export
get_model_method <- function(string){
  x <- strsplit(string, "_")

  method <- x[[1]][1]
  model <- x[[1]][2]

  method <- lookup_method$method[lookup_method$string == method]
  model <- lookup_model[as.numeric(model), 2]

  return(list(method_name = method, model_name = model))
}
