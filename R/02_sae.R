# Function to fit small area estimation models

run_sae <- function(survey, method, model, save = FALSE, df = NULL) {
  
  source("R/utils.R", local = TRUE)
  source("R/00_setup.R", local = TRUE)
  
  if(is.null(df)) {
    # Subset to only the relevant data
    # Should missing data be removed?
    df <- readRDS("data/all.rds") %>%
      filter(survey_id == survey, !is.na(est)) 
  }
  
  # MCMC settings
  nsim_warm <- 100
  nsim_iter <- 1000

  # Import model functions
  source("R/02-1.R", local = TRUE) # IID
  source("R/02-2.R", local = TRUE) # Besag
  source("R/02-3.R", local = TRUE) # Border ICAR
  source("R/02-4.R", local = TRUE) # BYM2
  source("R/02-5.R", local = TRUE) # Centroid MVN
  source("R/02-6.R", local = TRUE) # IID + MVN
  
  fun_list <- c(m1, m2, m3, m4, m5, m6)
  
  # Use strings to only run the model functions matching both method and model
  match_method <- grepl(paste(method, collapse = "|"), names(fun_list))
  match_model <- grepl(paste(model, collapse = "|"), names(fun_list))
  chosen_funs <- fun_list[(match_method & match_model)]

  lapply(chosen_funs, function(f) f(save = save))
}

# Example
my_models <- run_sae(survey = "MW2015DHS", method = "inla", model = c(1, 2), save = FALSE)
