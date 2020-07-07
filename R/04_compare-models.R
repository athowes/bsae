# Model comparison via cross-validation and other metrics

spatial_cross_validate <- function(survey, method = "inla", model) {

  source("R/utils.R", local = TRUE)
  source("R/00_setup.R", local = TRUE)
  source("R/02_sae.R", local = TRUE)
  source("R/model_comparison.R", local = TRUE) # Currently the model evaluation code is only written for INLA
  
  df <- readRDS("data/all.rds") %>%
    filter(survey_id == survey, !is.na(est))
  
  # Produce a list of training sets, each with some proportion of the data left out
  training_sets <- lobo(df)
  
  # Fit sae model on each traning set
  fitted_models <- lapply(training_sets, train_run_sae)
  
  # Append the fitted model to training_sets
  cv <- Map(c, training_sets, fitted_models)
  
  scores <- lapply(cv, inla_evaluate_model) %>% unlist()
  sum(scores)
}
