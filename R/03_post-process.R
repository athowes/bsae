# Function to create a list of already fitted models for a particular survey

list_of_models <- function(survey) {
  
  source("R/00_setup.R", local = TRUE)
  source("R/utils.R", local = TRUE)
  
  # All of the relevant file location strings
  fits <- tools::file_path_sans_ext(list.files(paste0("results/", survey)))
  
  # Read them all in and put into a list
  all_models <- lapply(list.files(paste0("results/", survey), full.names = TRUE), readRDS)
  names(all_models) <- fits
  
  # Extract the name of the model and the inferential method and append this information
  meta <- lapply(fits, get_model_method)
  all_models <- Map(c, all_models, meta)
  
  saveRDS(all_models, file = paste0("results/", survey, "/all_models.rds"))
  
  return(all_models)
}

# Example
# list_of_models(survey = "MW2015DHS")
