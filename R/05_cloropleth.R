# Cloropleths
source("src/00_setup.R")
source("src/utils.R")

survey <- "MW2015DHS"
all_models <- readRDS(file = paste0("results/", survey, "/all_models.rds"))

# Posterior mean and standard deviation
extract_results <- function(fit){
  
  if(fit$method_name == "Stan"){
    intervals <- stan_interval(fit[[1]], "rho")
  }
  
  else if(fit$method_name == "INLA"){
    intervals <- inla_interval(fit)
  }
  
  else{
    errorCondition("Fitting method not recognised (should be either Stan or INLA)")
  }
  
  readRDS("data/all.rds") %>%
    filter(survey_id == survey) %>%
    mutate(rho = intervals$mean, sd = intervals$sd, lower = intervals$lower, upper = intervals$upper,
           model = fit$model_name, method = fit$method_name)
}

all_results <- lapply(all_models, extract_results)
all_results <- do.call(rbind, all_results)

# Add the raw data to the list of results
raw <- readRDS("data/all.rds") %>%
  filter(survey_id == survey) %>%
  mutate(rho = est, sd = NA, lower = est, upper = est, model = "Raw data", method = NA)

all_results <- rbind(all_results, raw)

# Posterior mean estimates: so far they all look pretty similar
all_results %>%
  filter(method %in% c("INLA", "Stan")) %>%
  ggplot(aes(fill = rho)) +
  geom_sf(aes(geometry = geometry)) +
  facet_grid(method~model) +
  coord_sf() +
  scale_fill_viridis() +
  labs(title = "Prevalence: posterior mean estimates",
       fill = "Estimate")

# Posterior standard deviation estimates
all_results %>%
  filter(method %in% c("INLA", "Stan")) %>%
  ggplot(aes(fill = sd)) +
  geom_sf(aes(geometry = geometry)) +
  facet_grid(method~model) +
  coord_sf() +
  scale_fill_viridis(option = "plasma") +
  labs(title = "Prevalence: posterior standard deviation estimates",
       fill = "Standard deviation")

# Ladder plots
# http://tsitsul.in/blog/coloropt/
palette_13 <- c("#ebac23", "#b80058", "#008cf9", "#006e00", "#00bbad", "#d163e6", 
                "#b24502", "#ff9287", "#5954d6", "#00c6f8", "#878500", "#00a76c", "#bdbdbd")

all_results %>%
  filter(method %in% c("INLA", NA)) %>%
  ggplot(aes(x = reorder(name_1, rho), y = rho, ymin = lower, ymax = upper, 
             group = model, color = model)) +
  # Warning: it might either be name_1 or name_2 that you want! (Could be automated?)
  geom_pointrange(position = position_dodge(width = 0.6), alpha = 0.8) +
  labs(title = "Models fit using INLA", x = "District", y = "Posterior prevalence estimate") +
  theme_minimal() +
  scale_color_manual(values = palette_13) +
  coord_flip()
