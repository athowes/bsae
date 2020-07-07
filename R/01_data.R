source("R/00_setup.R")

# Small edits to data/prev-15to49-dhs-gadm.rds to create data/all.rds
data <- readRDS("data/prev-15to49-dhs-gadm.rds")
names(data) <- c("survey_id", "gid_0", "name_0", "gid_1", "name_1", "gid_2", "name_2", "n_cluster", "n_obs", "est", "se", "geometry")

# Note: could calculate l_prev = qlogis(est) and l_prev_se = se / (est * (1 - est)) via delta method
data %>% 
  st_as_sf() %>%
  mutate(y = est * n_obs) %>% # y: total number of cases (ignores survey design)
  saveRDS("data/all.rds")
