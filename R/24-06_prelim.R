prelim_tests <- function(survey) {
  m1_score <- spatial_cross_validate(survey, method = "inla", model = 1)
  m2_score <- spatial_cross_validate(survey, method = "inla", model = 2)
  m4_score <- spatial_cross_validate(survey, method = "inla", model = 4)
  return(list(m1_score = m1_score, m2_score = m2_score, m4_score = m4_score))
}

all <- readRDS("data/all.rds")
unique(all$survey_id)

# Malawi, Zimbabwe, Tanzania and Côte d’Ivoire
malawi <- prelim_tests("MW2015DHS")
zimbabwe <- prelim_tests("ZW2010DHS")

m1_tanzania <- 

tanzania <- prelim_tests("TZ2012AIS")
ivorycoast <- prelim_tests("CI2012DHS")
