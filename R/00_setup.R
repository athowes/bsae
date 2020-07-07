# Loading packages and setting options

suppressPackageStartupMessages({
  # Wrangling
  library(tidyverse)
  library(magrittr)
  library(rdhs)
  library(sf)
  library(survey)
  library(spdep)
  
  # Modelling
  library(INLA)
  library(rstan) # Sourcing Stan has tendency to break calling C++ from R on my machine (!!)
  library(rstanarm)
  library(TMB)
  library(tmbstan)
  
  # Visualisation
  library(bayesplot)
  library(cowplot)
  library(viridis)
  library(tikzDevice)
  
  options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = TRUE)
})
