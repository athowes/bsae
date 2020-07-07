# Download the GADM shapefiles (sf: simple features) for Sub-Saharan Africa

library(tidyverse)
library(rdhs)

ssa <- dhs_countries() %>%
  filter(RegionName == "Sub-Saharan Africa") %>%
  dplyr::select(iso3 = ISO3_CountryCode) %>%
  filter(iso3 != "") # Remove a blank

gadm_download <- function(admin = 1, countries) {
  
  urls <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_", 
                 countries, "_", admin, "_sf.rds")
  
  dests <- paste0("~/Documents/phd/data/gadm36_", 
                  countries, "_", admin, "_sf.rds")

  for (i in 1:length(urls)) {
    download.file(url = urls[i], dests[i])
  }
} # To-do: add TryCatch

gadm_download(admin = 1, countries = ssa$iso3)
gadm_download(admin = 2, countries = ssa$iso3)

