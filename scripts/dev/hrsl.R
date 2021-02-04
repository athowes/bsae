# Development of non-uniform sampling methods

library(raster)

unzip("~/Documents/phd/data/hrsl_mwi_v1.zip", exdir = "~/Documents/phd/package/bsae/scripts/hrsl")

mw_pop <- raster("scripts/hrsl/hrsl_mwi_pop.tif")
mw_set <- raster("scripts/hrsl/hrsl_mwi_settlement.tif")

mw_pop <- aggregate(mw_pop, fact = 100)
mw_set <- aggregate(mw_set, fact = 100)