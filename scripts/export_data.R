library(geojson)
countries <- c("ci", "mw", "tz", "zw")

for (country in countries) {
  # load data
  load(paste("data/", country, ".rda", sep = ""))
  data <- as.geojson(eval(as.symbol(country)))
  geo_write(data, paste("data/export_data/", country, ".geojson", sep=""))
  
}
