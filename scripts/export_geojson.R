library(geojson)
countries <- c("ci", "mw", "tz", "zw")

for (country in countries) {
  # Load data
  load(paste("data/", country, ".rda", sep = ""))
  data <- as.geojson(eval(as.symbol(country)))
  geo_write(data, paste("data/geojson/", country, ".geojson", sep = ""))
}
