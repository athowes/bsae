# Visualise the different ways to sample points using sf::st_sample depending on the type argument

library(ggplot2)

plot_samples <- function(samples){
ggplot(mw) +
  geom_sf(fill = "lightgrey") +
  geom_sf(data = samples, alpha = 0.5, shape = 4) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  labs(fill = "") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
}

L <- 10
n <- nrow(mw)

# type = "random" (the default)
random <- sf::st_sample(mw, size = rep(L, n))
length(random)
plot_samples(random)

# type = "hexagonal"
hexagonal <- sf::st_sample(mw, size = rep(L, n), type = "hexagonal", exact = TRUE)
length(hexagonal)
plot_samples(hexagonal)

# type = "regular"
regular <- sf::st_sample(mw, size = rep(L, n), type = "regular", exact = TRUE)
length(regular)
plot_samples(regular)

# Sampling methods from package spatstat are interfaced!
# https://cran.r-project.org/web/packages/spatstat/spatstat.pdf