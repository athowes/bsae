# Development of variogram methods
# Aim to automatrically determine length-scale

library(ggplot2)
library(gstat)

# Centroids
cent_mw <- sf::st_centroid(mw)
cent_mw

# Size of point corresponds to estimate
ggplot(mw) +
  geom_sf(fill = "lightgrey") +
  geom_sf(data = cent_mw, aes(size = est)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  labs(fill = "") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

vario <- variogram(est ~ 1, cent_mw)
plot(vario)
# np: the number of point pairs for this estimate
# dist: the average distance of all point pairs considered for this estimate
# gamma: the actual sample variogram estimate

fit <- fit.variogram(vario, model = vgm(psill = 1, 
                                        model = "Mat", 
                                        range = 300, 
                                        kappa = 1.5))
# psill: (partial) sill of the variogram model component
# model: model type, e.g. "Exp", "Sph", "Gau", "Mat"
# range: range parameter of the variogram model component; in case of anisotropy: major range
# kappa: smoothness parameter for the Matern class of variogram models
