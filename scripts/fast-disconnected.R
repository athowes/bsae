library(dplyr)
library(ggplot2)

sf <- mw

scale_gmrf_precision <- function(nb, A = matrix(1, 1, nrow(Q))){
  Q <- nb_to_precision(nb)
  comp <- spdep::n.comp.nb(nb)
  scales <- rep(NA, comp$nc)
  for (k in seq_len(comp$nc)) {
    idx <- which(comp$comp.id == k)
    Qc <- Q[idx, idx, drop = FALSE]
    if (length(idx) == 1) {
      scales[k] <- 1 / Qc[1, 1]
      Qc[1, 1] <- 1 # Marginal variance for singletons to be 1
    } else {
      Ac <- A[ , idx, drop = FALSE]
      scaling_factor <- get_scale(Qc, constraint = list(A = Ac, e = 0))
      scales[k] <- scaling_factor
      Qc <- scaling_factor * Qc
    }
    Q[idx, idx] <- Qc
  }
  return(list(Q = Q, scales = scales))
}

nb <- neighbours(sf)

Q <- nb_to_precision(nb)
plot_matrix(Q)

nb <- neighbours(mw)
result <- scale_gmrf_precision(nb)
result$scales

comp <- spdep::n.comp.nb(nb)

sf

ordered_sf <- sf %>%
  mutate(comp.id = comp$comp.id) %>%
  arrange(comp.id)

ordered_nb <- neighbours(ordered_sf)
ordered_g <- nb_to_graph(ordered_nb)
ordered_result <- scale_gmrf_precision(ordered_nb)
ordered_comp <- spdep::n.comp.nb(ordered_nb)
ordered_Q <- nb_to_precision(ordered_nb)

plot_matrix(ordered_Q)

rle <- rle(comp$comp.id[order(comp$comp.id)])
scales <- ordered_result$scales[ordered_comp$comp.id]
  
dat <- list(n = nrow(ordered_sf),
            y = round(ordered_sf$y),
            m = round(ordered_sf$n_obs),
            nc = comp$nc,
            group_sizes = rle$lengths,
            scales = scales,
            n_edges = ordered_g$n_edges,
            node1 = ordered_g$node1,
            node2 = ordered_g$node2)

temp <- rstan::stan(file = "scripts/fast-disconnected.stan", data = dat, warmup = 500, iter = 1000)
out <- rstan::extract(temp)
summary <- rstan::summary(temp)$summary
summary
