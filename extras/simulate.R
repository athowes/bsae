inv_logit <- function(x) { 
  exp(x) / (1 + exp(x))
}

# Algorithm 3.1 from Rue and Held (Sampling from an improper GMRF with mean zero)
igmrf <- function(Q){
  v <- eigen(Q, TRUE)
  inv <- sqrt(ifelse(v$values > sqrt(.Machine$double.eps), 1/v$values, 0))
  sim <- v$vectors %*% diag(inv) %*% rnorm(dim(Q)[1], 0, 1)
  X <- rep(1, length(sim))
  if(sum(inv == 0) == 2) {
    X <- cbind(X, 1:length(sim))
  }
  sim <- sim - X %*% solve(crossprod(X), crossprod(X, sim))
  return(sim)
}

# Adapted from Appendix A of "Joint SAE of HIV prevalence, ART coverage, and HIV incidence"
simulate_sae1 <- function(n = 28, m = 200, l_rho_0 = -2.4, sigma_l_rho = 0.5) {
  l_rho <- rnorm(n, l_rho_0, sigma_l_rho)
  y <- rbinom(n, m, inv_logit(l_rho))
  list(y = y, l_rho = l_rho)
}

simulate_sae2 <- function(Q, m, l_rho_0 = -2.4) {
  l_rho <- igmrf(Q) + l_rho_0
  y <- rbinom(dim(Q)[1], m, inv_logit(l_rho))
  list(y = y, l_rho = l_rho)
}

df <- readRDS("data/prev_malawi_2015.rds") %>% 
  st_as_sf() %>%
  select(name_1, n_obs, geometry) %>%
  mutate(sim = simulate_sae2(Q = Q1_scaled, m = df$n_obs)$y,
         est = sim / n_obs)

df %>%
  ggplot(aes(fill = est)) +
  geom_sf(aes(geometry = geometry)) +
  coord_sf() +
  scale_fill_viridis(option = "viridis")
