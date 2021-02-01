sf <- mw
L <- 10
type = "random"

n <- nrow(sf)
samples <- sf::st_sample(sf, type = type, exact = TRUE, size = rep(L, n))
S <- sf::st_distance(samples, samples)
sample_index <- sf::st_intersects(sf, samples)
sample_lengths <- lengths(sample_index)
start_index <- sapply(sample_index, function(x) x[1])

# R method
D <- sf::st_distance(samples, samples)
kD <- matern(D, l = 1)
K <- matrix(nrow = n, ncol = n)

for(i in 1:(n - 1)) {
  K[i, i] <- mean(kD[sample_index[[i]], sample_index[[i]]])
  for(j in (i + 1):n) {
    K[i, j] <- mean(kD[sample_index[[i]], sample_index[[j]]]) # Fill the upper triangle
    K[j, i] <- K[i, j] # Fill the lower triangle
  }
}
K[n, n] <- mean(kD[sample_index[[n]], sample_index[[n]]])

cov_r <- K

# Stan (new method)
rstan::expose_stan_functions("inst/stan/integrated.stan")

cov_stan <- cov_sample_average(
  S = S, 
  l = 1, 
  n = n, 
  start_index = start_index, 
  sample_lengths = sample_lengths, 
  total_samples = sum(sample_lengths)
)

# Stan (old method)
rstan::expose_stan_functions("scripts/integrated-old.stan")

cov_stan_old <- cov_sample_average_old(
  n = n, 
  L = 10,
  l = 1,
  S = S
)

# Comparison
cov_r[1:5, 1:5]
cov_stan[1:5, 1:5]
cov_stan_old[1:5, 1:5]
