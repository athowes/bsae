fit_model <- function(mw, fn, ...) {
  fn(mw, ...)  
}

test_class <- function(fn, class, ...) {
  t <- fit_model(mw, fn, ...)
  return(class(t) == class)
}

# Is the class of the fit object correct?

test_class(m0_inla, "inla")
test_class(m1_inla, "inla")
test_class(m2_inla, "inla")
test_class(m3_inla, "inla")
test_class(m4_inla, "inla")
test_class(m5_inla, "inla")
test_class(m6_inla, "inla")

test_class(m0_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m1_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m2_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m3_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m4_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m5_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m5_stan, bym2 = TRUE, nsim_warm = 0, nsim_iter = 10, class = "stanfit")
test_class(m6_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m6_stan, bym2 = TRUE, type = "random", nsim_warm = 0, nsim_iter = 10, class = "stanfit")
test_class(m7_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m7_stan, bym2 = TRUE, nsim_warm = 0, nsim_iter = 10, class = "stanfit")
test_class(m8_stan, "stanfit", nsim_warm = 0, nsim_iter = 10)
test_class(m8_stan, bym2 = TRUE, "stanfit", nsim_warm = 0, nsim_iter = 10)

# Diagnostics for Stan models

t <- m8_stan(mw, type = "regular", nsim_warm = 0, nsim_iter = 100)

m8_stan_old <- function(sf, L = 10, type = "regular", nsim_warm = 100, nsim_iter = 1000){
  n <- nrow(sf)
  samples <- sf::st_sample(sf, type = type, exact = TRUE, size = rep(L, n))
  S <- sf::st_distance(samples, samples)

  ii_obs <- which(!is.na(sf$y))
  ii_mis <- which(is.na(sf$y))
  n_obs <- length(ii_obs)
  n_mis <- length(ii_mis)

  dat <- list(n_obs = n_obs,
              n_mis = n_mis,
              ii_obs = array(ii_obs),
              ii_mis = array(ii_mis),
              n = nrow(sf),
              y_obs = sf$y[ii_obs],
              m = sf$n_obs,
              mu = rep(0, nrow(sf)),
              L = L,
              S = S)
  
  rstan::stan(file = "scripts/integrated-old.stan", 
              data = dat, 
              warmup = nsim_warm, 
              iter = nsim_iter,
              control = list(adapt_delta = 0.99))
}

t <- m8_stan_old(mw, nsim_warm = 0, nsim_iter = 10)
