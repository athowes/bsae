test_sets <- create_folds(mw, type = "LOO")
sf <- test_sets[[1]]$data

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
            m = sf$n_obs)

# Check that CV code works
temp <- rstan::stan(file = "inst/stan/model0.stan", data = dat, warmup = 500, iter = 1000)
rstan::summary(temp)$summary
out <- rstan::extract(temp)
plot(out$y_mis)

# Check that mX_stan works
temp2 <- m1_stan(sf, nsim_warm = 50, nsim_iter = 100)
out <- rstan::extract(temp2)
plot(out$y_mis)
mean(out$y_mis)

a <- m8_stan(sf, nsim_warm = 50, nsim_iter = 100)
b <- m4_stan(mw)
out <- rstan::extract(a)
plot(out$y_mis)
hist(out$y_mis)

temp3 <- rstan::stan(file = "inst/stan/test.stan", data = list(m = 10, rho = 0.5), warmup = 500, iter = 1000)
rstan::summary(temp3)$summary
out <- rstan::extract(temp3)
plot(out$y)
hist(out$y)

# Testing MVN change
temp <- m2_stan(mw, nsim_warm = 50, nsim_iter = 100, method = "morris")
temp2 <- m2_stan(mw, nsim_warm = 50, nsim_iter = 100, method = "default")

