# Test of cross-validation using missing data in Stan

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

temp <- rstan::stan(file = "inst/stan/model0.stan", data = dat, warmup = 100, iter = 900)
rstan::summary(temp)$summary
out <- rstan::extract(temp)
plot(out$y_mis)
