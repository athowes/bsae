# Test of sampling from the distribution produced by custom likelihood xbinomial_lpdf

temp <- rstan::stan(file = "scripts/xbinom.stan", data = list(m = 10, rho = 0.5), warmup = 100, iter = 900, verbose = TRUE)
rstan::summary(temp)$summary
out <- rstan::extract(temp)
plot(out$y)
hist(out$y)
