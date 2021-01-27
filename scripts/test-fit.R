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
