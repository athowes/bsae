test_that("DIC of a constant model fit using Stan approximately equals that fit using R-INLA", {
  fit_inla <- constant_inla(mw)
  capture.output(fit_stan <- constant_stan(mw, nsim_warm = 500, nsim_iter = 1000))
  expect_equal(dic(fit_inla)$est, dic(fit_stan)$est, tolerance = 0.5)
})

test_that("WAIC of a constant model fit using Stan approximately equals that fit using R-INLA", {
  fit_inla <- constant_inla(mw)
  capture.output(fit_stan <- constant_stan(mw, nsim_warm = 500, nsim_iter = 1000))
  expect_equal(waic(fit_inla)$est, waic(fit_stan)$est, tolerance = 0.5)
})