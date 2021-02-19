test_that("the constant model fits using Stan", {
  fit <- constant_stan(mw, nsim_warm = 200, nsim_iter = 400)
  expect_s4_class(fit, "stanfit")
})