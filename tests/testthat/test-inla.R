test_that("the constant model fits using R-INLA", {
  fit <- constant_inla(mw)
  expect_s3_class(fit, "inla")
})