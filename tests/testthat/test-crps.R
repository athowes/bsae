test_that("The CRPS of constant draws from a constant is zero, and as the draws become more variable the CRPS increases", {
  zero <- crps(samples = rep(0, 100), true_value = 0)
  one <- crps(samples = rnorm(100, 0, 1), true_value = 0)
  two <- crps(samples = rep(100, 0, 2), true_value = 0)
  expect_equal(zero, 0)
  expect_gt(two, one)
})