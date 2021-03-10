test_that("get_scale, riebler_gv, scale_gmrf_precision agree for singular and when applicable non-singular precisions", {
  Q_singular <- matrix(c(1, -1, -1, 1), nrow = 2, ncol = 2) # Besag precision for connected pair
  Q_singular_inv <- matrix(c(0.25, -0.25, -0.25, 0.25), nrow = 2, ncol = 2)
  Q <- matrix(c(2, -1, -1, 2), nrow = 2, ncol = 2) # Non-singular precision
  Q_inv <- solve(Q)
  expect_equal(get_scale(Q_singular), riebler_gv(Q_singular_inv))
  expect_equal(riebler_gv(Q_singular_inv), scale_gmrf_precision(Q_singular)$scales)
  expect_equal(get_scale(Q), riebler_gv(Q_inv))
})

test_that("scale_gmrf_precision works as expected for two connected components", {
  # Besag precision for a pair of connected pairs
  Q_singular <- matrix(c(1, -1, 0, 0, -1, 1, 0, 0, 0, 0, 1, -1, 0, 0, -1, 1), nrow = 4, ncol = 4)
  Q_scaled <- 0.25 * Q_singular
  expect_equal(scale_gmrf_precision(Q_singular)$scales, c(0.25, 0.25))
  expect_equal(scale_gmrf_precision(Q_singular)$Q, Q_scaled)
})
