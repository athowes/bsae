test_that("centroid_distance returns a symmetric matrix of the right dimensions, with positive entries", {
  D <- centroid_distance(mw)
  n <- nrow(mw)
  expect_is(D, "matrix")
  expect_equal(dim(D), c(n, n))
  expect_true(all(D >= 0))
  expect_true(isSymmetric(D))
})
  
test_that("centroid_covariance returns a symmetric matrix of the right dimensions, with positive entries", {
  K <- centroid_covariance(mw)
  n <- nrow(mw)
  expect_is(K, "matrix")
  expect_equal(dim(K), c(n, n))
  expect_true(all(K >= 0))
  expect_true(isSymmetric(K))
})

test_that("integrated_covariance returns a symmetric matrix of the right dimensions, with positive entries", {
  K <- integrated_covariance(mw)
  n <- nrow(mw)
  expect_is(K, "matrix")
  expect_equal(dim(K), c(n, n))
  expect_true(all(K >= 0))
  expect_true(isSymmetric(K))
})