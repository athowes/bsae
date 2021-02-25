test_that("the Best length-scale method gives a higher length-scale for higher average correlation", {
  D <- centroid_distance(mw)
  l1 <- best_average(D, p = 0.01)
  l2 <- best_average(D, p = 0.02)
  expect_gt(l2, l1)
})