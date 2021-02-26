test_that("dxbinom matches dbinom for integer data (on the regular and log-scales)", {
  p1 <- dxbinom(x = 1, size = 10, prob = 0.1)
  p2 <- dbinom(x = 1, size = 10, prob = 0.1)
  expect_equal(p1, p2)
  l1 <- dxbinom(x = 1, size = 10, prob = 0.1, log = TRUE)
  l2 <- dbinom(x = 1, size = 10, prob = 0.1, log = TRUE)
  expect_equal(l1, l2)
})