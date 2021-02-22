test_that("create_folds creates training sets of the right length", {
  training_sets <- create_folds(mw, type = "LOO")
  n <- nrow(mw)
  expect_length(training_sets, n)
})