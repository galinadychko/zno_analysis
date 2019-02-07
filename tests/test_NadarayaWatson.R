library(testthat)
source("tools/TestsTools.R")


test_that("Test Epanechnikov(not_correct_input_type)",{
  M <- matrix(NA, nrow = 2, ncol = 2)
  expect_error(Epanechnikov(M), "Not correct input type")
})


test_that("Test Epanechnikov(different_values)",{
  v <- c(1, 2, 0.5)
  expect_equal(Epanechnikov(v), c(0, 0, 0.5625))
})
