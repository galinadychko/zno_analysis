library(testthat)


test_that("Test Epanechnikov(not_correct_input_type)",{
  M <- matrix(NaN, nrow = 2, ncol = 2)
  expect_error(Epanechnikov(M), "Not correct input type")
})


test_that("Test Epanechnikov(different_values)",{
  v <- c(1, 2, 0.5)
  expect_equal(Epanechnikov(v), c(0, 0, 0.5625))
})


test_that("Test output type of Epanechnikov",{
  v <- c(1, 2, 0.5)
  expect_true(is.vector(Epanechnikov(v)))
  expect_true(is.vector(Epanechnikov(1)))
})


test_that("Test nw_any_components(not_correct_input_type)",{
  x <- matrix(NaN, 2, 2)
  X_train <- matrix(NaN, 2, 2)
  Y_train <- matrix(NaN, 2, 2)
  A <- matrix(NaN, 2, 2)
  h <- 1
  expect_error(nw_any_components(x, as.numeric(X_train), as.numeric(Y_train), 1, as.numeric(A)), "Not correct input type")
  expect_error(nw_any_components(x, X_train, as.numeric(Y_train), 1, as.numeric(A)), "Not correct input type")
  expect_error(nw_any_components(x, as.numeric(X_train), Y_train, 1, as.numeric(A)), "Not correct input type")
  expect_error(nw_any_components(x, as.numeric(X_train), as.numeric(Y_train), 1, A), "Not correct input type")
  expect_error(nw_any_components(as.numeric(x), X_train, as.numeric(Y_train), 1, as.numeric(A)), "Not correct input type")
  expect_error(nw_any_components(as.numeric(x), X_train, Y_train, 1, as.numeric(A)), "Not correct input type")
  expect_error(nw_any_components(as.numeric(x), X_train, Y_train, 1, A), "Not correct input type")
  expect_error(nw_any_components(NaN, X_train, Y_train, 1, A), "Not correct input type")
  expect_error(nw_any_components(2, rep(2, 2), rep(2, 2), 1, rep(0, 2)), "Devision by zero")
})


test_that("Test nw_any_components(different_values)",{
  x <- 0
  X_train <- rep(0.5, 2)
  Y_train <- rep(2, 2)
  h <- 1
  A <- rep(1, 2)
  expect_equal(nw_any_components(x, X_train, Y_train, h, A), 2)
  expect_equal(nw_any_components(x, X_train, Y_train, h, matrix(1:4, 2, 2)), c(2, 2))
})


test_that("Test output type nw_any_components",{
  x <- 0
  X_train <- rep(0.5, 2)
  Y_train <- rep(2, 2)
  h <- 1
  A <- rep(1, 2)
  expect_true(is.vector(nw_any_components(x, X_train, Y_train, h, A)))
  expect_true(is.vector(nw_any_components(x, X_train, Y_train, h, matrix(1:4, 2, 2))))
})
