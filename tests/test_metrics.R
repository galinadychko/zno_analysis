library(testthat)

test_that("Test weighted_MSE(not_correct_value)", {
  Y_true <- cbind(NaN)
  Y_predicted <- cbind(NaN)
  A_coeff <- cbind(NaN)
  expect_error(weighted_MSE(Y_true, Y_predicted, A_coeff), "Not correct input type")
  expect_error(weighted_MSE(as.vector(Y_true), Y_predicted, A_coeff), "Not correct input type")
  expect_error(weighted_MSE(as.vector(Y_true), as.vector(Y_predicted), A_coeff), "Not correct input type")
  expect_error(weighted_MSE(as.vector(Y_true), as.data.frame(Y_predicted), as.vector(A_coeff)), "Not correct input type")
  expect_error(weighted_MSE(as.vector(Y_true), as.vector(Y_predicted), list(NA)), "Not correct input type")
})


test_that("Test weighted_MSE(not_correct_type)", {
  expect_error(weighted_MSE(c("string"), c("string"), c("string")), "Not correct input type")
  expect_error(weighted_MSE(c(NaN), c("string"), c("string")), "Not correct input type")
  expect_error(weighted_MSE( c("string"), NaN, NaN), "Not correct input type")
})


test_that("Test weighted_MSE(not_correct_input_dimension)", {
  Y_true <- rep(NaN, 1)
  Y_predicted <- rep(NaN, 2)
  A_coeff <- rep(NaN, 1)
  expect_error(weighted_MSE(Y_true, Y_predicted, A_coeff), "Not correct imput dimensions")
  expect_error(weighted_MSE(c(Y_true, NaN), Y_predicted, A_coeff), "Not correct imput dimensions")
  expect_error(weighted_MSE(Y_true, Y_predicted, c(A_coeff, NaN)), "Not correct imput dimensions")
})


test_that("Test weighted_MSE(correct_value)", {
  Y_true <- c(1, 1)
  Y_predicted <- c(1, 1)
  A_coeff <- c(1, 1)
  expect_equal(weighted_MSE(Y_true, Y_predicted, A_coeff), 0)
  expect_equal(weighted_MSE(c(Y_true, 1), c(Y_predicted, 0),c( A_coeff, 0)), 0)
  expect_equal(round(weighted_MSE(c(Y_true, 1), c(Y_predicted, 0),c( A_coeff, 1)), 2), 0.33)
  expect_equal(weighted_MSE(c(Y_true, 1, 1), c(Y_predicted, 0, 0),c( A_coeff, 1, 0.5)), 0.375)
})


test_that("Test output type weighted_MSE(correct_value)", {
  Y_true <- c(1, 1)
  Y_predicted <- c(1, 1)
  A_coeff <- c(1, 1)
  expect_true(is.vector(weighted_MSE(Y_true, Y_predicted, A_coeff), mode = "numeric"))
  expect_true(is.vector(weighted_MSE(c(Y_true, 1), c(Y_predicted, 0),c( A_coeff, 0)), mode = "numeric"))
  expect_true(is.vector(round(weighted_MSE(c(Y_true, 1), c(Y_predicted, 0),c( A_coeff, 1)), 2), mode = "numeric"))
  expect_true(is.vector(weighted_MSE(c(Y_true, 1, 1), c(Y_predicted, 0, 0),c( A_coeff, 1, 0.5)), mode = "numeric"))
})

