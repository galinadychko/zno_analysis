library(testthat)

test_that("Test weighted_MSE(not_correct_value)", {
  Y_true <- cbind(NaN)
  Y_predicted <- cbind(NaN)
  A_coeff <- cbind(NaN)
  expect_error(weighted_MSE(Y_true, Y_predicted, A_coeff), "Not correct input type")
  expect_error(weighted_MSE(as.vector(Y_true), as.vector(Y_predicted), A_coeff), "Not correct input type")
  expect_error(weighted_MSE(as.vector(Y_true), Y_predicted, as.vector(A_coeff)), "Not correct input type")
  expect_error(weighted_MSE(as.vector(Y_true), as.data.frame(Y_predicted), A_coeff), "Not correct input type")
  expect_error(weighted_MSE(as.vector(Y_true), as.list(Y_predicted), list(NA)), "Not correct input type")
})


test_that("Test weighted_MSE(not_correct_type)", {
  expect_error(weighted_MSE(c("string"), matrix("string"), matrix("string")), "Not correct input type")
  expect_error(weighted_MSE( c("string"), matrix(NaN), matrix(NaN)), "Not correct input type")
})


test_that("Test weighted_MSE(not_correct_input_dimension)", {
  Y_true <- rep(NaN, 1)
  Y_predicted <- matrix(NaN, 2)
  A_coeff <- matrix(NaN, 1)
  expect_error(weighted_MSE(Y_true, Y_predicted, A_coeff), "Not correct input dimensions")
  expect_error(weighted_MSE(c(Y_true, NaN), Y_predicted, A_coeff), "Not correct input dimensions")
  expect_error(weighted_MSE(Y_true, Y_predicted, rbind(A_coeff, NaN)), "Not correct input dimensions")
})


test_that("Test weighted_MSE(correct_value)", {
  Y_true <- c(1, 1)
  Y_predicted <- rbind(c(1, 1, 1), c(1, 1, 1))
  A_coeff <- rbind(c(1, 1, 1), c(1, 1, 1))
  expect_equal(weighted_MSE(Y_true, Y_predicted, A_coeff), c(0, 0, 0))
  expect_equal(weighted_MSE(c(Y_true, 1), rbind(Y_predicted, c(1, 0, 0)), rbind(A_coeff, c(1, 0.5, 0.5))), c(0, 0.5/3, 0.5/3))
  expect_equal(weighted_MSE(c(Y_true, 1), rbind(Y_predicted, c(0, 0, 1)), rbind(A_coeff, c(0.5, 0.5, 0.5))), c(0.5/3, 0.5/3, 0))
})


test_that("Test output type weighted_MSE(correct_value)", {
  Y_true <- c(1, 1)
  Y_predicted <- rbind(c(1, 1, 1), c(1, 1, 1))
  A_coeff <- rbind(c(1, 1, 1), c(1, 1, 1))
  expect_true(is.vector(weighted_MSE(Y_true, Y_predicted, A_coeff), mode = "numeric"))
  expect_true(is.vector(weighted_MSE(c(Y_true, 1), rbind(Y_predicted, c(1, 0, 0)), rbind(A_coeff, c(1, 0.5, 0.5))), mode = "numeric"))
  expect_true(is.vector(weighted_MSE(c(Y_true, 1), rbind(Y_predicted, c(0, 0, 1)), rbind(A_coeff, c(0.5, 0.5, 0.5))), mode = "numeric"))
})
