library(testthat)
source("tools/TestsTools.R")


test_that("Test GeneralisedNadarayaWatson/init", {
  instance <- GeneralisedNadarayaWatson$new()
  expect_identical(instance$X_train, NULL)
  expect_identical(instance$Y_train, NULL)
  expect_identical(instance$A, NULL)
})

test_that("Test GeneralisedNadarayaWatson/train(not_correct_input)", {
  X_train <- cbind(NA)
  Y_train <- cbind(NA)
  w_coeff <- c(NA)
  instance <- GeneralisedNadarayaWatson$new()
  expect_error(instance$train(X_train, Y_train, w_coeff), "Not correct class atributes")
  expect_error(instance$train(as.numeric(X_train), Y_train, w_coeff), "Not correct class atributes")
  expect_error(instance$train(X_train, as.numeric(Y_train), w_coeff), "Not correct class atributes")
  expect_error(instance$train(X_train, Y_train, as.matrix(w_coeff)), "Not correct class atributes")
  expect_error(instance$train(rep(NaN, 2), rep(NaN, 2), matrix(NaN, 1, 2)), "Not correct input dimensions")
  expect_error(instance$train(rep(NaN, 1), rep(NaN, 2), matrix(NaN, 2, 2)), "Not correct input dimensions")
  expect_error(instance$train(rep(NaN, 1), rep(NaN, 1), matrix(NaN, 2, 2)), "Not correct input dimensions")
})


test_that("Test GeneralisedNadarayaWatson/train(correct_input)", {
  X_train <- rep(NaN, 2)
  Y_train <- rep(NaN, 2)
  w_coeff <- wcoeff_two_components(n = 2)
  w_coeff2 <- rbind(c(0.8, 0.1, 0.1), 
                    c(0.05, 0.90, 0.05), 
                    c(0.2, 0.1, 0.7))
  instance <- GeneralisedNadarayaWatson$new()
  instance$train(X_train, Y_train, w_coeff)
  expect_equal(instance$X_train, rep(NaN, 2))
  expect_equal(instance$Y_train, rep(NaN, 2))
  expect_equal(instance$A, cbind(c(0, 2), c(4, -2)))
  
  instance <- GeneralisedNadarayaWatson$new()
  instance$train(rep(NaN, 3), rep(NaN, 3), w_coeff2)
  expect_equal(instance$X_train, rep(NaN, 3))
  expect_equal(instance$Y_train, rep(NaN, 3))
  expect_equal(instance$A, cbind(c(3.90625, -0.37500, -0.53125), 
                                 c(-0.15625, 3.37500, -0.21875), 
                                 c(-1.09375, -0.37500, 4.46875)))
})


test_that("Test GeneralisedNadarayaWatson/predict(not_correct_input)", {
  instance <- GeneralisedNadarayaWatson$new()
  expect_error(instance$predict(rep(NaN), 1), "The model was not trained correctly")
  instance <- GeneralisedNadarayaWatson$new()
  instance$X_train <- 1:2
  instance$Y_train <- 1:2
  instance$A <- matrix(NA, 2, 2)
  expect_error(instance$predict(rep(NaN, 1), 1), "The model coefficients are not numbers")
})


test_that("Test GeneralisedNadarayaWatson/predict(correct_input)", {
  X_train <- rep(0.5, 2)
  Y_train <- rep(2, 2)
  X_test <- rep(0.5, 2)
  X_test2 <- c(0.5, 0.7)
  w_coeff <- wcoeff_two_components(n = 2)
  w_coeff2 <- rbind(c(0.8, 0.1, 0.1),
                    c(0.05, 0.90, 0.05),
                    c(0.2, 0.1, 0.7))
  instance <- GeneralisedNadarayaWatson$new()
  instance$train(X_train, Y_train, w_coeff)
  expect_equal(instance$predict(X_test, 1), rbind(c(2, 2), c(2, 2)))
  expect_equal(instance$predict(X_test, .5), rbind(c(2, 2), c(2, 2)))
  instance <- GeneralisedNadarayaWatson$new()
  instance$train(c(X_train, 3), c(Y_train, 1), w_coeff2)
  expect_equal(instance$predict(X_test2, 1), rbind(c(2, 2, 2), c(2, 2, 2)))
})
