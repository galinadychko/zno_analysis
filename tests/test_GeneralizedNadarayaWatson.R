library(testthat)
source("tools/TestsTools.R")
max_threads <<- detectCores() - 1


test_that("Test GeneralisedNadarayaWatson/init", {
  instance <- GeneralisedNadarayaWatson$new()
  expect_null(instance$X_train)
  expect_null(instance$Y_train)
  expect_null(instance$A)
})

test_that("Test GeneralisedNadarayaWatson/train(not_correct_input)", {
  X_train <- cbind(NaN)
  Y_train <- cbind(NaN)
  w_coeff <- c(NaN)
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


test_that("Test types GeneralisedNadarayaWatson/train(correct_input)", {
  X_train <- rep(NaN, 2)
  Y_train <- rep(NaN, 2)
  w_coeff <- wcoeff_two_components(n = 2)
  w_coeff2 <- rbind(c(0.8, 0.1, 0.1), 
                    c(0.05, 0.90, 0.05), 
                    c(0.2, 0.1, 0.7))
  instance <- GeneralisedNadarayaWatson$new()
  instance$train(X_train, Y_train, w_coeff)
  expect_true(is.vector(instance$X_train))
  expect_true(is.vector(instance$Y_train))
  expect_true(is.matrix(instance$A))
})


test_that("Test GeneralisedNadarayaWatson/predict(not_correct_input)", {
  instance <- GeneralisedNadarayaWatson$new()
  expect_error(instance$predict(rep(NaN), 1), "The model was not trained correctly")
  instance <- GeneralisedNadarayaWatson$new()
  instance$X_train <- 1:2
  instance$Y_train <- 1:2
  instance$A <- matrix(NaN, 2, 2)
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


test_that("Test output type GeneralisedNadarayaWatson/predict", {
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
  expect_true(is.matrix(instance$predict(X_test, 1)))
  expect_true(is.matrix(instance$predict(X_test, .5)))
  instance <- GeneralisedNadarayaWatson$new()
  instance$train(c(X_train, 3), c(Y_train, 1), w_coeff2)
  expect_true(is.matrix(instance$predict(X_test2, 1)))
})


test_that("Test GeneralisedNadarayaWatson/predict_in_parallel(not_correct_input)", {
  instance <- GeneralisedNadarayaWatson$new()
  instance$run_cluster(1)
  expect_error(instance$predict_in_parallel(rep(NaN), 1), "The model was not trained correctly")
  instance$stop_cluster()
  
  instance <- GeneralisedNadarayaWatson$new()
  instance$X_train <- 1:2
  instance$Y_train <- 1:2
  instance$A <- matrix(NaN, 2, 2)
  instance$run_cluster(1)
  expect_error(instance$predict(rep(NaN, 1), 1), "The model coefficients are not numbers")
  instance$stop_cluster()
  
  instance$run_cluster()
  expect_error(instance$predict(rep(NaN, max_threads), 1), "The model coefficients are not numbers")
  instance$stop_cluster()
})


test_that("Test GeneralisedNadarayaWatson/predict_in_parallel(correct_input)", {
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
  instance$run_cluster(2)
  expect_equal(instance$predict_in_parallel(X_test, 1), rbind(c(2, 2), c(2, 2)))
  expect_equal(instance$predict_in_parallel(X_test, .5), rbind(c(2, 2), c(2, 2)))
  instance$stop_cluster()
  instance <- GeneralisedNadarayaWatson$new()
  instance$train(c(X_train, 3), c(Y_train, 1), w_coeff2)
  instance$run_cluster(2)
  expect_equal(instance$predict_in_parallel(X_test2, 1), rbind(c(2, 2, 2), c(2, 2, 2)))
  instance$stop_cluster()
})


test_that("Test output type GeneralisedNadarayaWatson/predict_in_parallel", {
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
  instance$run_cluster(2)
  expect_true(is.matrix(instance$predict_in_parallel(X_test, 1)))
  expect_true(is.matrix(instance$predict_in_parallel(X_test, .5)))
  instance$stop_cluster()
  instance <- GeneralisedNadarayaWatson$new()
  instance$train(c(X_train, 3), c(Y_train, 1), w_coeff2)
  instance$run_cluster(2)
  expect_true(is.matrix(instance$predict_in_parallel(X_test2, 1)))
  instance$stop_cluster()
})

