library(testthat)
source("tools/TestsTools.R")


test_that("Test Gramm_matrix(not_matrix)",{
  expect_error(Gramm_matrix(c(NaN, NaN)), "Not appropriate input format")
  expect_error(Gramm_matrix(data.frame(NaN, NaN)), "Not appropriate input format")
  expect_error(Gramm_matrix(list(NaN, NaN)), "Not appropriate input format")
  expect_error(Gramm_matrix(factor(NaN, NaN)), "Not appropriate input format")
  expect_error(Gramm_matrix("String"), "Not appropriate input format")
})


test_that("Test output type of Gramm_matrix", {
  w1 <- wcoeff_two_components(1)
  w2 <- wcoeff_two_components(2)
  w4 <- wcoeff_three_components(4)
  expect_is(Gramm_matrix(w1), "matrix")
  expect_is(Gramm_matrix(w2), "matrix")
  expect_is(Gramm_matrix(w4), "matrix")
})


test_that("Test Gramm_matrix(one_component_mixture)",{
  expect_error(Gramm_matrix(rbind(0, 1)), "Not correct mixture")
})


test_that("Test Gramm_matrix(two_component_mixture)",{
  w1 <- wcoeff_two_components(1)
  w2 <- wcoeff_two_components(2)
  w4 <- wcoeff_two_components(4)
  expect_equal(Gramm_matrix(w1), rbind(c(1, 0), c(0, 0)))
  expect_equal(Gramm_matrix(w2), cbind(c(0.625, 0.125), c(0.125, 0.125)))
  expect_equal(Gramm_matrix(w4), cbind(c(0.46875, 0.15625), c(0.15625, 0.21875)))
})


test_that("Test Gramm_matrix(three_component_mixture)",{
  w1 <- wcoeff_three_components(1)
  w2 <- wcoeff_three_components(2)
  w4 <- wcoeff_three_components(4)
  expect_equal(Gramm_matrix(w1), rbind(c(1, 0, 0), c(0, 0, 0), c(0, 0, 0)))
  expect_equal(Gramm_matrix(w2), cbind(c(0.625, 0.0625, 0.0625), c(0.0625, 0.03125, 0.03125), c(0.0625, 0.03125, 0.03125)))
  expect_equal(Gramm_matrix(w4), cbind(c(0.46875, 0.078125, 0.078125), c(0.078125, 0.0546875, 0.0546875), c(0.078125, 0.0546875, 0.0546875)))
})


test_that("Test minor(not_matrix)",{
  expect_error(minor(c(NaN, NaN), 1, 1), "Not appropriate input format")
  expect_error(minor(data.frame(NaN, NaN), 1, 1), "Not appropriate input format")
  expect_error(minor(list(NaN, NaN), 1, 1), "Not appropriate input format")
  expect_error(minor(factor(NaN, NaN), 1, 1), "Not appropriate input format")
  expect_error(minor("String", 1, 1), "Not appropriate input format")
})


test_that("Test minor(matrix, not_correct_i_j)",{
  M <- matrix(NaN, nrow = 2, ncol = 2)
  expect_error(minor(M, 0, 0), "Not correct i,j")
  expect_error(minor(M, 1, 0), "Not correct i,j")
  expect_error(minor(M, 0, 1), "Not correct i,j")
  expect_error(minor(M, -1, 0), "Not correct i,j")
  expect_error(minor(M, 0, -1), "Not correct i,j")
  expect_error(minor(M, -1, -1), "Not correct i,j")
  expect_error(minor(M, -1, 1), "Not correct i,j")
  expect_error(minor(M, 1, -1), "Not correct i,j")
})


test_that("Test minor(from_different_parameters)",{
  M1 <- matrix(1:4, ncol = 2, nrow = 2)
  M2 <- matrix(1:9, ncol = 3, nrow = 3)
  expect_equal(minor(M1, 1, 1), 4)
  expect_equal(minor(M2, 2, 3), -6)
})


test_that("Test output type of minor",{
  M1 <- matrix(1:4, ncol = 2, nrow = 2)
  M2 <- matrix(1:9, ncol = 3, nrow = 3)
  expect_true(is.numeric(minor(M1, 1, 1)))
  expect_true(is.numeric(minor(M2, 2, 3)))
  expect_true(is.vector(minor(M1, 1, 1)))
  expect_true(is.vector(minor(M2, 2, 3)))
})


test_that("Test all_matrix_minors(not_matrix)",{
  expect_error(all_matrix_minors(c(NaN, NaN)), "Not appropriate input format")
  expect_error(all_matrix_minors(data.frame(NaN, NaN)), "Not appropriate input format")
  expect_error(all_matrix_minors(list(NaN, NaN)), "Not appropriate input format")
  expect_error(all_matrix_minors(factor(NaN, NaN)), "Not appropriate input format")
  expect_error(all_matrix_minors("String"), "Not appropriate input format")
})


test_that("Test all_matrix_minors(not_correct_matrix)",{
  M1 <- matrix(NA, ncol = 1, nrow = 1)
  expect_error(all_matrix_minors(M1), "Not correct dimension of input matrix")
})


test_that("Test all_matrix_minors(different_matrices)",{
  M1 <- matrix(1:4, ncol = 2, nrow = 2)
  M2 <- matrix(1:9, ncol = 3, nrow = 3)
  expect_equal(all_matrix_minors(M1), cbind(c(4, 3), c(2, 1)))
  expect_equal(all_matrix_minors(M2), cbind(c(-3, -6, -3), c(-6, -12, -6), c(-3, -6, -3)))
})


test_that("Test output type of all_matrix_minors",{
  M1 <- matrix(1:4, ncol = 2, nrow = 2)
  M2 <- matrix(1:9, ncol = 3, nrow = 3)
  expect_true(is.matrix(all_matrix_minors(M1)))
  expect_true(is.matrix(all_matrix_minors(M2)))
})


test_that("Test minus_one(not_correct_value)",{
  expect_error(minus_one(1), "Not correct number of components")
})


test_that("Test minus_one(different_values)",{
  expect_equal(minus_one(2), cbind(c(1, -1), c(-1, 1)))
  expect_equal(minus_one(3), cbind(c(1, -1, 1), c(-1, 1, -1), c(1, -1, 1)))
})


test_that("Test output type of minus_one",{
  expect_true(is.matrix(minus_one(2)))
  expect_true(is.matrix(minus_one(3)))
})


test_that("Test acoeff(not_correct_value)",{
  w <- cbind(c(1, 1), c(0, 0))
  expect_error(acoeff(w), "Devision by zero")
})


test_that("Test acoeff(different_values)",{
  w1 <- wcoeff_two_components(2)
  w2 <- wcoeff_two_components(4)
  w3 <- rbind(c(0.8, 0.1, 0.1), 
              c(0.05, 0.90, 0.05), 
              c(0.2, 0.1, 0.7))
  expect_equal(acoeff(w1), cbind(c(0, 2), c(4, -2)))
  expect_equal(acoeff(w2), cbind(c(-0.8, 0.4, 1.6, 2.8), c(4, 2, 0, -2)))
  expect_equal(acoeff(w3), cbind(c(3.90625, -0.37500, -0.53125), 
                                 c(-0.15625, 3.37500, -0.21875), 
                                 c(-1.09375, -0.37500, 4.46875)))
})


test_that("Test output type of acoeff",{
  w1 <- wcoeff_two_components(2)
  w2 <- wcoeff_two_components(4)
  w3 <- rbind(c(0.8, 0.1, 0.1), 
              c(0.05, 0.90, 0.05), 
              c(0.2, 0.1, 0.7))
  expect_true(is.matrix(acoeff(w1)))
  expect_true(is.matrix(acoeff(w2)))
  expect_true(is.matrix(acoeff(w3)))
})
