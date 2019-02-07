library(testthat)
source("tools/TestsTools.R")


test_that("Test Gramm_matrix(not_matrix)",{
  expect_error(Gramm_matrix(c(NA, NA)), "Not appropriate input format")
  expect_error(Gramm_matrix(data.frame(NA, NA)), "Not appropriate input format")
  expect_error(Gramm_matrix(list(NA, NA)), "Not appropriate input format")
  expect_error(Gramm_matrix(factor(NA, NA)), "Not appropriate input format")
  expect_error(Gramm_matrix("String"), "Not appropriate input format")
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
  expect_error(minor(c(NA, NA), 1, 1), "Not appropriate input format")
  expect_error(minor(data.frame(NA, NA), 1, 1), "Not appropriate input format")
  expect_error(minor(list(NA, NA), 1, 1), "Not appropriate input format")
  expect_error(minor(factor(NA, NA), 1, 1), "Not appropriate input format")
  expect_error(minor("String", 1, 1), "Not appropriate input format")
})

test_that("Test minor(matrix, not_correct_i_j)",{
  M <- matrix(NA, nrow = 2, ncol = 2)
  expect_error(minor(M, 0, 0), "Not correct i,j")
  expect_error(minor(M, 1, 0), "Not correct i,j")
  expect_error(minor(M, 0, 1), "Not correct i,j")
  expect_error(minor(M, -1, 0), "Not correct i,j")
  expect_error(minor(M, 0, -1), "Not correct i,j")
  expect_error(minor(M, -1, -1), "Not correct i,j")
  expect_error(minor(M, -1, 1), "Not correct i,j")
  expect_error(minor(M, 1, -1), "Not correct i,j")
})

test_that("Test all_matrix_minor(not_matrix)",{
  expect_error(all_matrix_minors(c(NA, NA)), "Not appropriate input format")
  expect_error(all_matrix_minors(data.frame(NA, NA)), "Not appropriate input format")
  expect_error(all_matrix_minors(list(NA, NA)), "Not appropriate input format")
  expect_error(all_matrix_minors(factor(NA, NA)), "Not appropriate input format")
  expect_error(all_matrix_minors("String"), "Not appropriate input format")
})
