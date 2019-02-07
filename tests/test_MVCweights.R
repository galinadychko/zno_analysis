library(testthat)
source("tools/TestsTools.R")


test_that("Test Gramm_matrix(any_vector)",{
  expect_error(Gramm_matrix(c(0, 1)), "Not appropriate input format")
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
