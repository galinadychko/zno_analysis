library("testthat")


test_that("Test split_k_parts(not_correct_input)", {
  expect_error(split_k_parts(2, 1), "Number of observations is smaller than number of folds")
})


test_that("Test split_k_parts(correct_input)", {
  expect_equal(split_k_parts(1, 2), list("1" = c(1, 2)))
  expect_equal(split_k_parts(2, 2), list("1" = 1, "2" = 2))
  expect_equal(split_k_parts(3, 4), list("1" = c(1, 2), "2" = c(3, 4), "3" = as.numeric()))
  expect_equal(split_k_parts(3, 5), list("1" = c(1, 2), "2" = c(3, 4), "3" = 5))
})
