library(R6)


test_that("Test train_test_split(not_correct_input)", {
  expect_error(train_test_split(rep(NaN, 5), 1), "Not correct input type")
  expect_error(train_test_split(list(NaN), 1), "Not correct input type")
  expect_error(train_test_split(as.factor(NaN), 1), "Not correct input type")
  expect_error(train_test_split(matrix(NA, 1, 2), 1), "Not correct input dimension")
})


test_that("Test output type of train_test_split(correct_input)", {
  expect_type(train_test_split(matrix(1:5, 5, 1), .5, 1), "list")
  expect_type(train_test_split(matrix(1:10, 5, 2), .9, 1)[["test"]], "integer")
})


test_that("Test cross_validation_split(not_correct_input)", {
  expect_error(cross_validation_split(rep(NaN, 5), 5, 1), "Not correct input type")
  expect_error(cross_validation_split(list(NaN), 5, 1), "Not correct input type")
  expect_error(cross_validation_split(as.factor(NaN), 5, 1), "Not correct input type")
  expect_error(cross_validation_split(matrix(NaN, 1, 2), 5, 1), "Not correct input dimension")
})


test_that("Test output type of cross_validation_split(correct_input)", {
  expect_type(cross_validation_split(matrix(1:5, 5, 1), 5, 1), "list")
  expect_type(cross_validation_split(matrix(1:10, 5, 2), 2, 1)[["2"]], "integer")
})
