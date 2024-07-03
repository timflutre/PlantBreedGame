
library(testthat)
source("../../src/fun/func_gameInit_validation.R", local = TRUE, encoding = "UTF-8")


test_that("valid_rng_seed", {

  # OK cases
  expect_null(valid_rng_seed(42))
  expect_null(valid_rng_seed(NULL))

  # invalid cases (no error)
  expect_type(valid_rng_seed(NA), "character")
  expect_type(valid_rng_seed(24.234), "character")
  expect_type(valid_rng_seed(-42), "character")
  expect_type(valid_rng_seed("abc"), "character")
  expect_type(valid_rng_seed(NULL, FALSE), "character")

  # invalid cases (error)
  expect_error(valid_rng_seed(24.234, FALSE, TRUE))

})
