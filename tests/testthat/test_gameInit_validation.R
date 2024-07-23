
library(testthat)
source("../../src/fun/func_gameInit_validation.R", local = TRUE, encoding = "UTF-8")

is_error <- function(x) {
  expect_type(x, "character")
}

test_that("valid_rng_seed", {

  # OK cases
  expect_null(valid_rng_seed(42))
  expect_null(valid_rng_seed(NULL))

  # invalid cases (no error)
  is_error(valid_rng_seed(42.234))
  is_error(valid_rng_seed(-42))

  is_error(valid_rng_seed(NA))
  is_error(valid_rng_seed("abc"))
  is_error(valid_rng_seed(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_rng_seed(24.234, raise_error = TRUE))

})


test_that("valid_positive_number", {

  # OK cases
  expect_null(valid_positive_number(42))
  expect_null(valid_positive_number(42.42))
  expect_null(valid_positive_number(0))
  expect_null(valid_positive_number(NULL))

  # invalid cases (no error)
  is_error(valid_positive_number(-42))
  is_error(valid_positive_number(0, strict = TRUE))

  is_error(valid_positive_number(NA))
  is_error(valid_positive_number("24.234"))
  is_error(valid_positive_number("abc"))
  is_error(valid_positive_number(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_positive_number("abc", raise_error = TRUE))

})

test_that("valid_number", {

  # OK cases
  expect_null(valid_number(42))
  expect_null(valid_number(42.42))
  expect_null(valid_number(-42))
  expect_null(valid_number(-42.42))
  expect_null(valid_number(0))
  expect_null(valid_number(NULL))

  # invalid cases (no error)
  is_error(valid_number(NA))
  is_error(valid_number("24.234"))
  is_error(valid_number("-24.234"))
  is_error(valid_number("abc"))
  is_error(valid_number(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_number("abc", FALSE, raise_error = TRUE))

})


test_that("valid_range", {

  # OK cases
  expect_null(valid_range(42, 41, 43))
  expect_null(valid_range(-42, -43, -41))
  expect_null(valid_range(0, 0, 1))
  expect_null(valid_range(1, 0, 1))
  expect_null(valid_range(NULL))

  # invalid cases (no error)
  is_error(valid_range(42, 43, 44))
  is_error(valid_range(-42, -44, -43))
  is_error(valid_range(0, 0, 1, incl_min = FALSE))
  is_error(valid_range(1, 0, 1, incl_max = FALSE))

  is_error(valid_range(NA))
  is_error(valid_range("abc"))
  is_error(valid_range(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_range("abc", FALSE, raise_error = TRUE))

})



test_that("valid_mu", {

  # OK cases
  expect_null(valid_mu(42))
  expect_null(valid_mu(42.42))
  expect_null(valid_mu(-42))
  expect_null(valid_mu(-42.42))
  expect_null(valid_mu(NULL))

  # invalid cases (no error)
  is_error(valid_mu(0))

  is_error(valid_mu(NA))
  is_error(valid_mu("24.234"))
  is_error(valid_mu("-24.234"))
  is_error(valid_mu("abc"))
  is_error(valid_mu(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_mu("abc", FALSE, raise_error = TRUE))

})


test_that("valid_Tmin", {

  # OK cases
  expect_null(valid_Tmin(42, 100))
  expect_null(valid_Tmin(42.42, 100))
  expect_null(valid_Tmin(-42, 100))
  expect_null(valid_Tmin(-42.42, 100))
  expect_null(valid_Tmin(NULL))

  # invalid cases (no error)
  is_error(valid_Tmin(42.42, 0))
  is_error(valid_Tmin(-42, -100))

  is_error(valid_Tmin(NA, 1))
  is_error(valid_Tmin("24.234", 1))
  is_error(valid_Tmin("-24.234", 1))
  is_error(valid_Tmin("abc", 1))

  is_error(valid_Tmin(NULL, 1, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_Tmin(42.42, 0, FALSE, raise_error = TRUE))

})


test_that("valid_cv_g", {

  # OK cases
  expect_null(valid_cv_g(12))
  expect_null(valid_cv_g(0.13))
  expect_null(valid_cv_g(NULL))

  # invalid cases (no error)
  is_error(valid_cv_g(-24.234))
  is_error(valid_cv_g(0))

  is_error(valid_cv_g(NA))
  is_error(valid_cv_g("24.234"))
  is_error(valid_cv_g("-24.234"))
  is_error(valid_cv_g("abc"))
  is_error(valid_cv_g(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_cv_g(-1, FALSE, raise_error = TRUE))

})

test_that("valid_h2", {

  # OK cases
  expect_null(valid_h2(0.3))
  expect_null(valid_h2(NULL))

  # invalid cases (no error)
  is_error(valid_h2(0))
  is_error(valid_h2(1))
  is_error(valid_h2(1.1))
  is_error(valid_h2(-0.1))

  is_error(valid_h2(NA))
  is_error(valid_h2("24.234"))
  is_error(valid_h2("-24.234"))
  is_error(valid_h2("abc"))
  is_error(valid_h2(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_h2(42, FALSE, raise_error = TRUE))

})


test_that("valid_variance", {

  # OK cases
  expect_null(valid_variance(0.3))
  expect_null(valid_variance(100))
  expect_null(valid_variance(NULL))
  expect_null(valid_variance(NA, accept_na = TRUE))

  # invalid cases (no error)
  is_error(valid_variance(-0.1))
  is_error(valid_variance(0))

  is_error(valid_variance(NA))
  is_error(valid_variance("24.234"))
  is_error(valid_variance("-24.234"))
  is_error(valid_variance("abc"))
  is_error(valid_variance(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_variance(-42, FALSE, raise_error = TRUE))

})


test_that("valid_prop_pleio", {

  # OK cases
  expect_null(valid_prop_pleio(0.7))
  expect_null(valid_prop_pleio(NULL))

  # invalid cases (no error)
  is_error(valid_prop_pleio(-0.1))
  is_error(valid_prop_pleio(1.2))

  is_error(valid_prop_pleio(NA))
  is_error(valid_prop_pleio("24.234"))
  is_error(valid_prop_pleio("-24.234"))
  is_error(valid_prop_pleio("abc"))
  is_error(valid_prop_pleio(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_prop_pleio(-42, FALSE, raise_error = TRUE))

})


test_that("valid_cor_pleio", {

  # OK cases
  expect_null(valid_cor_pleio(0.7))
  expect_null(valid_cor_pleio(-0.3))
  expect_null(valid_cor_pleio(NULL))

  # invalid cases (no error)
  is_error(valid_cor_pleio(-1.2))
  is_error(valid_cor_pleio(1.1))

  is_error(valid_cor_pleio(NA))
  is_error(valid_cor_pleio("24.234"))
  is_error(valid_cor_pleio("-24.234"))
  is_error(valid_cor_pleio("abc"))
  is_error(valid_cor_pleio(NULL, accept_null = FALSE))

  # invalid cases (error)
  expect_error(valid_cor_pleio(-42, FALSE, raise_error = TRUE))

})
