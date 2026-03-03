library(testthat)
library(dplyr)

source("../../src/fun/func_resistance_detection.R", local = TRUE, encoding = "UTF-8")


make_pheno <- function(ind, pathogen, trait3) {
  data.frame(ind = ind, pathogen = pathogen, trait3 = trait3,
             stringsAsFactors = FALSE)
}

# calc_resistant_summary ----

test_that("output is a data.frame with specific columns names", {
  pheno <- make_pheno("A", TRUE, FALSE)
  result <- calc_resistant_summary(pheno)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("ind", "n_patho_obs", "n_observed_resistance", "resistance_prop") %in% names(result)))
})

test_that("basic resistance calculation is correct", {
  pheno <- make_pheno(
    ind      = c("A", "A", "A"),
    pathogen = c(TRUE, TRUE, FALSE),
    trait3   = c(FALSE, TRUE, FALSE)
  )

  result <- calc_resistant_summary(pheno)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_patho_obs[result$ind == "A"], 2)
  expect_equal(result$n_observed_resistance[result$ind == "A"], 1)
  expect_equal(result$resistance_prop[result$ind == "A"], 0.5)
})

test_that("returns one row per individual", {
  pheno <- make_pheno(
    ind      = c("A", "A", "B", "B", "B"),
    pathogen = c(TRUE, TRUE, TRUE, FALSE, TRUE),
    trait3   = c(FALSE, FALSE, TRUE, FALSE, FALSE)
  )

  result <- calc_resistant_summary(pheno)

  expect_equal(nrow(result), 2)
  expect_setequal(result$ind, c("A", "B"))
})

test_that("individual with no pathogen observations has n_patho_obs = 0 and NaN resistance_prop", {
  pheno <- make_pheno(
    ind      = c("A", "A"),
    pathogen = c(FALSE, FALSE),
    trait3   = c(FALSE, FALSE)
  )

  result <- calc_resistant_summary(pheno)

  expect_equal(result$n_patho_obs[result$ind == "A"], 0)
  expect_equal(result$n_observed_resistance[result$ind == "A"], 0)
  expect_true(is.nan(result$resistance_prop[result$ind == "A"]))
})

test_that("full resistance (all pathogen obs are resistant)", {
  pheno <- make_pheno(
    ind      = c("A", "A", "A"),
    pathogen = c(TRUE, TRUE, TRUE),
    trait3   = c(FALSE, FALSE, FALSE)
  )

  result <- calc_resistant_summary(pheno)

  expect_equal(result$n_patho_obs[result$ind == "A"], 3)
  expect_equal(result$n_observed_resistance[result$ind == "A"], 3)
  expect_equal(result$resistance_prop[result$ind == "A"], 1.0)
})

test_that("zero resistance (all pathogen obs are susceptible)", {
  pheno <- make_pheno(
    ind      = c("A", "A"),
    pathogen = c(TRUE, TRUE),
    trait3   = c(TRUE, TRUE)
  )

  result <- calc_resistant_summary(pheno)

  expect_equal(result$n_patho_obs[result$ind == "A"], 2)
  expect_equal(result$n_observed_resistance[result$ind == "A"], 0)
  expect_equal(result$resistance_prop[result$ind == "A"], 0.0)
})

test_that("non-pathogen observations do not contribute to resistance counts", {
  pheno <- make_pheno(
    ind      = c("A", "A", "A"),
    pathogen = c(TRUE, FALSE, FALSE),
    trait3   = c(FALSE, FALSE, FALSE)   # last two would look resistant but pathogen=FALSE
  )

  result <- calc_resistant_summary(pheno)

  expect_equal(result$n_patho_obs[result$ind == "A"], 1)
  expect_equal(result$n_observed_resistance[result$ind == "A"], 1)
  expect_equal(result$resistance_prop[result$ind == "A"], 1.0)
})



test_that("multiple individuals with mixed observations", {
  pheno <- make_pheno(
    ind      = c("A", "A", "B", "B", "C"),
    pathogen = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    trait3   = c(FALSE, TRUE, FALSE, FALSE, TRUE)
  )

  result <- calc_resistant_summary(pheno)
  result <- result[order(result$ind), ]

  expect_equal(result$n_patho_obs,            c(2, 2, 0))
  expect_equal(result$n_observed_resistance,  c(1, 2, 0))
  expect_equal(result$resistance_prop,        c(0.5, 1.0, NaN))
})





# detect_resistant_naive ----

make_summary <- function(ind, n_patho_obs, resistance_prop) {
  data.frame(
    ind                   = ind,
    n_patho_obs           = n_patho_obs,
    n_observed_resistance = round(resistance_prop * n_patho_obs),
    resistance_prop       = resistance_prop,
    stringsAsFactors      = FALSE
  )
}

# Fixed thresholds used throughout tests
T_R  <- 0.6   # thresh_resistance
T_NR <- 0.4   # thresh_non_resistance


test_that("output is a data frame with 'resistant' column added", {
  df     <- make_summary("A", 3, 1.0)
  result <- detect_resistant_naive(df, T_R, T_NR)

  expect_s3_class(result, "data.frame")
  expect_true("resistant" %in% names(result))
})

test_that("all input columns are preserved", {
  df     <- make_summary("A", 10, 1.0)
  result <- detect_resistant_naive(df, T_R, T_NR)

  expect_true(all(names(df) %in% names(result)))
})

test_that("number of rows is unchanged", {
  df     <- make_summary(c("A", "B", "C"), c(10, 10, 10), c(1.0, 0.0, 0.5))
  result <- detect_resistant_naive(df, T_R, T_NR)

  expect_equal(nrow(result), nrow(df))
})


test_that("resistance_prop above thresh_resistance returns TRUE", {
  df     <- make_summary("A", 10, 0.9)
  result <- detect_resistant_naive(df, 0.6, T_NR)

  expect_true(result$resistant[result$ind == "A"])
})

test_that("resistance_prop below thresh_non_resistance -> FALSE", {
  df     <- make_summary("A", 10, 0.1)
  result <- detect_resistant_naive(df, T_R, 0.3)

  expect_false(result$resistant[result$ind == "A"])
})

test_that("resistance_prop in intermediate zone -> NA", {
  df     <- make_summary("A", 10, 0.5)
  result <- detect_resistant_naive(df, 0.6, 0.4)

  expect_true(is.na(result$resistant[result$ind == "A"]))
})

test_that("resistance_prop exactly at thresh_resistance -> NA (strict > not >=)", {
  df     <- make_summary("A", 3, 0.6)
  result <- detect_resistant_naive(df, 0.6, T_NR)

  expect_true(is.na(result$resistant[result$ind == "A"]))
})

test_that("resistance_prop exactly at thresh_non_resistance -> NA (strict < not <=)", {
  df     <- make_summary("A", 3, 0.3)
  result <- detect_resistant_naive(df, T_R, 0.3)

  expect_true(is.na(result$resistant[result$ind == "A"]))
})

test_that("resistance_prop is NaN -> NA regardless of the thresholds", {
  df     <- make_summary("A", 10, NaN)
  result <- detect_resistant_naive(df, 0.5, 0.5)
  expect_true(is.na(result$resistant[result$ind == "A"]))

  df     <- make_summary("A", 10, NaN)
  result <- detect_resistant_naive(df, 0, 0)
  expect_true(is.na(result$resistant[result$ind == "A"]))

  df     <- make_summary("A", 10, NaN)
  result <- detect_resistant_naive(df, 1, 1)
  expect_true(is.na(result$resistant[result$ind == "A"]))
})


test_that("n_patho_obs < min_patho_obs -> NA regardless of proportion", {
  df     <- make_summary("A", 3, 0.66666)
  result <- detect_resistant_naive(df, 0.6, 0.4, min_patho_obs = 5)
  expect_true(is.na(result$resistant[result$ind == "A"]))

  df     <- make_summary("A", 3, 0.66666)
  result <- detect_resistant_naive(df, 0.7, 0.5, min_patho_obs = 5)
  expect_true(is.na(result$resistant[result$ind == "A"]))

  df     <- make_summary("A", 3, 0.66666)
  result <- detect_resistant_naive(df, 0.8, 0.7, min_patho_obs = 5)
  expect_true(is.na(result$resistant[result$ind == "A"]))
})

test_that("n_patho_obs exactly equal to min_patho_obs is not filtered out", {
  df     <- make_summary("A", 3, 0.66666)
  result <- detect_resistant_naive(df, 0.6, 0.4, min_patho_obs = 3)
  expect_true(result$resistant[result$ind == "A"])

  df     <- make_summary("A", 3, 0.66666)
  result <- detect_resistant_naive(df, 0.7, 0.5, min_patho_obs = 3)
  expect_true(is.na(result$resistant[result$ind == "A"]))

  df     <- make_summary("A", 3, 0.66666)
  result <- detect_resistant_naive(df, 0.8, 0.7, min_patho_obs = 3)
  expect_false(result$resistant[result$ind == "A"])
})

test_that("n_patho_obs < custom min_patho_obs -> NA", {
  df     <- make_summary("A", 2, 1.0)
  result <- detect_resistant_naive(df, 0.5, 0.5, min_patho_obs = 5)
  expect_true(is.na(result$resistant[result$ind == "A"]))

  df     <- make_summary("A", 2, 0)
  result <- detect_resistant_naive(df, 0.5, 0.5, min_patho_obs = 5)
  expect_true(is.na(result$resistant[result$ind == "A"]))
})

test_that("all four outcome classes are assigned correctly in one call", {
  df <- make_summary(
    ind             = c("resistant", "non_resistant", "unknown", "insufficient"),
    n_patho_obs     = c(4, 4, 4, 2),
    resistance_prop = c(0.75, 0.25, 0.5, 1)
  )

  result <- detect_resistant_naive(df, 0.5, 0.4, min_patho_obs = 3)
  expect_true(result$resistant[result$ind == "resistant"])
  expect_false(result$resistant[result$ind == "non_resistant"])
  expect_true(is.na(result$resistant[result$ind == "unknown"]))
  expect_true(is.na(result$resistant[result$ind == "insufficient"]))
})
