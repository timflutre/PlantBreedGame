library(testthat)

source("../../src/fun/func_blups.R", local = TRUE, encoding = "UTF-8")


# build_formula ----
test_that("basic formula with fixed and random effects", {
  result <- build_formula(
    trait = "yield",
    fixed_effects = c("treatment", "block"),
    random_effects = "subject"
  )

  expect_s3_class(result, "formula")
  expect_equal(result, as.formula("yield ~ 1 + treatment + block + (1|subject)"))
})

test_that("formula without intercept", {
  result <- build_formula(
    trait = "yield",
    fixed_effects = "treatment",
    random_effects = "subject",
    intercept = FALSE
  )

  expect_s3_class(result, "formula")
  expect_equal(result, as.formula("yield ~ 0 + treatment + (1|subject)"))
})

test_that("intercept defaults to TRUE", {
  result_default <- build_formula("y", "x", "g")
  result_explicit <- build_formula("y", "x", "g", intercept = TRUE)

  expect_equal(result_default, result_explicit)
  formula_char <- as.character(result_default)
  expect_true(grepl(pattern = "^1 +", x = formula_char[3]))
})

test_that("uses correct (ie. parent's) environment", {
  formula_1 <- build_formula("y", "x", "g")
  formula_2 <- build_formula("y", "x", "g")
  formula_build_here <- as.formula("y ~ 1 + x + (1|g)")

  expect_identical(formula_1, formula_2)
  expect_identical(formula_1, formula_build_here)
})


test_that("multiple random effects are all included", {
  rnd_effs <- c("subject", "site", "batch")
  result <- build_formula(
    trait = "yield",
    fixed_effects = "treatment",
    random_effects = rnd_effs
  )

  formula_char <- as.character(result)
  expect_true(grepl("(1 | subject)", formula_char[3], fixed = TRUE))
  expect_true(grepl("(1 | site)", formula_char[3], fixed = TRUE))
  expect_true(grepl("(1 | batch)", formula_char[3], fixed = TRUE))

  formula_rnd_eff <- na.omit(stringr::str_match(labels(terms(result)), "1 \\| (.+)")[, 2])
  expect_setequal(formula_rnd_eff, rnd_effs)
})

test_that("multiple fixed effects are all included", {
  fixed_effs <- c("treatment", "block", "year")
  result <- build_formula(
    trait = "yield",
    fixed_effects = fixed_effs,
    random_effects = "subject"
  )

  formula_char <- as.character(result)
  expect_true(grepl("treatment", formula_char[3]))
  expect_true(grepl("block", formula_char[3]))
  expect_true(grepl("year", formula_char[3]))
  expect_false(grepl("(1 | treatment)", formula_char[3], fixed = TRUE))
  expect_false(grepl("(1 | block)", formula_char[3], fixed = TRUE))
  expect_false(grepl("(1 | year)", formula_char[3], fixed = TRUE))

  formula_fix_eff <- grep("1 \\|", labels(terms(result)), value = TRUE, invert = TRUE)
  expect_setequal(formula_fix_eff, fixed_effs)
})

test_that("trait appears as the response variable", {
  result <- build_formula("my_trait", "x", "g")
  expect_equal(as.character(result[[2]]), "my_trait")
})

test_that("empty fixed effects produces a valid formula", {
  result <- build_formula(
    trait = "yield",
    fixed_effects = character(0),
    random_effects = "subject"
  )
  expect_s3_class(result, "formula")
  formula_fix_eff <- grep("1 \\|", labels(terms(result)), value = TRUE, invert = TRUE)
  expect_true(length(formula_fix_eff) == 0)

  formula_char <- as.character(result)
  expect_true(grepl("(1 | subject)", formula_char[3], fixed = TRUE))
})

test_that("empty random effects produces a valid formula", {
  result <- build_formula(
    trait = "yield",
    fixed_effects = "treatment",
    random_effects = character(0)
  )

  expect_s3_class(result, "formula")
  formula_rnd_eff <- na.omit(stringr::str_match(labels(terms(result)), "1 \\| (.+)")[, 2])
  expect_true(length(formula_rnd_eff) == 0)

  formula_char <- as.character(result)
  expect_true(grepl("treatment", formula_char[3]))
  expect_false(grepl("(1 | treatment)", formula_char[3], fixed = TRUE))
})


# parse_formula ----
expect_correct_parsed_formula_structure <- function(result) {
  expect_type(result, "list")
  expect_setequal(
    names(result),
    c("trait", "fixed_effects", "random_effects", "intercept")
  )
}

test_that("basic formula with fixed and random effects", {
  result <- parse_formula(as.formula("yield ~ 1 + treatment + block + (1|subject)"))
  expect_correct_parsed_formula_structure(result)
  expect_equal(result$trait, "yield")
  expect_equal(result$fixed_effects, c("treatment", "block"))
  expect_equal(result$random_effects, "subject")
  expect_equal(result$intercept, TRUE)
})

test_that("formula with multiple random effects", {
  result <- parse_formula(as.formula("yield ~ 1 + treatment + (1|subject) + (1|site) + (1|batch)"))
  expect_correct_parsed_formula_structure(result)
  expect_equal(result$trait, "yield")
  expect_equal(result$fixed_effects, "treatment")
  expect_equal(result$random_effects, c("subject", "site", "batch"))
  expect_equal(result$intercept, TRUE)
})

test_that("formula with multiple fixed effects", {
  result <- parse_formula(as.formula("yield ~ 1 + treatment + block + year + (1|subject)"))
  expect_correct_parsed_formula_structure(result)
  expect_equal(result$trait, "yield")
  expect_equal(result$fixed_effects, c("treatment", "block", "year"))
  expect_equal(result$random_effects, "subject")
  expect_equal(result$intercept, TRUE)
})

test_that("formula with multiple fixed and random effects", {
  result <- parse_formula(as.formula("yield ~ 1 + treatment + block + (1|subject) + (1|site)"))
  expect_correct_parsed_formula_structure(result)
  expect_equal(result$trait, "yield")
  expect_equal(result$fixed_effects, c("treatment", "block"))
  expect_equal(result$random_effects, c("subject", "site"))
  expect_equal(result$intercept, TRUE)
})

test_that("formula without intercept", {
  result <- parse_formula(as.formula("yield ~ 0 + treatment + (1|subject)"))
  expect_correct_parsed_formula_structure(result)
  expect_equal(result$trait, "yield")
  expect_equal(result$fixed_effects, "treatment")
  expect_equal(result$random_effects, "subject")
  expect_equal(result$intercept, FALSE)
})

test_that("formula with intercept not explicitly set", {
  result <- parse_formula(as.formula("yield ~ treatment + (1|subject)"))
  expect_correct_parsed_formula_structure(result)
  expect_equal(result$trait, "yield")
  expect_equal(result$fixed_effects, "treatment")
  expect_equal(result$random_effects, "subject")
  expect_equal(result$intercept, TRUE)
})

test_that("formula without random effects", {
  result <- parse_formula(as.formula("yield ~ 1 + treatment + block"))
  expect_correct_parsed_formula_structure(result)
  expect_equal(result$trait, "yield")
  expect_equal(result$fixed_effects, c("treatment", "block"))
  expect_equal(result$random_effects, character(0))
  expect_equal(result$intercept, TRUE)
})

test_that("formula without fixed effects", {
  result <- parse_formula(as.formula("yield ~ 1 + (1|subject)"))
  expect_correct_parsed_formula_structure(result)
  expect_equal(result$trait, "yield")
  expect_equal(result$fixed_effects, character(0))
  expect_equal(result$random_effects, "subject")
  expect_equal(result$intercept, TRUE)
})

# extract_fixed_eff ----

expect_correct_effects_structure <- function(result) {
  expect_type(result, "list")
  expect_true("type" %in% names(result))
  expect_true(result$type %in% c("random", "fixed"))

  expect_true("estimations_df" %in% names(result))
  expect_s3_class(result$estimations_df, "data.frame")
  expect_equal(colnames(result$estimations_df)[2], "estimations")
  expect_true(!any(duplicated(result$estimations_df[1])))
  if (result$type == "random") {
    expect_true("sd" %in% names(result))
  }
}

test_that("factor fixed effect", {
  mock_pheno <- mock_pheno_data(n_years = 3, n_inds = 10, n_rep_per_year = 2)
  data <- mock_pheno$data

  model <- lme4::lmer("trait1 ~ year + (1|ind)",
    data = data,
    REML = TRUE
  )

  result <- extract_fixed_eff("year", model)
  expect_correct_effects_structure(result)
  expect_equal(result$type, "fixed")
  expect_equal(colnames(result$estimations_df)[1], "year")
  expect_equal(nrow(result$estimations_df), length(unique(data$year)))
})

test_that("numeric fixed effect", {
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  result <- extract_fixed_eff("Days", model)

  expect_correct_effects_structure(result)
  expect_equal(result$type, "fixed")
  expect_equal(colnames(result$estimations_df)[1], "Days")
  expect_equal(nrow(result$estimations_df), 1)
  expect_equal(result$estimations_df[1, 1], "")
})


test_that("edge case with similar variables names", {
  n <- 30
  data <- data.frame(
    var = rep(c("21", "22", "23"), each = n / 3),
    var2 = rep(c("1", "2"), times = n / 2),
    var22 = rnorm(n),
    rnd_var = rep(c("1", "2"), each = n / 2)
  )
  beta_var_22 <- 30
  beta_var_23 <- -20
  beta_var2_2 <- 50
  data$Y <- (data$var22 + beta_var_22 * (data$var == "22") +
    beta_var_23 * (data$var == "23") +
    beta_var2_2 * (data$var2 == "2")) + rnorm(n, sd = 0.0001)

  (model <- lme4::lmer(Y ~ var + var2 + var22 + (1 | rnd_var), data = data))
  # here `names(lme4::fixef(model))` is
  # [1] "(Intercept)" "var22" "var23" "var22" "var22" <- duplicated names !

  fe_var <- extract_fixed_eff("var", model)
  expect_correct_effects_structure(fe_var)
  expect_equal(fe_var$type, "fixed")
  expect_equal(colnames(fe_var$estimations_df)[1], "var")
  expect_setequal(fe_var$estimations_df$var, data$var)
  expect_true(isTRUE(all.equal(fe_var$estimations_df$estimations[1], 0, tolerance = 1e-3)))
  expect_true(isTRUE(all.equal(fe_var$estimations_df$estimations[2], beta_var_22, tolerance = 1e-3)))
  expect_true(isTRUE(all.equal(fe_var$estimations_df$estimations[3], beta_var_23, tolerance = 1e-3)))

  fe_var2 <- extract_fixed_eff("var2", model)
  expect_correct_effects_structure(fe_var2)
  expect_equal(colnames(fe_var2$estimations_df)[1], "var2")
  expect_setequal(fe_var2$estimations_df$var2, data$var2)
  expect_true(isTRUE(all.equal(fe_var2$estimations_df$estimations[1], 0, tolerance = 1e-3)))
  expect_true(isTRUE(all.equal(fe_var2$estimations_df$estimations[2], beta_var2_2, tolerance = 1e-3)))

  fe_var22 <- extract_fixed_eff("var22", model)
  expect_correct_effects_structure(fe_var22)
  expect_equal(colnames(fe_var22$estimations_df)[1], "var22")
  expect_true(isTRUE(all.equal(fe_var22$estimations_df$estimations[1], 1, tolerance = 1e-3)))
})

# extract_rand_eff ----
test_that("extract_rand_eff", {
  mock_pheno <- mock_pheno_data(n_years = 3, n_inds = 10, n_rep_per_year = 2)
  data <- mock_pheno$data

  model <- lme4::lmer("trait1 ~ year + (1|ind) + (1|plot)",
    data = data,
    REML = TRUE
  )

  result <- extract_rand_eff("ind", model)
  expect_correct_effects_structure(result)
  expect_equal(result$type, "random")
  expect_equal(colnames(result$estimations_df)[1], "ind")
  expect_setequal(result$estimations_df$ind, data$ind)

  result_plot <- extract_rand_eff("plot", model)
  expect_correct_effects_structure(result_plot)
  expect_equal(result_plot$type, "random")
  expect_equal(colnames(result_plot$estimations_df)[1], "plot")
  expect_setequal(result_plot$estimations_df$plot, data$plot)
})



# extract_lmer_effects ----
test_that("extract_lmer_effects", {
  mock_pheno <- mock_pheno_data(n_years = 3, n_inds = 10, n_rep_per_year = 2)
  data <- mock_pheno$data

  model <- lme4::lmer("trait1 ~ 1 + year + (1|ind) + (1|plot)",
    data = data,
    REML = TRUE
  )

  result <- extract_lmer_effects(model)
  expect_type(result, "list")
  expect_setequal(names(result), c("model", "effects"))
  expect_identical(result$model, model)

  expect_type(result$effects, "list")
  expect_setequal(names(result$effects), c(
    "year", "ind", "plot",
    "residuals", "intercept"
  ))
})


# calculate_blups ----
test_that("calculate_blups", {
  mock_pheno <- mock_pheno_data(n_years = 3, n_inds = 10, n_rep_per_year = 2)
  data <- mock_pheno$data

  result <- calculate_blups(pheno_data = data,
                            trait = "trait1",
                            fixed_effects = c("year", "trait3"),
                            random_effects = "ind",
                            intercept = TRUE)
  expect_type(result, "list")
  expect_setequal(names(result), c("model", "effects"))
  expect_s4_class(result$model, "lmerMod")

  expect_type(result$effects, "list")
  expect_setequal(names(result$effects), c(
    "year", "ind", "trait3",
    "residuals", "intercept"
  ))
})
