calculate_blups <- function(pheno_data,
                            trait,
                            fixed_effects,
                            random_effects,
                            intercept = TRUE,
                            contrast_type = "sum") {
  model <- fit_lmer(
    pheno_data = pheno_data,
    trait = trait,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    intercept = intercept,
    contrast_type = contrast_type
  )
  extract_lmer_effects(model)
}


CONTRAST_FUNCTIONS <- list(
  sum = contr.sum,
  treatment = contr.treatment
)

fit_lmer <- function(pheno_data,
                     trait,
                     fixed_effects,
                     random_effects,
                     intercept = TRUE,
                     contrast_type = "sum") {
  ## TODO: missing values management

  pheno_data$ind <- as.factor(pheno_data$ind)
  pheno_data$year <- as.factor(pheno_data$year)
  pheno_data$plot <- as.factor(pheno_data$plot)
  pheno_data$pathogen <- as.factor(pheno_data$pathogen)
  pheno_data$trait3 <- as.factor(pheno_data$trait3)

  formula <- build_formula(trait, fixed_effects, random_effects, intercept = TRUE)

  for (v in fixed_effects) {
    if (is.factor(pheno_data[, v])) {
      contrasts(pheno_data[, v]) <- CONTRAST_FUNCTIONS[[contrast_type]](levels(pheno_data[, v]))
    }
  }

  model <- lme4::lmer(formula, data = pheno_data, REML = TRUE)

  return(model)
}


extract_lmer_effects <- function(model) {
  parsed_formula <- parse_formula(as.formula(model))
  fixed_effects <- parsed_formula$fixed_effects
  random_effects <- parsed_formula$random_effects
  intercept <- parsed_formula$intercept

  rand_eff_var <- as.data.frame(lme4::VarCorr(model))
  rand_eff <- lme4::ranef(model, condVar = TRUE)
  mu <- if (intercept) {
    lme4::fixef(model)["(Intercept)"]
  } else {
    NA
  }

  effect_intercept <- list(
    intercept = list(
      type = "fixed",
      estimations_df = data.frame(
        intercept = "intercept",
        estimations = mu
      )
    )
  )

  effects_fixed <- lapply(fixed_effects, extract_fixed_eff, model = model)
  names(effects_fixed) <- fixed_effects

  effects_rand <- lapply(random_effects, extract_rand_eff, model = model)
  names(effects_rand) <- random_effects

  residuals <- list(
    residuals = list(
      type = "residuals",
      sd = sigma(model)
    )
  )

  return(list(
    model = model,
    effects = c(
      effect_intercept,
      effects_fixed,
      effects_rand,
      residuals
    )
  ))
}

extract_fixed_eff <- function(v, model) {
  # To extract the fixed effect of the model (ie. `fixef(model)`) that correspond
  # to the variable of interest `v` using a naive pattern recognition on
  # `names(fixef(model))` could return wrong values:
  # eg. "var" (factors A and B) and "var2" (factors C and D) as fixed effects:
  #   - `names(fixef(model))` could be `"varA" "varB" "var2C" "var2D"`
  #   - `grepl(pattern = "^var, x = names(fixef(model)))`) would return all instead
  #   of only the fixed effects for "varA" "varB"
  #
  # Rather, `attr(model.matrix(model), "assign")` returns the id of the
  # variable corresponding to each `fixef(model)`
  variables_used_as_fixed_effects <- attr(terms(model), "term.labels")
  v_id <- which(variables_used_as_fixed_effects == v)
  fixed_effects_variable_id <- attr(model.matrix(model), "assign")
  coef_idx <- which(fixed_effects_variable_id == v_id)
  coefs <- lme4::fixef(model)[coef_idx]

  if (is.factor(model@frame[, v])) {
    cmat <- contrasts(model@frame[, v])
    estimations_mat <- cmat %*% coefs
  } else {
    estimations_mat <- matrix(coefs)
    row.names(estimations_mat) <- ""
  }

  df <- data.frame(
    V = row.names(estimations_mat),
    estimations = estimations_mat[, 1],
    row.names = row.names(estimations_mat)
  )
  colnames(df)[1] <- v
  return(list(
    type = "fixed",
    estimations_df = df
  ))
}

extract_rand_eff <- function(v, model) {
  rand_eff <- lme4::ranef(model, condVar = TRUE, whichel = v)
  df <- as.data.frame(rand_eff)
  df <- df[, c("grp", "condval", "condsd")]
  colnames(df) <- c(v, "estimations", "condsd")
  row.names(df) <- df[[v]]

  list(
    type = "random",
    sd = attr(lme4::VarCorr(model)[[v]], "stddev"),
    estimations_df = df
  )
}


build_formula <- function(trait,
                          fixed_effects,
                          random_effects,
                          intercept = TRUE) {
  caller_env <- parent.frame()

  intercept_str <- if (intercept) "1" else "0"
  fixed_part <- paste(fixed_effects, collapse = " + ")
  random_part <- paste(sprintf("(1|%s)", random_effects), collapse = " + ")
  formula_str <- paste(trait, "~", paste(
    Filter(nzchar, c(
      intercept_str,
      fixed_part,
      random_part
    )),
    collapse = " + "
  ))
  formula <- as.formula(formula_str, env = caller_env)
  return(formula)
}

parse_formula <- function(formula) {
  formula_terms <- terms(formula)

  intercept <- as.logical(attr(formula_terms, "intercept"))
  components <- attr(formula_terms, "term.labels")
  fixed_effects <- components[!grepl(pattern = "^1 \\|.*$", components)]
  random_effects <- do.call(c, regmatches(
    components,
    gregexpr("(?<=^1 \\| ).*",
             components,
             perl = TRUE
    )
  ))
  list(
    trait = as.character(formula)[2],
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    intercept = intercept
  )
}
