mock_pheno_data <- function(n_years = 3,
                            n_inds = 10,
                            n_rep_per_year = 2,
                            mu = c(100, 15),
                            sigma_a2 = c(100, 0.81),
                            sigma_y2 = c(378, 9.8),
                            sigma2 = c(233, 0.54),
                            prop_disease_loss = c(0.2, 0)) {
  years <- as.character(seq(2000, length.out = n_years))
  year_effects <- matrix(
    c(
      rnorm(n_years, 0, sqrt(sigma_y2[1])),
      rnorm(n_years, 0, sqrt(sigma_y2[2]))
    ),
    nrow = n_years, ncol = 2, dimnames = list(years, c("T1", "T2"))
  )
  pathogen <- c(TRUE, sample(c(TRUE, FALSE), n_years - 1, replace = TRUE))

  inds_ids <- sprintf(
    paste0(
      "ind_",
      "%0", floor(log10(n_inds)) + 1, "i"
    ),
    seq(n_inds)
  )
  gvs <- matrix(
    c(
      rnorm(n_inds, 0, sqrt(sigma_a2[1])),
      rnorm(n_inds, 0, sqrt(sigma_a2[2])),
      sample(c(TRUE, FALSE), n_inds, replace = TRUE, prob = c(1 / 5, 1 - 1 / 5))
    ),
    nrow = n_inds, ncol = 3, dimnames = list(inds_ids, c("T1", "T2", "resist"))
  )
  gvs <- as.data.frame(gvs)
  gvs$ind <- row.names(gvs)
  prop_disease_loss_per_inds <- matrix(
    c(
      (!gvs[, "resist"]) * prop_disease_loss[1],
      (!gvs[, "resist"]) * prop_disease_loss[2]
    ),
    nrow = n_inds, ncol = 2, dimnames = list(inds_ids, c("T1", "T2"))
  )

  n_obs <- n_inds * n_years * n_rep_per_year
  noise <- matrix(
    c(
      rnorm(n_obs, 0, sqrt(sigma2[1])),
      rnorm(n_obs, 0, sqrt(sigma2[2]))
    ),
    nrow = n_obs, ncol = 2
  )

  pheno_dta <- data.frame(
    ind = rep(rep(inds_ids, each = n_rep_per_year), n_years),
    year = rep(years, each = n_inds * n_rep_per_year),
    plot = as.vector(replicate(n_years, {
      rep(sample(n_inds * n_rep_per_year))
    })),
    pathogen = rep(pathogen, each = n_inds * n_rep_per_year)
  )
  trait1_raw <- mu[1] + gvs[pheno_dta$ind, 1] + year_effects[pheno_dta$year, 1] + noise[, 1]
  trait2_raw <- mu[2] + gvs[pheno_dta$ind, 2] + year_effects[pheno_dta$year, 2] + noise[, 2]
  pheno_dta$trait3 <- pheno_dta$pathogen & !gvs[pheno_dta$ind, "resist"]
  pheno_dta$trait1 <- ifelse(
    pheno_dta$trait3,
    trait1_raw * (1 - prop_disease_loss_per_inds[pheno_dta$ind, 1]),
    trait1_raw
  )
  pheno_dta$trait2 <- ifelse(
    pheno_dta$trait3,
    trait2_raw * (1 - prop_disease_loss_per_inds[pheno_dta$ind, 2]),
    trait2_raw
  )
  gvs$T1_bis <- (gvs$T1 + mu[1]) * (1 - prop_disease_loss_per_inds[row.names(gvs), 1]) - mu[1]
  gvs$T2_bis <- (gvs$T2 + mu[2]) * (1 - prop_disease_loss_per_inds[row.names(gvs), 2]) - mu[2]


  list(
    data = pheno_dta,
    gvs = gvs,
    year_effects = year_effects
  )
}
