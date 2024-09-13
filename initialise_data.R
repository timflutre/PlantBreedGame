#! /usr/bin/env Rscript

params = list(
  rng_seed = 1993,

  cost.pheno.field = 50,
  cost.pheno.patho = 0.1,
  cost.allof = 0.1,
  cost.autof = 0.25,
  cost.haplodiplo = 1,
  cost.geno.hd = 1,
  cost.geno.ld = 0.5,
  cost.geno.single = 0.02,
  cost.register = 4,
  initialBudget = 3900,

  t1_mu = 100,
  t1_min = 20,
  t1_cv_g = 0.1,
  t1_h2 = 0.3,

  t2_mu = 15,
  t2_min = 5,
  t2_cv_g = 0.06,
  t2_h2 = 0.6,

  prop_pleio = 0.4,
  cor_pleio = -0.7,

  max.nb.haplodiplos = 300,
  max.nb.pltmatReq = 300,
  nb.plots = 300,
  maxEvalInds = 5
)

rmd_env <- new.env(parent = globalenv())
out_report <- rmarkdown::render("./src/plantbreedgame_setup.Rmd",
  output_file = tempfile(),
  encoding = "UTF-8",
  params = params,
  knit_root_dir = getwd(),
  envir = rmd_env,
)

new_session_id <- rmd_env$session_id
file.copy(
  from = out_report,
  to = file.path("data", new_session_id, "reports", "plantBreedGame_initialisation_report.html")
)
