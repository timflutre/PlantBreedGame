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
  cor_pleio = -0.7
)


out_report <- rmarkdown::render("./src/plantbreedgame_setup.Rmd",
  output_file = tempfile(),
  encoding = "UTF-8",
  params = params,
  envir = new.env(parent = globalenv()),
)
file.copy(
  from = out_report,
  to = file.path("data", "reports", "plantBreedGame_initialisation_report.html")
)
