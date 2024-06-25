#! /usr/bin/env Rscript

out_report <- rmarkdown::render("./src/plantbreedgame_setup.Rmd",
  output_file = tempfile(),
  encoding = "UTF-8"
)
file.copy(
  from = out_report,
  to = file.path("data", "reports", "plantBreedGame_initialisation_report.html")
)
