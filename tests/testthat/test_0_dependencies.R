packages <- c(
  "DBI",
  "DT",
  "GenomicRanges",
  "MASS",
  "RSQLite",
  "bsicons",
  "data.table",
  "digest",
  "dplyr",
  "htmltools",
  "igraph",
  "jsonlite",
  "lubridate",
  "plotly",
  "prettyunits",
  "reactable",
  "rmarkdown",
  "rutilstimflutre",
  "scrm",
  "shiny",
  "shinyTree",
  "shinyWidgets",
  "shinycssloaders",
  "shinydashboard",
  "shinyjs",
  "shinyvalidate",
  "tidyr",
  "vistime"
)

for (pkg in packages) {
  test_that(paste("Package", pkg, "is installed"), {
    expect_no_error({
      suppressWarnings({
        library(package = pkg, character.only = TRUE)
      })
    })
  })
}
