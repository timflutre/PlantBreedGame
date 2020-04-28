# script to install all apps dependencies
print(options("repos"))

# Dep from CRAN
install.packages(
  c(
    "DBI",
    "digest",
    "DT",
    "igraph",
    "knitr",
    "MASS",
    "parallel",
    "plotly",
    "RSQLite",
    "scrm",
    "shiny",
    "shinycssloaders",
    "shinydashboard",
    "shinyjs",
    "rmarkdown"
  )
)

# install installer
install.packages(c(
    "devtools",
    "BiocManager"))

# Dep from bioconductor
BiocManager::install("GenomicRanges")

# Dep from github
devtools::install_github("timflutre/rutilstimflutre")
