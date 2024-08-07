## Copyright 2015,2016,2017,2018,2019 Institut National de la Recherche Agronomique
## and Montpellier SupAgro.
##
## This file is part of PlantBreedGame.
##
## PlantBreedGame is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## PlantBreedGame is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with PlantBreedGame.  If not, see
## <http://www.gnu.org/licenses/>.


## required packages available on the CRAN
## https://cran.r-project.org/
## R> install.packages("<package_name>")

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyjs)
  library(shinyvalidate)
  library(RSQLite)
  library(MASS)
  library(digest)
  library(plotly)
  library(DT)
  library(igraph)
  library(lubridate)
  library(vistime)
  library(tidyr)
  library(bsicons)

  ## required packages NOT available on the CRAN
  ## R> devtools::install_github("timflutre/rutilstimflutre")
  library(rutilstimflutre)
  stopifnot(compareVersion(
    "0.158.2",
    as.character(packageVersion("rutilstimflutre"))
  )
  != 1)
})
