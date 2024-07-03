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


source("src/dependencies.R", local = TRUE, encoding = "UTF-8")

source("src/fun/functions.R", local = TRUE, encoding = "UTF-8")
source("src/fun/func_time.R", local = TRUE, encoding = "UTF-8")
source("src/fun/func_id.R", local = TRUE, encoding = "UTF-8")
source("./src/fun/module_constants.R", local = TRUE, encoding = "UTF-8")
source("./src/fun/module_breederList.R", local = TRUE, encoding = "UTF-8")
source("./src/fun/func_dbRequests.R", local = TRUE, encoding = "UTF-8")
source("./src/fun/module_gameInit_params.R", local = TRUE, encoding = "UTF-8")
source("./src/fun/func_gameInit_validation.R", local = TRUE, encoding = "UTF-8")

## -------------------------------------------------------------------
## parameters

options(warn = -1) # don't show warnings
options(stringsAsFactors = TRUE) # R-4.0.0 compatibility
debugDisplay <- FALSE # display debug

mycolors <- c("#00aedb", "#a200ff", "#f47835", "#d41243", "#8ec127")

if (Sys.info()["sysname"] == "Windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
}

## -------------------------------------------------------------------
## variables

DATA_ROOT <- normalizePath("./data", mustWork = TRUE)
DATA_TRUTH <- file.path(DATA_ROOT, "truth")
DATA_SHARED <- file.path(DATA_ROOT, "shared")
DATA_INITIAL_DATA <- file.path(DATA_SHARED, "initial_data")
DATA_DB <- file.path(DATA_ROOT, "breeding-game.sqlite")
DATA_REPORTS <- file.path(DATA_ROOT, "reports")
GAME_INIT_REPORT <- file.path(DATA_REPORTS, "plantBreedGame_initialisation_report.html")

if (dir.exists(DATA_REPORTS)) {
  addResourcePath("reports", DATA_REPORTS)
}

url.repo <- "https://github.com/timflutre/PlantBreedGame"
code.version <- getCodeVersion(url.repo)
