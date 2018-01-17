## Copyright 2015,2016,2017,2018 Institut National de la Recherche Agronomique
## and Montpellier SupAgro.
##
## This file is part of PlantSelBreedGame.
##
## PlantSelBreedGame is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## PlantSelBreedGame is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with PlantSelBreedGame.  If not, see
## <http://www.gnu.org/licenses/>.

## -------------------------------------------------------------------
## packages

library(shiny)
library(shinydashboard)
library(RSQLite)
library(MASS)
library(digest)

library(rutilstimflutre) # https://github.com/timflutre/rutilstimflutre
stopifnot(compareVersion("0.156.5",
                         as.character(packageVersion("rutilstimflutre")))
          != 1)


## -------------------------------------------------------------------
## functions

source("src/functions.R", local=TRUE, encoding="UTF-8")$value
source("src/func_time.R", local=TRUE, encoding="UTF-8")$value


## -------------------------------------------------------------------
## variables

init.funds <- 100000
## breeder <- "test"
year <- 2015

currentGTime <- reactive({
  ## this reactive variable is reevaluated every second
  invalidateLater(10000)
  getGameTime(setup)
})

root.dir <- "data"
setup <- getBreedingGameSetup(root.dir)
constants <- getBreedingGameConstants(setup$dbname)
constants$max.upload.pheno.field <- as.Date(constants$max.upload.pheno.field,
                                            format="%m-%d")

subset.snps <- list()
f <- paste0(setup$init.dir, "/snp_coords_hd.txt.gz")
subset.snps[["hd"]] <- rownames(read.table(f))
f <- paste0(setup$init.dir, "/snp_coords_ld.txt.gz")
subset.snps[["ld"]] <- rownames(read.table(f))


## -------------------------------------------------------------------
## functions

readCheckBreedDataFileJD <- function (f = NULL, df = NULL, max.nb.plots = 300, subset.snps,
                                      max.nb.inds = 1000, breeder){

  stopifnot(!is.null(f) || !is.null(df),
            is.numeric(max.nb.plots),
            length(max.nb.plots) == 1,
            max.nb.plots > 0,
            is.list(subset.snps),
            all(names(subset.snps) %in% c("ld", "hd")))

  if (is.null(df)) {
    stopifnot(file.exists(f))
    df <- utils::read.table(f,header = TRUE,
                            sep = "\t",
                            stringsAsFactors = FALSE)
  }


  stopifnot(is.data.frame(df),
            ncol(df) >= 3,
            all(c("ind","task", "details") %in% colnames(df)),
            all(!is.na(df$ind)),
            length(unique(df$ind)) <= max.nb.inds,
            all(!grepl("[^[:alnum:]._-]",df$ind)),
            all(!is.na(df$task)),
            all(df$task %in% c("pheno", "geno")))


  if ("pheno" %in% df$task) {
    tmp <- suppressWarnings(as.numeric(df$details[df$task == "pheno"]))
    stopifnot(all(!is.na(tmp)),
              !anyDuplicated(df$ind[df$task == "pheno"]),
              sum(as.numeric(df$details[df$task == "pheno"])) <= max.nb.plots)
  }

  if ("geno" %in% df$task) {
    stopifnot(all(grepl("hd|ld|snp", df$details[df$task == "geno"])))
    idx.notsnp <- df$task == "geno" & !grepl("snp", df$details)
    stopifnot(!anyDuplicated(df$ind[idx.notsnp]),
              all(df$details[df$task == "geno" & !df$details %in% c("ld", "hd")] %in% subset.snps[["hd"]]))
  }

  ## 2. check that the requested individuals already exist
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  tbl <- paste0("plant_material_", breeder)
  stopifnot(tbl %in% dbListTables(db))
  query <- paste0("SELECT child FROM ", tbl)
  res <- dbGetQuery(conn=db, query)
  stopifnot(all(df$ind %in% res$child))
  # disconnect db
  dbDisconnect(db)

  invisible(df)
}
