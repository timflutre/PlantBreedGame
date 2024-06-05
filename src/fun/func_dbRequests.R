## Copyright 2015~2024 Institut National de la Recherche Agronomique
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

## Functions related to data file / data-base requests

db_get_request <- function(query, dbname = DATA_DB) {
  # for SELECT query only
  conn <- dbConnect(SQLite(), dbname = dbname)
  tryCatch({
    out <- dbGetQuery(conn = conn, query)
  }, finally = {
    dbDisconnect(conn)
  })
  return(out)
}

db_execute_request <- function(query, dbname = DATA_DB) {
  conn <- dbConnect(SQLite(), dbname = dbname)
  tryCatch({
    dbExecute(conn = conn, query)
  }, finally = {
    dbDisconnect(conn)
  })
  return(TRUE)
}

db_list_tables <- function(dbname = DATA_DB) {
  conn <- dbConnect(SQLite(), dbname = dbname)
  tryCatch({
    allTbls <- dbListTables(conn = conn)
  }, finally = {
    dbDisconnect(conn)
  })
  return(allTbls)
}

getBreederList <- function(dbname) {
  query <- paste0("SELECT name FROM breeders")
  breederNames <- db_get_request(query)[, 1]
  return(breederNames)
}

getBreederStatus <- function(dbname, breeder.name) {
  query <- paste0(
    "SELECT status FROM breeders WHERE name = '", breeder.name, "'"
  )
  breeder.status <- db_get_request(query)[, 1]
  return(breeder.status)
}



##' Get the breeding game constants
##'
##' Retrieve the constants used to parametrized the breeding game from the SQLite database.
##' @return list
##' @author Timothee Flutre
getBreedingGameConstants <- function() {
  ## retrieve the content of the table
  query <- "SELECT * FROM constants"
  out.df <- db_get_request(query)

  ## reformat
  # suppress "NAs introduced by coercion" warning
  withCallingHandlers(
    {
      out.list <- lapply(out.df$value, function(x) {
        ifelse(!is.na(as.numeric(x)),
          as.numeric(x),
          x
        )
      })
    },
    warning = function(warn) {
      warning_to_catch <- "NAs introduced by coercion"
      if (identical(warn$message, warning_to_catch)) {
        tryInvokeRestart("muffleWarning")
      }
    }
  )
  names(out.list) <- out.df$item

  return(out.list)
}

getSNPsubset <- function() {
  snpcoord_hd_file <- file.path(DATA_INITIAL_DATA, "snp_coords_hd.txt.gz")
  snpcoord_ld_file <- file.path(DATA_INITIAL_DATA, "snp_coords_ld.txt.gz")

  if (!file.exists(snpcoord_hd_file) || !file.exists(snpcoord_ld_file)) {
    return(NULL)
  }
  subset.snps <- list()
  subset.snps[["hd"]] <- rownames(read.table(snpcoord_hd_file))
  subset.snps[["ld"]] <- rownames(read.table(snpcoord_ld_file))
  return(subset.snps)
}
