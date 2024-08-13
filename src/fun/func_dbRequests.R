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

connect_to_db <- function(dbname = dbname) {
  dbConnect(SQLite(), dbname = dbname)
}

db_get_request <- function(query, dbname = DATA_DB) {
  # for SELECT query only
  conn <-  connect_to_db(dbname = dbname)
  tryCatch({
    out <- dbGetQuery(conn = conn, query)
  }, finally = {
    dbDisconnect(conn)
  })
  return(out)
}

db_execute_request <- function(query, dbname = DATA_DB) {
  conn <-  connect_to_db(dbname = dbname)
  tryCatch({
    dbExecute(conn = conn, query)
  }, finally = {
    dbDisconnect(conn)
  })
  return(TRUE)
}
db_execute_request_safe <- function(query, dbname = DATA_DB, ...) {
  conn <-  connect_to_db(dbname = dbname)
  tryCatch({
    safe_query <- DBI::sqlInterpolate(conn, query, ...)
    dbExecute(conn = conn, safe_query)
  }, error = function(err) {
    stop(err)
  }, finally = {
    dbDisconnect(conn)
  })
  return(TRUE)


}


db_list_tables <- function(dbname = DATA_DB) {
  conn <-  connect_to_db(dbname = dbname)
  tryCatch({
    allTbls <- dbListTables(conn = conn)
  }, finally = {
    dbDisconnect(conn)
  })
  return(allTbls)
}

getBreederList <- function(dbname = DATA_DB) {
  query <- paste0("SELECT name FROM breeders")
  breederNames <- db_get_request(query)[, 1]
  return(breederNames)
}

getBreederStatus <- function(breeder.name, dbname = DATA_DB) {
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

getBreedersIndividuals <- function(breeder) {
  # return a data-frame of all the breeder's individuals

  tbl <- paste0("plant_material_", breeder)
  query <- paste0("SELECT * FROM ", tbl)
  return(db_get_request(query))
}

getGameSessions <- function() {
  query <- paste0("SELECT * FROM sessions")
  res <- db_get_request(query)
  return(res)
}

addGameSession <- function(id, startDate, endDate, yearTime, timeZone) {

    query <- paste0(
      "INSERT INTO sessions", " VALUES",
      # " ('", id, "','", startDate, "','", endDate, "','", yearTime, "','", timeZone,"')"
      "(?id,?startDate,?endDate,?yearTime,?timeZone)"
    )
  db_execute_request_safe(query,
    id = id,
    startDate = startDate,
    endDate = endDate,
    yearTime = yearTime,
    timeZone = timeZone
  )


}

delGameSession <- function(id) {
  query <- "DELETE FROM sessions WHERE id = ?id"
  db_execute_request_safe(query, id = id)
}


get_folder_size <- function(dir, as_string = FALSE) {
  size <- as.numeric(strsplit(system(paste("du -s --block-size=1", dir), intern = TRUE), "\t")[[1]][1])
  if (as_string) {
    return(prettyunits::pretty_bytes(size, style = "6"))
  }
  return(size)
}

get_files_size <- function(files, as_string = FALSE) {
  size <- file.info(files)$size
  if (as_string) {
    return(prettyunits::pretty_bytes(size))
  }
  return(size)
}

get_folder_tree <- function(dir, exclude_files = FALSE, excluded_files_ext = NULL) {
  dirs_fullnames <- list.dirs(path = dir, recursive = F)
  dirs_list <- lapply(dirs_fullnames, function(dir) {
    structure(get_folder_tree(dir,
                              exclude_files = exclude_files,
                              excluded_files_ext = excluded_files_ext),
              sttype = "directory")
  })
  dirs <- basename(dirs_fullnames)
  if (length(dirs) > 0) {
    dir_sizes <- sapply(dirs_fullnames, get_folder_size, as_string = TRUE)
    names(dirs_list) <- paste0(dirs, " (", dir_sizes, ")")
  }

  if (exclude_files) {
    file_list <- list()
  } else {
    files_fullnames <- list.files(path = dir, full.names = TRUE, include.dirs = F)
    files_fullnames <- setdiff(files_fullnames, dirs_fullnames)

    if (!is.null(excluded_files_ext)) {
      excl_file_regex <- paste0("\\.(", paste0(excluded_files_ext, collapse = "|"), ")$")
      excl_files <- grepl(excl_file_regex, files_fullnames)
      files_fullnames <- files_fullnames[!excl_files]
    }

    files <- basename(files_fullnames)
    file_list <- lapply(files, function(f){
      structure("", sttype="file")
    })
    if (length(files) > 0) {
      file_sizes <- get_files_size(files_fullnames, as_string = TRUE)
      names(file_list) <- paste0(files, " (", file_sizes, ")")
    }
  }

  tree_list <- c(dirs_list, file_list)
  return(tree_list)
}

