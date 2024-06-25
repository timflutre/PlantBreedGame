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

clean_data_root <- function(
  data_root = DATA_ROOT,
  data_truth = DATA_TRUTH,
  data_shared = DATA_SHARED,
  data_initial_data = DATA_INITIAL_DATA,
  data_db = DATA_DB,
  data_reports = DATA_REPORTS,
  game_initialisation_report = GAME_INIT_REPORT
) {
  # WARN / TODO --- IMPORTANT ! ---
  # the initialisation script do not allow its execution if "the data" folder
  # already exists.
  # Therefore here we will delete this folder, however, in general,
  # IT IS QUITE RISKY to delete folder with code. For example:
  # - if `DATA_ROOT` have been wrongly defined
  # - if a malicious agent placed files/folder inside DATA_ROOT
  # - if files are currently beeing created
  #
  # A better approach could be instead to only create a new `DATA_ROOT` folder
  # and save in the data-base (that should only be erase, not deleted) the current
  # `DATA_ROOT` to use.
  # The server administrator would then responsible to safely remove the unecessary data.
  #
  # Here, to mitigate the risks the application will remove the files it
  # has created (based on their names) and the folders if they are empty.
  #
  # WARN / TODO --- IMPORTANT ! ---


  # initial files.
  initial_haplo_files <- sprintf( # `Coll0001_haplos.RData` to `Coll1000_haplos.RData`
    paste0(
      "Coll",
      "%0", floor(log10(1000)) + 1, "i",
      "_haplos.RData"
    ),
    seq(1000)
  )

  initial_files_truth <- file.path(data_truth, c(
    "afs0.RData",
    "allBV.RData",
    "coll.RData",
    "g0.RData",
    "p0.RData",
    initial_haplo_files
  ))
  initial_files_shared <- file.path(data_shared, c(
    file.path("initial_data", c(
      "controls.txt",
      "example_request_plant_material.txt",
      "Result_phenos_controls.txt.gz",
      "snp_coords_hd.txt.gz",
      "example_request_data.txt",
      "Result_genos_subset-initialColl-hd.txt.gz",
      "Result_phenos_initialColl.txt.gz",
      "snp_coords_ld.txt.gz"
    )),
    "Evaluation.txt"
  ))

  # breeder related files
  breeders_files <- unlist(lapply(getBreederList(data_db), function(breeder) {
    # truth
    all_breeder_inds <- getBreedersIndividuals(breeder)$child
    haplo_files <- c(
      initial_haplo_files,
      paste0(all_breeder_inds, "_haplos.RData")
    )
    truth_files <- file.path(data_truth, breeder, haplo_files)

    # shared
    shared_files <- file.path(data_shared, breeder, c(
      list.files(
        file.path(data_shared, breeder),
        pattern = "^IndList_\\d{4}-\\d{2}-\\d{2}(_\\d+)*\\.txt$"
      ),
      list.files(
        file.path(data_shared, breeder),
        pattern = "^Request-geno_(.*)\\.txt$"
      ),
      list.files(
        file.path(data_shared, breeder),
        pattern = "^Request-pheno_(.*)\\.txt$"
      ),
      list.files(
        file.path(data_shared, breeder),
        pattern = "^Request-pltMat_(.*)\\.txt$"
      ),
      list.files(
        file.path(data_shared, breeder),
        pattern = "^Result_genos-((hd)|(ld)|(single-snps))_(.*)_\\d{4}-\\d{2}-\\d{2}(_\\d)*\\.txt\\.gz$"
      ),
      list.files(
        file.path(data_shared, breeder),
        pattern = "^Result_pheno-((field)|(patho))_(.*)_\\d{4}-\\d{2}-\\d{2}(_\\d)*\\.txt\\.gz$"
      )
    ))
    return(c(truth_files, shared_files))
  }))

  # delete files
  all_files <- c(
    initial_files_truth,
    initial_files_shared,
    game_initialisation_report,
    breeders_files
  )
  file.remove(all_files)

  lapply(getBreederList(data_db), function(breeder) {
    if (length(list.files(file.path(data_shared, breeder))) == 0) {
      file.remove(file.path(data_shared, breeder))
    } else {
      stop(paste("can't remove", file.path(data_shared, breeder), "folder not empty."))
    }
    if (length(list.files(file.path(data_truth, breeder))) == 0) {
      file.remove(file.path(data_truth, breeder))
    } else {
      stop(paste("can't remove", file.path(data_truth, breeder), "folder not empty."))
    }
  })

  if (length(list.files(data_truth)) == 0) {
    file.remove(data_truth)
  } else {
    stop(paste("can't remove", data_truth, "folder not empty."))
  }
  if (length(list.files(file.path(data_shared, "initial_data"))) == 0) {
    file.remove(file.path(data_shared, "initial_data"))
  } else {
    stop(paste("can't remove", file.path(data_shared, "initial_data"), "folder not empty."))
  }
  if (length(list.files(data_shared)) == 0) {
    file.remove(data_shared)
  } else {
    stop(paste("can't remove", data_shared, "folder not empty."))
  }
  if (length(list.files(data_reports)) == 0) {
    file.remove(data_reports)
  } else {
    stop(paste("can't remove", data_reports, "folder not empty."))
  }

  file.remove(data_db)

  if (length(list.files(data_root)) != 0) {
    stop(paste0("Problem occured when cleaning `", data_root, "`, folder is not empty."))
  }
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

