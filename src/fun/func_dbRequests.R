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

#' Create a safe SQL condition
#' @param logic AND or OR
#' @param column element on which to apply the condition
#' @param cond_type "IN", "=", "BETWEEN" ...
#' @param values
#'
#' @example
#' condition("AND", "id", "IN", c(1, 2, 3, 4))
#' # "AND id IN (1, 2, 3, 4)"
#' condition("AND", "name", "=", "' OR 1=1 --")
#' # "AND name = ''' OR 1=1 --'"
condition <- function(logic, column, cond_type, values) {
  if (is.null(values)) {
    return("")
  }

  safe_values <- dbQuoteLiteral(DBI::ANSI(), values)

  if (cond_type == "IN") {
    return(paste(logic, column, "IN (", paste(safe_values, collapse = ", ") , ")"))
  }
  if (cond_type == "=") {
    return(paste(logic, column, "=", safe_values))
  }
  if (cond_type == ">") {
    return(paste(logic, column, ">", safe_values))
  }
  if (cond_type == ">=") {
    return(paste(logic, column, ">=", safe_values))
  }
  if (cond_type == "<") {
    return(paste(logic, column, "<", safe_values))
  }
  if (cond_type == "<=") {
    return(paste(logic, column, "<=", safe_values))
  }
  if (cond_type == "BETWEEN") {
    return(paste(logic, column, "BETWEEN", safe_values[1], "AND", safe_values[2]))
  }
  stop("Condition type not known")
}


# Basic db requests functions ----
# those function are the basic function that send requests to the db
# they should not be used in the app code
# (except here for the "specific functions")


#' Basic SQL parser
#'
#' @param file path to `sql` file
#' @param collapse how each lines are concatenated
#'
#' @return character string
read_sql_file <- function(file, collapse = " ") {
  lines <- readLines(file)
  lines <- gsub("--.*$", "", lines)
  lines <- lines[lines != ""]
  paste0(lines, collapse = collapse)
}


#' connect to the db and return the connection
connect_to_db <- function(dbname = DATA_DB) {
  conn <- DBI::dbConnect(SQLite(), dbname = dbname)
  dbExecute(conn = conn, "PRAGMA foreign_keys = ON")
  conn
}

#' Send a get request to the db WITHOUT SQL INTERPOLATION !!!
#' can be prone to SQL injection
db_get <- function(query, dbname = DATA_DB) {
  # for SELECT query only
  conn <-  connect_to_db(dbname = dbname)
  tryCatch({
    out <- dbGetQuery(conn = conn, query)
  }, finally = {
    dbDisconnect(conn)
  })
  return(out)
}

#' Send a get request to the db safe from SQL injection
db_get_safe <- function(query, dbname = DATA_DB, ...) {
  conn <-  connect_to_db(dbname = dbname)
  out <- tryCatch({
    safe_query <- DBI::sqlInterpolate(conn, query, ...)
    dbGetQuery(conn = conn, safe_query)
  }, error = function(err) {
    stop(err)
  }, finally = {
    dbDisconnect(conn)
  })
  return(out)
}

#' Send a request that alter the db WITHOUT SQL INTERPOLATION !!!
#' can be prone to SQL injection
db_execute <- function(query, dbname = DATA_DB) {
  conn <-  connect_to_db(dbname = dbname)

  tryCatch({
    dbBegin(conn)
    lapply(
      strsplit(query, ";")[[1]],
      function(q) {dbExecute(conn, q)}
    )
    # dbExecute(conn = conn, query)
    dbCommit(conn)
  }, finally = {
    dbDisconnect(conn)
  })
  return(TRUE)
}

#' Send a request that alter the db safe from SQL injection
db_execute_safe <- function(query, dbname = DATA_DB, ...) {
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


#' add data to a table from a data.frame.
#' The data.frame must have have a structure matching the table
db_add_data <- function(table, data, append = TRUE, overwrite = FALSE, dbname = DATA_DB, ...) {
  conn <- connect_to_db()
  out <- tryCatch({
    db_col_names <- paste(colnames(data), collapse = ", ")
    query <- paste0("INSERT INTO ", table, " (", db_col_names, ") VALUES ")

    values <- paste(paste0("(", apply(data, 1, function(x){
      paste(dbQuoteLiteral(conn, x), collapse = ", ")
    }), ")"), collapse = ", ")
    query <- paste0(query, values, " RETURNING id")
    dbGetQuery(conn = conn, query)$id
  }, error = function(err) {
    stop(err)
  }, finally = {
    dbDisconnect(conn)
    })
  return(out)
}

# NOT USED
# #' Retrive data from the db from a data.frame
# #' This can be use to retrieve db data from a partial data
# db_get_data <- function(table, data) {
#
#   query <- paste("SELECT * FROM", table, "WHERE 1=2")
#
#   conditions <- apply(data, 1, function(x){
#     conditions <- na.omit(ifelse(is.na(x),
#                                  NA,
#                                  paste0(names(x),
#                                         " = ",
#                                         dbQuoteLiteral(DBI::ANSI(), x)
#                                  )
#     )
#     )
#     paste0(conditions, collapse = " AND ")
#   })
#
#   conditions <- paste0(" OR (", conditions, ")", collapse = " ")
#   query <- paste0(query, conditions, collapse = " ")
#   db_get(query)
# }

#' List data base tables
db_list_tables <- function(dbname = DATA_DB) {
  conn <-  connect_to_db(dbname = dbname)
  tryCatch({
    allTbls <- dbListTables(conn = conn)
  }, finally = {
    dbDisconnect(conn)
  })
  return(allTbls)
}


# Specific db requests functions ----
# Those function are specific to some tables of the db
# or for a specific usage in the app




## Constants ----

db_add_constants <- function(constants_list) {
  query <- "INSERT INTO constants VALUES (?key, ?value)"
  mapply(db_execute_safe,
         key = names(constants_list),
         value = constants_list,
         MoreArgs = list(query = query))
}

db_update_constants <- function(constants_list) {
  query <- "UPDATE constants SET value = ?value WHERE key = ?key"
  mapply(db_execute_safe,
         key = names(constants_list),
         value = constants_list,
         MoreArgs = list(query = query))
}

##' Get the breeding game constants
##'
##' Retrieve the constants used to parametrized the breeding game from the SQLite database.
##' @return list
##' @author Timothee Flutre
getBreedingGameConstants <- function() {
  ## retrieve the content of the table
  query <- "SELECT * FROM constants"
  out.df <- db_get(query)

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
  names(out.list) <- out.df$key

  return(out.list)
}







## Game sessions ----

db_add_game_session <- function(id = NA, startDate, endDate, yearTime, timeZone) {
  query <- paste0(
    "INSERT INTO sessions (id, start, end, year_time, time_zone)", " VALUES ",
    "(?id, ?startDate, ?endDate, ?yearTime, ?timeZone)"
  )

  db_execute_safe(query,
                          id = id,
                          startDate = startDate,
                          endDate = endDate,
                          yearTime = yearTime,
                          timeZone = timeZone
  )
}

db_get_game_sessions <- function() {
  query <- paste0("SELECT * FROM sessions")
  res <- db_get(query)
  return(res)
}


db_delete_game_session <- function(id) {
  query <- "DELETE FROM sessions WHERE id = ?id"
  db_execute_safe(query, id = id)
}








## Breeders ----
db_add_breeder <- function(name, status, hashed_psw) {
  query <- paste0(
    "INSERT INTO breeders (name, status, h_psw) VALUES (?name, ?status, ?hashed_psw) "
  )
  db_execute_safe(query,
                          name = name,
                          status = status,
                          hashed_psw = hashed_psw)
}

db_delete_breeder <- function(name) {
  query <- paste0(
    "DELETE FROM breeders WHERE name = ?name"
  )
  db_execute_safe(query, name = name)
}

getBreederList <- function(dbname = DATA_DB) {
  query <- paste0("SELECT name FROM breeders")
  breederNames <- db_get(query)[, 1]
  return(breederNames)
}

db_get_breeder <- function(breeder.name, dbname = DATA_DB) {
  query <- paste("SELECT * FROM breeders WHERE name = ?name")
  as.list(db_get_safe(query, name = breeder.name))
}









## Main game requests ----

db_add_request <- function(id,
                           breeder,
                           name,
                           type,
                           game_date) {
  query <- paste(
    "INSERT INTO requests",
    "(id, breeder, name, type, game_date)",
    "VALUES (?id, ?breeder, ?name, ?type, ?game_date)"
  )

  db_execute_safe(query,
                          id = id,
                          breeder = breeder,
                          name = name,
                          type = type,
                          game_date = game_date)
}

db_get_game_requests <- function(breeder = NULL, name = NULL, type = NULL, id = NULL) {

  breeder_condition <- ""
  if (!is.null(breeder)) {
    breeder_condition <- condition("AND", "breeder", "IN", c(breeder, "@ALL"))
  }

  query <- paste(
    "SELECT * FROM requests WHERE 1=1",
    breeder_condition,
    condition("AND", "name", "IN", name),
    condition("AND", "type", "IN", type),
    condition("AND", "id", "IN", id)
  )
  db_get(query)
}





#' Get requests' content
#' It will automatically call the corresponding table
#' (ie. `pheno_request`, `pltmat_request` or `geno_request`)
#'
#' @param breeder
#' @param type
#' @param name
#' @param id
db_get_game_requests_data <- function(breeder = NULL,
                                   type = NULL,
                                   name = NULL,
                                   id = NULL) {

  requests <- db_get_game_requests(breeder = breeder, name = name, type = type, id = id)
  # requests <- requests[, c("id", "type")]
  req_type <- unique(requests$type)
  if (length(unique(requests$type)) != 1) {
    stop("Requesting request data can be done on request of the same type")
  }

  table <- ""
  if (req_type == "pltmat") {
    table <- "pltmat_requests"
  }
  if (req_type == "pheno") {
    table <- "pheno_requests"
  }
  if (req_type == "geno") {
    table <- "geno_requests"
  }

  query <- paste("SELECT * FROM", table, "WHERE req_id IN (",
                 paste(requests$id, collapse = ", "),
                 ")")

  data <- db_get(query)
  data <- data[order(data$id, decreasing = T), ]

  full_data <- data %>%
    dplyr::left_join(requests %>%
                       dplyr::rename_with( ~ paste0("request_", .), -id),
                     by = c("req_id" = "id"))

  return(full_data)
}



## plant material requests ----

#' Add initial plant material data in the db
#' This function is different than `add_pltmat_req_data`
#' Because with `add_pltmat_req_data`:
#'  - The parents must exist in the db, which is not the case here
#'  - `add_pltmat_req_data` call `db_get_individuals_ids` that do not include those parents
db_add_init_pltmat_req_data <- function(req_id, data) {
  breeder <- "@ALL"

  # add initial collection's parents
  parent1_id <- db_add_data(table = "plant_material",
              data = data.frame(
                name = data$parent1,
                parent1_id = NA,
                parent2_id = NA,
                pltmat_request_id = NA,
                haplotype_file = NA
              ))
  colnames(data) <- c("parent1_request_name", "parent2_request_name", "child_name", "cross_type")
  data$req_id <- req_id
  data$parent1_id <- parent1_id
  new_ids <- db_add_data("pltmat_requests", data)

  query <- paste("SELECT * FROM pltmat_requests WHERE id IN (",
                 paste(new_ids, collapse = ", "),
                 ")")
  return(db_get(query))
}

db_add_pltmat_req_data <- function(req_id, data) {
  game_requests <- db_get_game_requests(id = req_id, type = "pltmat")
  if (length(game_requests) == 0) {
    stop("requested plant material request does not exist")
  }
  breeder <- game_requests$breeder

  ind_ids <- db_get_individuals_ids(breeder, c(data$parent1,
                                              data$parent2))
  parent1_id <- ind_ids[1:nrow(data)]
  parent2_id <- ind_ids[(nrow(data) + 1):(2 * nrow(data))]

  if (any(is.na(parent1_id))) {
    stop("Some requested individuals are missing in the db.")
  }
  if (any(is.na(parent2_id[!is.na(data$parent2)]))) {
    stop("Some requested individuals are missing in the db.")
  }

  colnames(data) <- c("parent1_request_name", "parent2_request_name", "child_name", "cross_type")
  data$req_id <- req_id
  data$parent1_id <- parent1_id
  data$parent2_id <- parent2_id
  new_ids <- db_add_data("pltmat_requests", data)

  query <- paste("SELECT * FROM pltmat_requests WHERE id IN (",
                 paste(new_ids, collapse = ", "),
                 ")")
  return(db_get(query))
}







## plant material ----

db_add_pltmat <- function(req_id) {
  query <- paste("
    INSERT INTO plant_material (name, parent1_id, parent2_id, pltmat_request_id)
      SELECT
        child_name,
        parent1_id,
        parent2_id,
        id
      FROM pltmat_requests
      WHERE req_id IN (", paste(req_id, collapse = ", "),")
    RETURNING id
  ")
  new_row_ids <- db_get(query)$id
}

#' Add individuals' haplotype_file path to the db
#' @params haplotype_file_data data.frame associating the haplotypes files
#' and their corresponding individuals id. 2 columns `id` and `haplotype_file`
db_add_ind_haplotype <- function(haplotype_file_data) {

  queries <- apply(haplotype_file_data, 1, function(x){
    id <- dbQuoteLiteral(DBI::ANSI(), x["id"])
    haplotype_file <- dbQuoteLiteral(DBI::ANSI(), x["haplotype_file"])
    paste("UPDATE plant_material SET haplotype_file =",
          haplotype_file,
          "WHERE id =",
          id)
  })
  query <- paste(queries, collapse = "; ")
  db_execute(query)
}

#' From a breeder and a vector of individuals names
#' return the vector of the individuals id in the same order (with repetition
#' if there is repetitions in the `name` vector).
#' Note: the parents of the initial collection are not returned
db_get_individuals_ids <- function(breeder, names) {
  ind_ids <- db_get_individual(breeder = breeder,
                               name = unique(names))[, c("id", "name")]
  ind_ids$id[match(names, ind_ids$name)]
}


#' Get individuals information humanly readable with information from
#' other tables. eg:
#' - the parents names are shown, not their id.
#' - the request name that generate the individual is displayed
#' - the "available date" is calculated by the DB
#'
#' Note: the parents of the initial collection are not returned
db_get_individual <- function(ind_id = NULL,
                              breeder = NULL,
                              name = NULL,
                              parent1 = NULL,
                              parent2 = NULL,
                              cross_type = NULL,
                              request_name = NULL,
                              n_pheno_min = NULL,
                              n_geno_min = NULL) {
  base_query <- "SELECT * FROM v_plant_material WHERE 1=1"

  breeder_condition <- ""
  if (!is.null(breeder)) {
    breeder_condition <- condition("AND", "breeder", "IN", c(breeder, "@ALL"))
  }

  query <- paste(
    base_query,
    condition("AND", "id", "IN", ind_id),
    breeder_condition,
    condition("AND", "name", "IN", name),
    condition("AND", "parent1", "IN", parent1),
    condition("AND", "parent2", "IN", parent2),
    condition("AND", "cross_type", "IN", cross_type),
    condition("AND", "request_name", "IN", request_name),
    condition("AND", "n_pheno", ">=", n_pheno_min),
    condition("AND", "n_geno", ">=", n_geno_min)
  )
  db_get(query)
}



#' Get individuals data from the plant_material table as is
db_get_individual_raw <- function(name = NULL, id = NULL) {
  query <- paste(
    "SELECT * FROM plant_material WHERE 1=1",
    condition("AND", "name", "IN", name),
    condition("AND", "id", "IN", id)
  )
  db_get(query)
}

## phenotype requests ----

add_pheno_req_data <- function(req_id, request_data) {
  game_requests <- db_get_game_requests(id = req_id, type = "pheno")
  if (length(game_requests) == 0) {
    stop("requested phenotype request does not exist")
  }
  breeder <- game_requests$breeder
  ind_ids <- db_get_individuals_ids(breeder, request_data$ind)

  if (any(is.na(ind_ids))) {
    stop("Some requested individuals are missing in the db.")
  }
  request_data <- request_data[, c("ind", "task", "details")]
  colnames(request_data) <- c("ind_request_name", "type", "n_pheno")
  request_data$req_id <- req_id
  request_data$ind_id <- ind_ids
  new_ids <- db_add_data("pheno_requests", request_data)

  query <- paste("SELECT * FROM pheno_requests WHERE id IN (",
                 paste(new_ids, collapse = ", "),
                 ")")
  return(db_get(query))
}

## phenotype ----

#' Initialise the db with initial phenotypes related data
#'
#' From the initial raw phenotypes data, this function will
#' create the related requests in the db (table `requests` and `pheno_requests`)
#'  and the initial phenotypes data
#'
#' @param init_pheno_data raw data of the initial phenotypes
#'
db_add_initial_pheno_data <- function(init_pheno_data) {
  pheno_years <- unique(init_pheno_data$year)
  lapply(pheno_years, FUN = function(year) {
    pheno_data <- dplyr::group_by(init_pheno_data[init_pheno_data$year == year, ],
                                  ind)
    request_data <- dplyr::summarise(pheno_data, details = dplyr::n())
    request_data$task <- "pheno-field"

    request_name <- paste("-- Initial phenotype year", year, "--")
    db_add_request(id = NA,
                        breeder = "@ALL",
                        name = request_name,
                        type = "pheno",
                        game_date = paste0(year, "-01-01"))
    pheno_req_id <- db_get_game_requests(breeder = "@ALL", name = request_name)[1, 1]

    add_pheno_req_data(pheno_req_id, request_data)
    db_add_pheno_data(pheno_data, pheno_req_id)
  })
}

#' Add phenotypic data in the db
#'
#' @param pheno_data data.frame of the phenotypic data
#' @param pheno_req_id corresponding main request id
db_add_pheno_data <- function(pheno_data, pheno_req_id) {
  pheno_request <- db_get_game_requests_data(id = pheno_req_id, type = "pheno")
  if (nrow(pheno_request) == 0) {
    stop("No phenotype's request data found")
  }
  pheno_field_data <- pheno_data[!is.na(pheno_data$trait1),]
  pheno_request_field <- pheno_request[pheno_request$type == "pheno-field", c("id", "ind_request_name", "ind_id")]
  pheno_field_data <- dplyr::left_join(pheno_field_data,
                                       pheno_request_field,
                                       by = dplyr::join_by(ind == ind_request_name))

  pheno_request_patho <- pheno_request[pheno_request$type == "pheno-patho", c("id", "ind_request_name", "ind_id")]
  pheno_patho_data <- pheno_data[is.na(pheno_data$trait1),]
  pheno_patho_data <- dplyr::left_join(pheno_patho_data,
                                       pheno_request_patho,
                                       by = dplyr::join_by(ind == ind_request_name))
  pheno_data <- rbind(pheno_field_data, pheno_patho_data)
  pheno_data <- pheno_data[, c("year", "plot", "pathogen", "trait1", "trait2", "trait3", "id")]
  colnames(pheno_data) <- c("year", "plot", "pathogen", "trait1", "trait2", "trait3", "pheno_req_id")

  new_ids <- db_add_data("phenotypes", pheno_data)
  query <- paste("SELECT * FROM phenotypes WHERE id IN (",
                 paste(new_ids, collapse = ", "),
                 ")")
  return(db_get(query))
}


#' Get phenotype data humanly readable with information from
#' other tables.
db_get_phenotypes <- function(id = NULL,
                              breeder = NULL,
                              ind_id = NULL,
                              ind_name = NULL,
                              type = NULL,
                              request_name = NULL,
                              plot = NULL,
                              min_t1 = NULL,
                              min_t2 = NULL,
                              max_t1 = NULL,
                              max_t2 = NULL,
                              t3 = NULL,
                              pathogen = NULL,
                              year = NULL,
                              initial_data_only = NULL
                              ) {
  base_query <- "SELECT * FROM v_phenotypes WHERE 1=1"

  breeder_condition <- ""
  if (!is.null(breeder)) {
    breeder_condition <- condition("AND", "breeder", "IN", c(breeder, "@ALL"))
  }

  initial_data_condition <- ""
  if (!is.null(initial_data_only)) {
    stopifnot(length(initial_data_only) == 1)
    if (initial_data_only) {
      initial_data_condition <- condition("AND", "breeder", "=", "@ALL")
    }
  }

  query <- paste(
    base_query,
    condition("AND", "id", "IN", id),
    breeder_condition,
    condition("AND", "ind_id", "IN", ind_id),
    condition("AND", "ind", "IN", ind_name),
    condition("AND", "type", "IN", type),
    condition("AND", "request_name", "IN", request_name),
    condition("AND", "plot", "IN", plot),
    condition("AND", "trait1", ">=", min_t1),
    condition("AND", "trait2", ">=", min_t2),
    condition("AND", "trait1", "<=", max_t1),
    condition("AND", "trait2", "<=", max_t2),
    condition("AND", "trait3", "IN", t3),
    condition("AND", "pathogen", "IN", pathogen),
    condition("AND", "year", "IN", year),
    initial_data_condition
  )
  db_get(query)
}


## genotype requests ----

db_add_geno_req_data <- function(req_id, request_data) {
  game_requests <- db_get_game_requests(id = req_id, type = "geno")
  if (length(game_requests) == 0) {
    stop("requested genotype request does not exist")
  }
  breeder <- game_requests$breeder
  ind_ids <- db_get_individuals_ids(breeder, request_data$ind)

  if (any(is.na(ind_ids))) {
    stop("Some requested individuals are missing in the db.")
  }
  request_data <- request_data[, c("ind", "details")]
  colnames(request_data) <- c("ind_request_name", "type")
  request_data$req_id <- req_id
  request_data$ind_id <- ind_ids
  new_ids <- db_add_data("geno_requests", request_data)

  query <- paste("SELECT * FROM geno_requests WHERE id IN (",
                 paste(new_ids, collapse = ", "),
                 ")")
  return(db_get(query))
}


## genotype ----

#' add genetic data to the data base
#'
#' @param geno_req_id genotype request id (in geno_requests table)
#' @param genotype_data_files named list of the path of the results files
#' names should be `hd`, `ld`, `snp` and the associated values must be the path
#' of the corresponding result files
db_add_geno_data <- function(geno_req_id, genotype_data_files) {
  geno_request <- db_get_game_requests_data(id = geno_req_id, type = "geno")
  if (nrow(geno_request) == 0) {
    stop("No genotype's request data found")
  }
  geno_data_hd <- geno_request[geno_request$type == "hd",]
  geno_data_hd$result_file <- genotype_data_files$hd

  geno_data_ld <- geno_request[geno_request$type == "ld",]
  geno_data_ld$result_file <- genotype_data_files$ld

  geno_data_snp <- geno_request[!geno_request$type %in% c("hd", "ld"),]
  geno_data_snp$result_file <- genotype_data_files$snp

  geno_data <- rbind(geno_data_hd, geno_data_ld, geno_data_snp)

  geno_data <- geno_data[, c("id", "type", "result_file")]
  colnames(geno_data) <- c("geno_req_id", "type", "result_file")

  new_ids <- db_add_data("genotypes", geno_data)
  query <- paste("SELECT * FROM genotypes WHERE id IN (",
                 paste(new_ids, collapse = ", "),
                 ")")
  return(db_get(query))
}

db_get_genotypes <- function(id = NULL,
                             breeder = NULL,
                             ind_id = NULL,
                             ind_name = NULL,
                             type = NULL,
                             snp = NULL,
                             request_name = NULL,
                             result_file = NULL
) {
  base_query <- "SELECT * FROM v_genotypes WHERE 1=1"

  breeder_condition <- ""
  if (!is.null(breeder)) {
    breeder_condition <- condition("AND", "breeder", "IN", c(breeder, "@ALL"))
  }

  query <- paste(
    base_query,
    condition("AND", "id", "IN", id),
    breeder_condition,
    condition("AND", "ind_id", "IN", ind_id),
    condition("AND", "ind", "IN", ind_name),
    condition("AND", "type", "IN", type),
    condition("AND", "request_name", "IN", request_name),
    condition("AND", "result_file", "IN", result_file)
  )
  db_get(query)
}

















# Other not related to the db but to game's data ----

getSNPsubset <- function() {
  # this could be saved in the db rather than in files
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
