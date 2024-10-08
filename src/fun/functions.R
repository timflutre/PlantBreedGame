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

##' Get code version
##'
##' Returns the code version of the local git repository from which the application is run, and, possibly, a link to the GitHub repository.
##' @return list with two components, display and link, to be used as a(display, href=link)
##' @author Timothee Flutre
##' @export
getCodeVersion <- function(url.repo = "") {
  code.version <- list(display = "", link = "")

  current_version <- readLines("VERSION")
  code.version$display <- current_version
  code.version$link <- paste0(url.repo, "/tree/v", current_version)

  return(code.version)
}

##' Simul breeding game
##'
##' Make the structure of the data.frame that will be given to the players when they request phenotyping during the game.
##' @param ind.ids vector of genotype identifiers (will be sorted using \code{\link{sort}}))
##' @param nb.plots.per.ind vector with the number of plots at which each genotype should be phenotype (will be sorted in the same order as \code{ind.ids})
##' @param year numeric of the year at which phenotyping occurs
##' @param pathogen if TRUE, the pathogen will be present the given year
##' @return data.frame
##' @author Timothee Flutre
##' @seealso \code{\link{makeDfInitPhenos}}
##' @export
makeDfPhenos <- function(ind.ids, nb.plots.per.ind, year, pathogen) {
  stopifnot(
    is.character(ind.ids),
    is.numeric(nb.plots.per.ind),
    length(nb.plots.per.ind) == length(ind.ids),
    is.numeric(year),
    is.logical(pathogen)
  )

  tmp <- data.frame(
    ind.id = ind.ids, nb.plots = nb.plots.per.ind,
    stringsAsFactors = FALSE
  )
  tmp <- tmp[order(tmp$ind.id), ]

  df <- data.frame(
    ind = as.factor(rep(tmp$ind.id, tmp$nb.plots)),
    year = as.factor(rep(year, sum(tmp$nb.plots))),
    plot = as.factor(1:sum(tmp$nb.plots)),
    pathogen = pathogen,
    trait1.raw = NA,
    trait1 = NA,
    trait2 = NA,
    trait3 = NA
  )

  return(df)
}

##' Read for breeding game
##'
##' Read and check a file supposed to contain requests about plant material.
##' It should have 3 columns named \code{parent1}, \code{parent2} and \code{child}.
##' @param f path to the input file (columns should be separated by a tabulation)
##' @param df data.frame (if the file was already read)
##' @param max.nb maximum number of requests in a single file
##' @param max.nb.hd maximum number of haplodiploidization to request in a single file
##' @return invisible data.frame
##' @author Timothee Flutre
##' @seealso \code{\link{makeExamplePlantFile}}
##' @export
readCheckBreedPlantFile <- function(f = NULL, df = NULL, max.nb, max.nb.hd) {
  stopifnot(
    !is.null(f) || !is.null(df),
    is.numeric(max.nb),
    is.numeric(max.nb.hd),
    length(max.nb) == 1,
    length(max.nb.hd) == 1,
    max.nb > 0,
    max.nb.hd > 0
  )

  if (is.null(df)) {
    stopifnot(file.exists(f))
    df <- utils::read.table(f, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  }


  # check:
  if (!is.data.frame(df)) {
    stop("df must be a data.frame, or your file can't be read.")
  }
  if (ncol(df) < 3) {
    stop(paste0(
      "Your file has less than 3 columns, please use a tabulation as separator,",
      " or check the encoding of your file (must be \"UTF-8\" without BOM)"
    ))
  }
  if (!all(c("parent1", "parent2", "child") %in% colnames(df))) {
    stop(paste0("Your file must contain \"parent1\", \"parent2\" and \"child\" columns, instead of: \"",
      paste(colnames(df), collapse = "\", \""),
      "\". (Be sure your are using the \"UTF-8\" encoding without BOM.)",
      collapse = ""
    ))
  }
  if (!all(!is.na(df$parent1)) | any(df$parent1 == "")) {
    stop("There is some \"NA\" values in the \"parent1\" column.")
  }
  if (!all(!is.na(df$child)) | any(df$child == "")) {
    stop("There is some \"NA\" values in the \"child\" column.")
  }
  if (anyDuplicated(df$child) != 0) {
    stop("There are duplicates in the \"child\" column.")
  }
  if (!all(!grepl("[^[:alnum:]._-]", df$child))) {
    stop("Some child's ids are not good.")
  }
  if (sum(is.na(df$parent2)) > max.nb.hd) {
    stop(paste0("Sorry, you can not request more than ", max.nb.hd, " haplo-diploidisations",
      collapse = ""
    ))
  }

  if (nrow(df) > max.nb) {
    stop(paste0(
      "Too many lines in your request. The maximum is: ", max.nb,
      ".\nYour request have ", nrow(df), " lines.\n",
      "Please split your request in several files."
    ))
  }



  df$parent2[df$parent2 == ""] <- NA

  ## add the column "explanations" if necessary
  if (!"explanations" %in% colnames(df)) {
    df$explanations <- NA

    idx <- which(df$parent1 != df$parent2 &
      !is.na(df$parent2))
    if (length(idx) > 0) {
      df$explanations[idx] <- "allofecundation"
    }

    idx <- which(df$parent1 == df$parent2 &
      !is.na(df$parent2))
    if (length(idx) > 0) {
      df$explanations[idx] <- "autofecundation"
    }

    idx <- which(is.na(df$parent2))
    if (length(idx) > 0) {
      df$explanations[idx] <- "haplodiploidization"
    }

    if (any(is.na(df$explanations))) {
      stop("for some individuals, can't deduce the breeding type.")
    }
  }

  invisible(df)
}

##' Read for breeding game
##'
##' Read and check a file supposed to contain requests about pheno/geno data.
##' It should have 3 columns named \code{ind}, \code{task} and \code{details}.
##' @param f path to the input file (columns should be separated by a tabulation)
##' @param df data.frame (if the file was already read)
##' @param max.nb.plots maximum number of plots
##' @param subset.snps list with two components named "ld" and "hd" containing vector of SNP identifiers
##' @param max.nb.inds maximum number of unique individuals for which at least one request is made
##' @return invisible data.frame
##' @author Timothee Flutre
##' @seealso \code{\link{makeExampleDataFile}}
##' @export
readCheckBreedDataFile <- function(f = NULL, df = NULL, max.nb.plots = 300, subset.snps,
                                   max.nb.inds = 1000) {
  stopifnot(
    !is.null(f) || !is.null(df),
    is.numeric(max.nb.plots),
    length(max.nb.plots) == 1,
    max.nb.plots > 0,
    is.list(subset.snps),
    all(names(subset.snps) %in% c("ld", "hd"))
  )

  if (is.null(df)) {
    stopifnot(file.exists(f))
    df <- utils::read.table(f,
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE
    )
  }

  if (!is.data.frame(df)) {
    "df must be a data.frame, or your file can't be read."
  }
  if (ncol(df) < 3) {
    stop("Your file has less than 3 columns, please use tabulation separator or check the encoding of your file. (it must be \"UTF8 sans BOM\")")
  }
  if (!all(c("ind", "task", "details") %in% colnames(df))) {
    stop(paste0("Your file must contain \"ind\", \"task\", and \"details\" columns. \n Your file's columns are: \"",
      paste(colnames(df), collapse = "\", \""),
      "\". (Be sure your are using \"UTF8 sans BOM\" encoding)",
      collapse = ""
    ))
  }
  if (!all(!is.na(df$ind))) {
    stop("All individuals ids must be specified.")
  }
  if (length(unique(df$ind)) > max.nb.inds) {
    stop(paste0(
      "Total number of individuals must not exceed ", max.nb.inds, " your request contains ", length(unique(df$ind)),
      " individuals."
    ))
  }
  if (!all(!grepl("[^[:alnum:]._-]", df$ind))) {
    stop("Some individuals's ids are not good.")
  }

  if (any(is.na(df$task))) {
    stop("There are missing values in \"task\" column")
  }
  if (any(!df$task %in% c("pheno-field", "pheno-patho", "geno"))) {
    id <- which(!df$task %in% c("pheno-field", "pheno-patho", "geno"))[1]
    stop(
      paste0("Requested tasks must be one of : \"pheno-field\", \"pheno-patho\", \"geno\".\n"),
      "First line with wrong value is line ", id, ": \"", df$task[id], "\".\n",
      "Please be carefull to invisible characters (like spaces for example)."
    )
  }

  if ("pheno-field" %in% df$task) {
    tmp <- suppressWarnings(as.numeric(df$details[df$task == "pheno-field"]))
    if (!all(!is.na(tmp))) {
      stop("There is some \"NA\" in \"details\" column.")
    }
    if (anyDuplicated(df$ind[df$task == "pheno-field"])) {
      stop("There is some repetition in \"ind\" column.")
    }
    if (sum(as.numeric(df$details[df$task == "pheno-field"])) > max.nb.plots) {
      stop(paste0("Your request exeeds the maximal number of plot (max ", max.nb.plots, " plots)"))
    }
  }

  if ("pheno-patho" %in% df$task) {
    if (anyDuplicated(df$ind[df$task == "pheno-patho"])) {
      stop("There is some repetition in \"ind\" column.")
    }
  }

  if ("geno" %in% df$task) {
    if (!all(grepl("hd|ld|snp", df$details[df$task == "geno"]))) {
      stop("\"details\" column must contain \"hd\", \"ld\", or \"snp00000\"")
    }
    idx.notsnp <- df$task == "geno" & !grepl("snp", df$details)
    if (anyDuplicated(df$ind[idx.notsnp])) {
      stop("There is some repetition in \"ind\" column.")
    }
    if (!all(df$details[df$task == "geno" & !df$details %in% c("ld", "hd")] %in% subset.snps[["hd"]])) {
      stop("Some requested SNPs don't exist.")
    }
  }

  invisible(df)
}

##' Count types
##'
##' Count the types of breeding requests.
##' @param df data.frame from \code{\link{readCheckBreedPlantFile}} or \code{\link{readCheckBreedDataFile}}
##' @return named vector
##' @author Timothee Flutre
##' @export
countRequestedBreedTypes <- function(df) {
  stopifnot(
    is.data.frame(df),
    !is.null(colnames(df))
  )

  types <- NULL

  if ("parent1" %in% colnames(df)) {
    types <- stats::setNames(
      c(
        sum(df$parent1 != df$parent2 &
          !is.na(df$parent2)),
        sum(df$parent1 == df$parent2 &
          !is.na(df$parent2)),
        sum(is.na(df$parent2))
      ),
      c(
        "allofecundation",
        "autofecundation",
        "haplodiploidization"
      )
    )
  } else if ("ind" %in% colnames(df)) {
    types <- stats::setNames(
      c(
        sum(as.numeric(df$details[df$task ==
          "pheno-field"])),
        sum(as.numeric(df$details[df$task ==
          "pheno-patho"])),
        sum(df$task == "geno" &
          df$details == "hd"),
        sum(df$task == "geno" &
          df$details == "ld"),
        sum(df$task == "geno" &
          !df$details %in% c("hd", "ld"))
      ),
      c(
        "pheno-field",
        "pheno-patho",
        "geno-hd",
        "geno-ld",
        "geno-single-snp"
      )
    )
  }

  return(types)
}


indAvailable <- function(indList, gameTime, breeder) {
  # function to check if individuals are available
  # indList (character vector), list of individuals to check
  # gameTime ("POSIXlt") (given by getGameTime function)
  # breeder (character) breeder name

  ## 1. check that the requested individuals exist
  all_breeder_inds <- getBreedersIndividuals(breeder)

  indExist <- all(indList %in% all_breeder_inds$child)

  ## 2. check available date
  indSQLlist <- paste0("('", paste(indList, collapse = "','"), "')")

  ## 3. get requested individuals information
  tbl <- paste0("plant_material_", breeder)
  query <- paste0("SELECT child, avail_from FROM ", tbl, " WHERE child IN ", indSQLlist)
  res <- db_get_request(query)

  # compare dates
  funApply <- function(x) {
    difftime(gameTime, strptime(x, format = "%Y-%m-%d %H:%M:%S")) >= 0
  }
  indGrown <- all(sapply(res$avail_from, FUN = funApply))

  return(list("indExist" = indExist, "indGrown" = indGrown))
}


## value box modified
valueBoxServer <- function(value, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL) {
  shinydashboard:::validateColor(color)
  if (!is.null(icon)) {
    shinydashboard:::tagAssert(icon, type = "i")
  }
  boxContent <- div(
    class = paste0("small-box ", "serverIndicator "),
    div(class = "inner", style = "color: #fff", h3(value), p(subtitle)), if (!is.null(icon)) {
      div(class = "icon-large", icon)
    }
  )
  if (!is.null(href)) {
    boxContent <- a(href = href, boxContent)
  }
  div(class = if (!is.null(width)) {
    paste0("col-sm-", width)
  }, boxContent)
}


writeRequest <- function(df, breeder, fileName = NULL) {
  # detect type of request:
  if (all(c("parent1", "parent2", "child") %in% colnames(df))) {
    reqType <- "pltMat"
  } else if (any(df$task == "geno")) {
    reqType <- "geno"
  } else {
    reqType <- "pheno"
  }


  # fileName
  fileName <- strsplit(fileName, split = "[.]")[[1]][1] # delete extention
  fout <- paste0(DATA_SHARED, "/", breeder, "/", "Request-", reqType, "_", fileName, ".txt")
  n <- 0
  while (file.exists(fout)) {
    n <- n + 1
    fout <- paste0(DATA_SHARED, "/", breeder, "/", "Request-", reqType, "_", fileName, "_", n, ".txt")
  }

  write.table(df, file = fout, sep = "\t", row.names = FALSE, quote = FALSE)
}


create_issue_link <- function(title = "", body = ""){
  repo_base_url <- "https://github.com/timflutre/PlantBreedGame"
  body <- gsub(pattern = "\n", replacement = "%0A", x = body)
  issue_link <- paste0(
    repo_base_url,
    "/issues/new?title=",
    title,
    "&body=",
    body
  )
  return(issue_link)
}


