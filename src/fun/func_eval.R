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




readCheckEvalFile <- function(f = NULL, df = NULL) {
  stopifnot(xor(is.null(f), is.null(df)))

  if (is.null(df)) {
    stopifnot(file.exists(f))
    df <- utils::read.table(f,
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE
    )
  }

  stopifnot(
    is.data.frame(df),
    ncol(df) == 2,
    all(c("breeder", "ind") %in% colnames(df)),
    all(!grepl("[^[:alnum:]._-]", df$ind))
  )

  invisible(df)
}




phenotype4Eval <- function(df, nRep = 50) {
  # function which evaluate the phenotype of individuals

  # df (data.frame) given by readCheckEvalFile function
  # rep (int) number of repetition for each individuals



  ## Initialisations
  data.types <- "evaluation"



  ## 0. load required data
  flush.console()
  f <- paste0(DATA_TRUTH, "/p0.RData")
  load(f)
  f <- paste0(DATA_TRUTH, "/afs0.RData")
  load(f)
  subset.snps <- getSNPsubset()



  ## 2. check that the requested individuals already exist
  flush.console()
  for (breeder in unique(df$breeder)) {
    if (breeder != "control") {
      all_breeder_inds <- getBreedersIndividuals(breeder)
      stopifnot(all(df$ind[df$breeder == breeder] %in% all_breeder_inds$child))
    }
  }



  ## 3. load the haplotypes and convert to genotypes
  flush.console()
  X <- NULL # TODO: allocate whole matrix at this stage

  for (breeder in unique(df$breeder)) {
    inds.todo <- df$ind[df$breeder == breeder]
    for (i in 1:length(inds.todo)) {
      ind.id <- inds.todo[i]
      indName <- paste0(c(breeder, ind.id), collapse = "*")
      if (ind.id %in% rownames(X)) {
        next
      }
      # message(paste0(i, "/", length(inds.todo), " ", ind.id))

      if (breeder == "control") {
        f <- paste0(DATA_TRUTH, "/", ind.id, "_haplos.RData")
      } else {
        f <- paste0(DATA_TRUTH, "/", breeder, "/", ind.id, "_haplos.RData")
      }


      if (!file.exists(f)) {
        stop(paste0(f, " doesn't exist"))
      }
      load(f)

      ind$genos <- segSites2allDoses(seg.sites = ind$haplos, ind.ids = ind.id)
      rownames(ind$genos) <- indName
      if (is.null(X)) {
        X <- ind$genos
      } else {
        X <- rbind(X, ind$genos)
      }
    }
  }


  ## 4.1 handle the 'pheno-field' tasks for the requested individuals
  flush.console()

  nrow(X)
  if (nrow(X) > 0) {
    phenosField.df <- makeDfPhenos(
      ind.ids = rownames(X),
      nb.plots = rep(nRep, length(rownames(X))),
      year = 2015,
      pathogen = TRUE
    )

    phenosField <- simulTraits12(
      dat = phenosField.df,
      mu = p0$mu,
      sigma.alpha2 = c(
        trait1 = 0,
        trait2 = 0
      ),
      X = X[levels(phenosField.df$ind), , drop = FALSE],
      Beta = p0$Beta,
      sigma2 = p0$sigma2,
      afs = afs0,
      verbose = 0
    )

    phenosField$trait3 <- simulTrait3(
      dat = phenosField.df,
      X = X[levels(phenosField.df$ind), , drop = FALSE],
      qtn.id = p0$trait3$qtn.id,
      resist.genos = p0$trait3$resist.genos,
      prob.resist.no.qtl = 0,
      verbose = 0
    )

    phenosField.df$trait1.raw <- phenosField$Y[, 1]
    phenosField.df$trait2 <- phenosField$Y[, 2]
    phenosField.df$trait3 <- phenosField$trait3$y
    phenosField.df$trait1 <- phenosField.df$trait1.raw
    phenosField.df$GAT1 <- rep(phenosField$G.A[, 1], each = nRep)
    phenosField.df$GAT2 <- rep(phenosField$G.A[, 2], each = nRep)
  }


  ## 7. log
  query <- paste0(
    "INSERT INTO log(breeder,request_date,task,quantity)",
    " VALUES ('", "evaluation",
    "', '", strftime(getGameTime(), format = "%Y-%m-%d %H:%M:%S"),
    "', '", "evaluation", "', '",
    "1", "')"
  )
  db_execute_request(query)

  # output
  return(phenosField.df)
}



## allalic frequency
getAFs <- function(pop, breeder, progressAFS = NULL) {
  # pop (character verctor) names of individuals
  X <- matrix(
    nrow = length(pop),
    ncol = getBreedingGameConstants()$nb.snps
  )
  rownames(X) <- pop

  for (i in 1:length(pop)) {
    ind.id <- pop[i]
    indName <- paste0(c(breeder, ind.id), collapse = "_")

    if (!is.null(progressAFS)) {
      progressAFS$set(
        value = i / length(pop),
        detail = indName
      )
    }

    # message(paste0(i, "/", length(pop), " ", ind.id))
    f <- paste0(DATA_TRUTH, "/", breeder, "/", ind.id, "_haplos.RData")
    load(f)

    ind$genos <- segSites2allDoses(seg.sites = ind$haplos, ind.ids = ind.id)
    rownames(ind$genos) <- indName
    X[i, ] <- ind$genos
  }
  colnames(X) <- colnames(ind$genos)

  return(estimSnpAf(X = X))
}



#' getBreederHistory
#' @description Get the breeder's requests history.
#' @param breeder breeder's name as a character string.
#' @param setup game's setup.
#'
#' @return \code{data.frame} with all breeder's request.
#' @export
getBreederHistory <- function(breeder, setup) {
  # get data
  query <- paste0("SELECT * FROM log WHERE breeder=\'", breeder, "\'")
  res <- db_get_request(query)

  # manage variable class
  res$task <- as.factor(res$task)
  res$request_date <- as.Date(res$request_date)

  res[, c("breeder", "task", "quantity", "request_date")]
}





#' calcAdditiveRelation
#' @description Get the additive relationship between individuals
#' @param breeder breeder's name as a character string.
#' @param query \code{data.frame} containing individuals list
#' (\code{ind} column is required)
#' @param setup game's setup.
#' @param progressBar (optional) a \code{shiny} progress bar
#'
#' @return
#' @export
#'
#' @examples
calcAdditiveRelation <- function(breeder, query, setup, progressBar = NULL) {
  query <- query[query$breeder == breeder, ]
  ## 1. load the haplotypes and convert to genotypes
  X <- matrix(
    nrow = length(unique(query$ind)),
    ncol = getBreedingGameConstants()$nb.snps
  )

  for (i in 1:length(unique(query$ind))) {
    ind.id <- unique(query$ind)[i]

    if (!is.null(progressBar)) {
      progressBar$inc(
        amount = 1 / length(unique(query$ind)),
        detail = paste0("Load haplotypes: ", paste0(i, "/", nrow(query), " ", ind.id))
      )
    }

    f <- paste0(DATA_TRUTH, "/", breeder, "/", ind.id, "_haplos.RData")
    if (!file.exists(f)) {
      stop(paste0(f, " doesn't exist"))
    }
    load(f)

    ind$genos <- segSites2allDoses(seg.sites = ind$haplos, ind.ids = ind.id)
    X[i, ] <- ind$genos
  }
  rownames(X) <- unique(query$ind)
  colnames(X) <- colnames(ind$genos)

  rutilstimflutre::estimGenRel(X = X, relationships = "additive", method = "vanraden1", verbose = 0)
}
