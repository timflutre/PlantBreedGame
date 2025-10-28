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




phenotype4Eval <- function(nRep = 50) {
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


  ## 3. load the haplotypes and convert to genotypes
  evaluated_inds <- rbind(
    db_get_individual(control = 1),
    db_get_individual(selected_for_eval = 1)
  )
  X <- load_genotypes(evaluated_inds$id, add_breeder_to_inds_names = TRUE)


  ## 4.1 handle the 'pheno-field' tasks for the requested individuals
  if (nrow(X) > 0) {
    phenosField.df <- makeDfPhenos(
      ind.ids = rownames(X),
      nb.plots = rep(nRep, length(rownames(X))),
      year = 2015,
      pathogen = TRUE
    )
    phenosField.df$ind <- as.factor(phenosField.df$ind)

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

  # output
  return(phenosField.df)
}



## allelic frequency
getAFs <- function(inds_ids, progressAFS = NULL) {
  X <- load_genotypes(inds_ids = inds_ids, UIprogress = progressAFS)
  return(estimSnpAf(X = X))
}


#' calcAdditiveRelation
#' @description Get the additive relationship between individuals
#' @param breeder breeder's name as a character string.
#' @param progressBar (optional) a \code{shiny} progress bar
#'
calcAdditiveRelation <- function(inds_ids, progressBar = NULL) {
  X <- load_genotypes(inds_ids = inds_ids, UIprogress = progressBar)
  rutilstimflutre::estimGenRel(X = X, relationships = "additive", method = "vanraden1", verbose = 0)
}
