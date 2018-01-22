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
makeDfPhenos <- function(ind.ids, nb.plots.per.ind, year, pathogen){
  stopifnot(is.character(ind.ids),
            is.numeric(nb.plots.per.ind),
            length(nb.plots.per.ind) == length(ind.ids),
            is.numeric(year),
            is.logical(pathogen))

  tmp <- data.frame(ind.id=ind.ids, nb.plots=nb.plots.per.ind,
                    stringsAsFactors=FALSE)
  tmp <- tmp[order(tmp$ind.id),]

  df <- data.frame(ind=rep(tmp$ind.id, tmp$nb.plots),
                   year=as.factor(rep(year, sum(tmp$nb.plots))),
                   plot=as.factor(1:sum(tmp$nb.plots)),
                   pathogen=pathogen,
                   trait1.raw=NA,
                   trait1=NA,
                   trait2=NA,
                   trait3=NA)

  return(df)
}

##' Read for breeding game
##'
##' Read and check a file supposed to contain requests about plant material.
##' It should have 3 columns named \code{parent1}, \code{parent2} and \code{child}.
##' @param f path to the input file (columns should be separated by a tabulation)
##' @param df data.frame (if the file was already read)
##' @param max.nb.hd maximum number of haplodiploidization to request in a single file
##' @return invisible data.frame
##' @author Timothee Flutre
##' @seealso \code{\link{makeExamplePlantFile}}
##' @export
readCheckBreedPlantFile <- function(f=NULL, df=NULL, max.nb.hd=800){
  stopifnot(! is.null(f) || ! is.null(df),
            is.numeric(max.nb.hd),
            length(max.nb.hd) == 1,
            max.nb.hd > 0)

  if(is.null(df)){
    stopifnot(file.exists(f))
    df <- utils::read.table(f, header=TRUE, sep="\t", stringsAsFactors=FALSE)
  }

  stopifnot(is.data.frame(df),
            ncol(df) >= 3,
            all(c("parent1", "parent2", "child") %in% colnames(df)),
            all(! is.na(df$parent1)),
            ! any(df$parent1 == ""),
            all(! is.na(df$child)),
            ! any(df$child == ""),
            anyDuplicated(df$child) == 0,
            all(! grepl("[^[:alnum:]._-]", df$child)),
            sum(is.na(df$parent2)) <= max.nb.hd)

  df$parent2[df$parent2 == ""] <- NA

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
readCheckBreedDataFile <- function (f = NULL, df = NULL, max.nb.plots = 300, subset.snps,
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
            all(df$task %in% c("pheno-field", "pheno-patho", "geno")))
  

  if ("pheno-field" %in% df$task) {
    tmp <- suppressWarnings(as.numeric(df$details[df$task == "pheno-field"]))
    stopifnot(all(!is.na(tmp)),
              !anyDuplicated(df$ind[df$task == "pheno-field"]),
              sum(as.numeric(df$details[df$task == "pheno-field"])) <= max.nb.plots)
  }
  
  if ("pheno-patho" %in% df$task) {
    stopifnot(!anyDuplicated(df$ind[df$task == "pheno-patho"]))
  }
  
  if ("geno" %in% df$task) {
    stopifnot(all(grepl("hd|ld|snp", df$details[df$task == "geno"])))
    idx.notsnp <- df$task == "geno" & !grepl("snp", df$details)
    stopifnot(!anyDuplicated(df$ind[idx.notsnp]),
              all(df$details[df$task == "geno" & !df$details %in% c("ld", "hd")] %in% subset.snps[["hd"]]))
  }
  
  ## check that the requested individuals already exist
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

##' Count types
##'
##' Count the types of breeding requests.
##' @param df data.frame from \code{\link{readCheckBreedPlantFile}} or \code{\link{readCheckBreedDataFile}}
##' @return named vector
##' @author Timothee Flutre
##' @export
countRequestedBreedTypes <- function(df){
  stopifnot(is.data.frame(df),
            ! is.null(colnames(df)))

  types <- NULL

  if("parent1" %in% colnames(df)){
    types <- stats::setNames(c(sum(df$parent1 != df$parent2 &
                            ! is.na(df$parent2)),
                        sum(df$parent1 == df$parent2 &
                            ! is.na(df$parent2)),
                        sum(is.na(df$parent2))),
                      c("allofecundation", "autofecundation",
                        "haplodiploidization"))
  } else if("ind" %in% colnames(df)){
    # types <- stats::setNames(c(ifelse("pheno" %in% df$task,
    #                            sum(as.numeric(df$details[df$task ==
    #                                                             "pheno"])),
    #                            0),
    #                     sum(df$task == "geno" &
    #                         df$details == "hd"),
    #                     sum(df$task == "geno" &
    #                         df$details == "ld"),
    #                     sum(df$task == "geno" &
    #                         ! df$details %in% c("hd", "ld"))),
    #                   c("pheno", "geno-hd", "geno-ld", "geno-single-snp"))
    types <- stats::setNames(c(sum(df$task == "pheno-field"),
                               sum(df$task == "pheno-patho"),
                               sum(df$task == "geno" &
                                     df$details == "hd"),
                               sum(df$task == "geno" &
                                     df$details == "ld"),
                               sum(df$task == "geno" &
                                     ! df$details %in% c("hd", "ld"))),
                             c("pheno-field", "pheno-patho", "geno-hd", "geno-ld", "geno-single-snp"))
  }

  return(types)
}
