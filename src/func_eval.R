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




readCheckEvalFile <- function(f = NULL, df = NULL){

  stopifnot(xor(is.null(f),is.null(df)))

  if (is.null(df)) {
    stopifnot(file.exists(f))
    df <- utils::read.table(f,header = TRUE,
                            sep = "\t",
                            stringsAsFactors = FALSE)
  }

  stopifnot(is.data.frame(df),
            ncol(df) ==2,
            all(c("breeder","ind") %in% colnames(df)),
            all(!grepl("[^[:alnum:]._-]",df$ind)))

  invisible(df)
}




phenotype4Eval <- function (df, nRep=50){
  # function which evaluate the phenotype of individuals

  # df (data.frame) given by readCheckEvalFile function
  # rep (int) number of repetition for each individuals



  ## Initialisations
  data.types <- "evaluation"
  db <- dbConnect(SQLite(), dbname=setup$dbname)



  ## 0. load required data
  flush.console()
  f <- paste0(setup$truth.dir, "/p0.RData")
  load(f)
  subset.snps <- list()
  f <- paste0(setup$init.dir, "/snp_coords_hd.txt.gz")
  subset.snps[["hd"]] <- rownames(read.table(f))
  f <- paste0(setup$init.dir, "/snp_coords_ld.txt.gz")
  subset.snps[["ld"]] <- rownames(read.table(f))



  ## 2. check that the requested individuals already exist
  flush.console()
  for (breeder in unique(df$breeder)){
    if (breeder!="control"){
      tbl <- paste0("plant_material_", breeder)
      stopifnot(tbl %in% dbListTables(db))
      query <- paste0("SELECT child FROM ", tbl)
      res <- dbGetQuery(conn=db, query)
      stopifnot(all(df$ind[df$breeder==breeder] %in% res$child))
    }
  }



  ## 3. load the haplotypes and convert to genotypes
  flush.console()
  X <- NULL # TODO: allocate whole matrix at this stage

  for (breeder in unique(df$breeder)){
    inds.todo <- df$ind[df$breeder==breeder]
    for(i in 1:length(inds.todo)){
      ind.id <- inds.todo[i]
      indName <- paste0(c(breeder, ind.id),collapse="_")
      if(ind.id %in% rownames(X))
        next
      message(paste0(i, "/", length(inds.todo), " ", ind.id))

      if (breeder=="control"){
        f <- paste0(setup$truth.dir, "/", ind.id, "_haplos.RData")
      }else{
        f <- paste0(setup$truth.dir, "/", breeder, "/", ind.id, "_haplos.RData")
      }


      if(! file.exists(f))
        stop(paste0(f, " doesn't exist"))
      load(f)

      ind$genos <- segSites2allDoses(seg.sites=ind$haplos, ind.ids=ind.id)
      rownames(ind$genos) <- indName
      if(is.null(X)){
        X <- ind$genos
      } else
        X <- rbind(X, ind$genos)

    }
  }


  ## 4.1 handle the 'pheno-field' tasks for the requested individuals
  flush.console()

  nrow(X)
  if(nrow(X) > 0){
    phenosField.df <- makeDfPhenos(ind.ids=rownames(X),
                                   nb.plots=rep(nRep,length(rownames(X))),
                                   year=2015,
                                   pathogen=TRUE)

    phenosField <- simulTraits12(dat=phenosField.df,
                                 mu=p0$mu,
                                 sigma.alpha2=p0$sigma.alpha2,
                                 X=X[levels(phenosField.df$ind),,drop=FALSE],
                                 Beta=p0$Beta,
                                 sigma2=p0$sigma2,
                                 afs=p0$afs)

    phenosField$trait3 <- simulTrait3(dat=phenosField.df,
                                      X=X[levels(phenosField.df$ind),,drop=FALSE],
                                      qtn.id=p0$trait3$qtn.id,
                                      resist.genos=p0$trait3$resist.genos,
                                      prob.resist.no.qtl=0)

    phenosField.df$trait1.raw <- phenosField$Y[,1]
    phenosField.df$trait2 <- phenosField$Y[,2]
    phenosField.df$trait3 <- phenosField$trait3$y
    phenosField.df$trait1 <- phenosField.df$trait1.raw
    phenosField.df$GAT1 <- rep(phenosField$G.A[,1], each=nRep)
    phenosField.df$GAT2 <- rep(phenosField$G.A[,2], each=nRep)
    # ## write the phenotypes (all inds into the same file)
    # fout <- paste0(setup$breeder.dirs[[breeder]], "/", pre.fin,
    #                "_evaluation_", strftime(gameTime, format = "%Y-%m-%d"), ".txt.gz")
    # if(!file.exists(fout)){
    #   # stop(paste0(fout, " already exists"))
    #   write.table(x=phenosField.df[, -grep("raw", colnames(phenosField.df))],
    #               file=gzfile(fout), quote=FALSE,
    #               sep="\t", row.names=FALSE, col.names=TRUE)
    # }
  }


  ## 7. log
  query <- paste0("INSERT INTO log(breeder,request_date,task,quantity)",
                  " VALUES ('", "evaluation",
                  "', '", strftime(getGameTime(setup), format = "%Y-%m-%d %H:%M:%S"),
                  "', '", 'evaluation', "', '",
                  "1", "')")
  res <- dbExecute(db, query)
  dbDisconnect(db)

  # output
  return(phenosField.df)
}


