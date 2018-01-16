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



## Contain functions used in "phenotyping" section.

phenotype <- function (breeder, inds.todo, gameTime){
  # function which phenotype the requested individuals (see game_master_pheno-geno.R)
  # create a result file in shared folder of the breeder

  # breeder (character) name of the breeder
  # inds.todo (data frame) output of "readCheckBreedDataFile"
  # gameTime ("POSIXlt") of the request (given by getGameTime function)



  ## Initialisations
  stopifnot(breeder %in% setup$breeders)
  pre.fin <- breeder
  data.types <- countRequestedBreedTypes(inds.todo)
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
  tbl <- paste0("plant_material_", breeder)
  stopifnot(tbl %in% dbListTables(db))
  query <- paste0("SELECT child FROM ", tbl)
  res <- dbGetQuery(conn=db, query)
  stopifnot(all(inds.todo$ind %in% res$child))



  ## 3. load the haplotypes and convert to genotypes
  flush.console()
  X <- NULL # TODO: allocate whole matrix at this stage
  for(i in 1:nrow(inds.todo)){
    ind.id <- inds.todo$ind[i]
    if(ind.id %in% rownames(X))
      next
    message(paste0(i, "/", nrow(inds.todo), " ", ind.id))

    f <- paste0(setup$truth.dir, "/", breeder, "/", ind.id, "_haplos.RData")
    if(! file.exists(f))
      stop(paste0(f, " doesn't exist"))
    load(f)

    # ind$genos <- segSites2allDoses(seg.sites=ind$haplos, ind.ids=ind.id,
    #                                rnd.choice.ref.all=FALSE)
    ind$genos <- segSites2allDoses(seg.sites=ind$haplos, ind.ids=ind.id)

    if(is.null(X)){
      X <- ind$genos
    } else
      X <- rbind(X, ind$genos)
  }



  ## 4. handle the 'pheno' tasks for the requested individuals
  flush.console()
  idx <- which(inds.todo$task == "pheno")
  length(idx)
  if(length(idx) > 0){
    phenos.df <- makeDfPhenos(ind.ids=inds.todo$ind[idx],
                              nb.plots=as.numeric(inds.todo$details[idx]),
                              year=year,
                              pathogen=ifelse((year - 2005) %% 3 == 0,
                                              TRUE, FALSE))


    phenos <- simulTraits12(dat=phenos.df,
                            mu=p0$mu,
                            sigma.alpha2=p0$sigma.alpha2,
                            X=X[levels(phenos.df$ind),,drop=FALSE],
                            Beta=p0$Beta,
                            sigma2=p0$sigma2)
    
    phenos$trait3 <- simulTrait3(dat=phenos.df,
                                 X=X[levels(phenos.df$ind),,drop=FALSE],
                                 qtn.id=p0$trait3$qtn.id,
                                 resist.genos=p0$trait3$resist.genos,
                                 prob.resist.no.qtl=p0$trait3$prob.resist.no.qtl)

    phenos.df$trait1.raw <- phenos$Y[,1]
    phenos.df$trait2 <- phenos$Y[,2]
    phenos.df$trait3 <- phenos$trait3$y
    phenos.df$trait1 <- phenos.df$trait1.raw
    tmp <- (phenos.df$pathogen & as.logical(phenos.df$trait3))
    if(any(tmp))
      phenos.df$trait1[tmp] <- (1 - p0$prop.yield.loss) * phenos.df$trait1[tmp]

    ## write the phenotypes (all inds into the same file)
    fout <- paste0(setup$breeder.dirs[[breeder]], "/", pre.fin,
                   "_phenos-", year, ".txt.gz")
    if(!file.exists(fout)){
      # stop(paste0(fout, " already exists"))
      write.table(x=phenos.df[, -grep("raw", colnames(phenos.df))],
                  file=gzfile(fout), quote=FALSE,
                  sep="\t", row.names=FALSE, col.names=TRUE)
    }
  }



  ## 7. log
  flush.console()
  for(type in names(data.types)){
    if(type=="pheno" && data.types[type] > 0){
      query <- paste0("INSERT INTO log(breeder,year,task,quantity)",
                      " VALUES ('", breeder,
                      "', '", year,
                      "', '", type, "', '",
                      data.types[type], "')")
      res <- dbGetQuery(db, query)
    }
  }

  # disconnect db
  dbDisconnect(db)

  # output
  pheno_data <- list()
  pheno_data$filename <- paste0(pre.fin,"_phenos-", year, ".txt.gz")
  pheno_data$df <- phenos.df[, -grep("raw", colnames(phenos.df))]
  return(pheno_data)
  # return("Phenotyping requested")
}
