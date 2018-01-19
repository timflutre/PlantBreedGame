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


## Contain functions used in "genotyping" section.


genotype <- function (breeder, inds.todo, gameTime){
  # function which genotype the requested individuals (see game_master_pheno-geno.R)
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
    
    
    ind$genos <- segSites2allDoses(seg.sites=ind$haplos, ind.ids=ind.id)
    
    if(is.null(X)){
      X <- ind$genos
    } else
      X <- rbind(X, ind$genos)
  }
  
  
  
  ## 5. handle the 'geno' tasks for the requested individuals
  flush.console()
  df.geno.ld <- NULL
  df.geno.hd <- NULL 
  for(dty in c("ld", "hd")){
    idx <- which(inds.todo$task == "geno" & inds.todo$details == dty &
                   inds.todo$ind %in% rownames(X))
    message(paste0(dty, ": ", length(idx)))
    if(length(idx) > 0){
      
      ## write the genotypes (all inds into the same file)
      fout <- paste0(setup$breeder.dirs[[breeder]], "/", pre.fin,
                     "_genos-", dty, "-", strftime(gameTime, format = "%Y-%m-%d"),".txt.gz")
      if(!file.exists(fout)){
        write.table(x=X[inds.todo$ind[idx], subset.snps[[dty]], drop=FALSE],
                    file=gzfile(fout), quote=FALSE,
                    sep="\t", row.names=TRUE, col.names=TRUE)
      }
      
      ## save data.frame
      if (dty == "ld"){
        df.geno.ld <- as.data.frame(X[inds.todo$ind[idx], subset.snps[[dty]], drop=FALSE])
      } else {df.geno.hd <- as.data.frame(X[inds.todo$ind[idx], subset.snps[[dty]], drop=FALSE])}
      
    }
  }
  
  
  
  
  
  ## 6. handle the 'snp' tasks for the requested individuals
  flush.console()
  idx <- which(inds.todo$task == "geno" & ! inds.todo$details %in% c("ld","hd"))
  length(idx)
  if(length(idx) > 0){
    all.genos <- data.frame(ind=inds.todo$ind[idx],
                            snp=inds.todo$details[idx],
                            geno=NA,
                            stringsAsFactors=FALSE)
    for(i in idx){
      ind.id <- inds.todo$ind[i]
      snp <- inds.todo$details[i]
      all.genos$geno[all.genos$ind == ind.id &
                       all.genos$snp == snp] <- X[ind.id, snp]
    }
    
    ## write the genotypes (all inds into the same file)
    fout <- paste0(setup$breeder.dirs[[breeder]], "/", pre.fin,
                   "-single-snps", "-", strftime(gameTime, format = "%Y-%m-%d"), ".txt.gz")
    if(!file.exists(fout)){
      write.table(x=all.genos,
                  file=gzfile(fout), quote=FALSE,
                  sep="\t", row.names=FALSE, col.names=TRUE)
    }
  }else {all.genos <- NULL}
  
  
  
  ## 7. log
  year <- year(gameTime)
  
  flush.console()
  for(type in names(data.types)){
    if((type=="geno-hd" || type=="geno-ld" || type=="geno-single-snp") && data.types[type] > 0){
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
  geno_data <- list()
  geno_data$filename.ld <- paste0(pre.fin,"_genos-ld", strftime(gameTime), ".txt.gz")
  geno_data$filename.hd <- paste0(pre.fin,"_genos-hd", strftime(gameTime), ".txt.gz")
  geno_data$filename.snp <- paste0(pre.fin,"_single-snps-", strftime(gameTime), ".txt.gz")
  
  geno_data$df.ld <- df.geno.ld
  geno_data$df.hd <- df.geno.hd
  geno_data$df.snp <- all.genos
  return(geno_data)
}