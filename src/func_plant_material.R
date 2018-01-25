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


## Contain functions used in "plant material" section.

create_plant_material <- function (breeder, crosses.todo, gameTime){
  # function which create the new generations (see game_master_plant-material.R)
  
  # breeder (character) name of the breeder
  # crosses.todo (data frame) output of "readCheckBreedPlantFile"
  # gameTime ("POSIXlt") of the request (given by getGameTime function)
  
  
  ## Initialisation
  stopifnot(breeder %in% setup$breeders)
  stopifnot(! is.null(crosses.todo))
  cross.types <- countRequestedBreedTypes(crosses.todo)
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  
  year <- data.table::year(gameTime)
  
  
  ## check the presence of new individuals in the set of existing individuals
  flush.console()
  parent.ids <- unique(c(crosses.todo$parent1, crosses.todo$parent2))
  parent.ids <- parent.ids[! is.na(parent.ids)]
  child.ids <- crosses.todo$child
  tbl <- paste0("plant_material_", breeder)
  stopifnot(tbl %in% dbListTables(db))
  query <- paste0("SELECT child FROM ", tbl)
  res <- dbGetQuery(conn=db, query)
  stopifnot(all(parent.ids %in% res$child))
  stopifnot(all(! child.ids %in% res$child))
  
  
  ## load the haplotypes of all parents
  flush.console()
  parents <- list(haplos=list())
  for(parent.id in parent.ids){
    if("ind" %in% ls())
      rm(ind)
    f <- paste0(setup$truth.dir, "/", breeder, "/", parent.id, "_haplos.RData")
    if(! file.exists(f))
      stop(paste0(f, " doesn't exist"))
    load(f)
    if(length(parents$haplos) == 0){ # first to insert
      parents$haplos <- ind$haplos
    } else{
      for(chr.id in names(parents$haplos))
        parents$haplos[[chr.id]] <- rbind(parents$haplos[[chr.id]],
                                          ind$haplos[[chr.id]])
    }
  }
  stopifnot(sapply(parents$haplos, nrow) / 2 == length(parent.ids))
  
  
  ## perform the requested crosses
  flush.console()
  new.inds <- list()
  loc.crossovers <- drawLocCrossovers(crosses=crosses.todo,
                                      nb.snps=sapply(parents$haplos, ncol))
  new.inds$haplos <- makeCrosses(haplos=parents$haplos,
                                 crosses=crosses.todo,
                                 loc.crossovers=loc.crossovers, verbose=2)
  
  
  ## save the haplotypes of the new individuals
  flush.console()
  for(new.ind.id in getIndNamesFromHaplos(new.inds$haplos)){
    message(new.ind.id)
    ind <- list(haplos=getHaplosInd(new.inds$haplos, new.ind.id))
    f <- paste0(setup$truth.dir, "/", breeder, "/", new.ind.id, "_haplos.RData")
    save(ind, file=f)
  }
  
  
  
  
  
  ## insert the requested crosses into their table
  flush.console()
  nrow(res <- dbGetQuery(db, paste0("SELECT * FROM ", tbl)))
  for(i in 1:nrow(crosses.todo)){
    ## calculate the available date:
    if (crosses.todo$explanations[i]=="allofecundation"){
      availableDate <- seq(from=gameTime, by=paste0(constants$duration.allof, " month"), length.out=2)[2]
    }else if (crosses.todo$explanations[i]=="autofecundation"){
      availableDate <- seq(from=gameTime, by=paste0(constants$duration.autof, " month"), length.out=2)[2]
    }else if (crosses.todo$explanations[i]=="haplodiploidization"){
      availableDate <- seq(from=gameTime, by=paste0(constants$duration.haplodiplo, " month"), length.out=2)[2]
    }
    
    
    message(paste0(i, "/", nrow(crosses.todo)))
    query <- paste0("INSERT INTO ", tbl, " VALUES",
                    "('", crosses.todo$parent1[i],
                    "', '", crosses.todo$parent2[i],
                    "', '", crosses.todo$child[i],
                    "', '", availableDate, "')")
    res <- dbGetQuery(conn=db, query)
  }
  nrow(res <- dbGetQuery(db, paste0("SELECT * FROM ", tbl)))
  
  
  
  ## log
  flush.console()
  for(type in names(cross.types)){
    if(cross.types[type] > 0){
      query <- paste0("INSERT INTO log(breeder,year,task,quantity)",
                      " VALUES ('", breeder,
                      "', '", year,
                      "', '", type,
                      "', '", cross.types[type], "')")
      res <- dbGetQuery(db, query)
    }
  }
  
  # disconnect db
  dbDisconnect(db)
  
  return("Done")

}


