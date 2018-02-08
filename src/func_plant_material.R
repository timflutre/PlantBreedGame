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

create_plant_material <- function (breeder, crosses.todo, gameTime, progressPltMat=NULL){
  
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
    if (!is.null(progressPltMat)){
      progressPltMat$set(value = 1,
                         detail = paste0("Load haplotypes: ",parent.id))
    }
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
  if (!is.null(progressPltMat)){
    progressPltMat$set(value = 2,
                       detail = "perform crosses...")
  }
  
  
  flush.console()
  new.inds <- list()
  loc.crossovers <- drawLocCrossovers(crosses=crosses.todo,
                                      nb.snps=sapply(parents$haplos, ncol))
  new.inds$haplos <- makeCrosses(haplos=parents$haplos,
                                 crosses=crosses.todo,
                                 loc.crossovers=loc.crossovers, verbose=0)
  
  
  ## save the haplotypes of the new individuals
  flush.console()
  for(new.ind.id in getIndNamesFromHaplos(new.inds$haplos)){
    if (!is.null(progressPltMat)){
      progressPltMat$set(value = 3,
                         detail = paste0("Save haplotypes: ", new.ind.id))
    }
    message(new.ind.id)
    ind <- list(haplos=getHaplosInd(new.inds$haplos, new.ind.id))
    f <- paste0(setup$truth.dir, "/", breeder, "/", new.ind.id, "_haplos.RData")
    save(ind, file=f)
  }
  

  
  ## insert the requested crosses into their table
  flush.console()
  nrow(res <- dbGetQuery(db, paste0("SELECT * FROM ", tbl)))
  
  getAvailDate <- function(type){
    if (type=="allofecundation"){
      availableDate <- seq(from=gameTime, by=paste0(constants$duration.allof, " month"), length.out=2)[2]
    }else if (type=="autofecundation"){
      availableDate <- seq(from=gameTime, by=paste0(constants$duration.autof, " month"), length.out=2)[2]
    }else if (type=="haplodiploidization"){
      availableDate <- seq(from=gameTime, by=paste0(constants$duration.haplodiplo, " month"), length.out=2)[2]
    }
    return(strftime(availableDate, format = "%Y-%m-%d %H:%M:%S"))
  }
  crosses.todo$availableDate <- sapply(crosses.todo$explanations, FUN=getAvailDate)
  crosses.todo
  
  
  query <- paste((crosses.todo$parent1),
                 (crosses.todo$parent2),
                 (crosses.todo$child),
                 (crosses.todo$availableDate),
                 sep="','",collapse = "'),('")
  query <- paste0("INSERT INTO ", tbl, " (parent1, parent2, child, avail_from) VALUES ('",query,"')")
  res <- dbGetQuery(conn=db, query)

 
  ## log
  flush.console()
  for(type in names(cross.types)){
    if(cross.types[type] > 0){
      query <- paste0("INSERT INTO log(breeder,request_date,task,quantity)",
                      " VALUES ('", breeder,
                      "', '", strftime(gameTime, format = "%Y-%m-%d %H:%M:%S"),
                      "', '", type,
                      "', '", cross.types[type], "')")
      res <- dbGetQuery(db, query)
    }
  }
  dbDisconnect(db)
  

  
  return("done")

}






createInvoicePltmat <- function(request.df){
  # function which create the corresponding invoice of a request
  # request.df (data.frame) of the request
  
  
  # aggregate by explanations
  invoice.pltmat<- aggregate(rep(1,nrow(request.df)) ~ explanations, data = request.df, sum)
  names(invoice.pltmat) <- c("Task","Quantity")
  
  
  # get prices
  invoice.pltmat$unitaryPrice <- as.vector(as.numeric(prices[invoice.pltmat$Task]))
  invoice.pltmat$Total <- invoice.pltmat$unitaryPrice*invoice.pltmat$Quantity
  
  
  
  ## create invoice:
  invoice <- rbind(invoice.pltmat,
                   data.frame(Task="Total",
                              Quantity="",
                              unitaryPrice="",
                              Total= sum(invoice.pltmat$Total)))
  
  return(invoice)
}



indExist <- function(indList, breeder){
  # function to check if an individuals already exist
  # indList (character verctor), list of individuals to check
  # breeder (charracter) breeder name

  # get requested individuals information
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  tbl <- paste0("plant_material_", breeder)
  stopifnot(tbl %in% dbListTables(db))
  query <- paste0("SELECT child FROM ", tbl)
  res <- dbGetQuery(conn=db, query)
  dbDisconnect(db)
  
  return(any(indList %in% res$child))
  
}




