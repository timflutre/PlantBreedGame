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


## function for the "id part"

getDataFileList <- function (type, breeder){
  # function to get the list of data file of the breeder
  # type (char) type of data (pheno or geno)
  # breeder (char) name of the breeder
  
  
  stopifnot(type =="pheno" || type =="geno")
  
  
  dirPath <- paste0("data/shared/",breeder)
  dataFile <- list.files(dirPath)
  
  ## Get the ids of the files
  matchId <- as.logical(lapply(dataFile, FUN=grepl, pattern="pheno"))
  if (type =="pheno"){
    matchId <- which(matchId)
  }else matchId <- which(!matchId) # type =="geno"
  
  return(as.list(dataFile[matchId]))

}




availToDwnld <- function(fileName, gameTime){
  # function to check if files are available to download
  # fileName (char) name of the file
  # gameTime ("POSIXlt") (given by getGameTime function)
  
  stopifnot(is.character(fileName),
            fileName!="")
  
  
  # get the date when the file was requested
  requestDate <- strptime(strsplit(fileName, split = "[_.]")[[1]][3], format = "%Y-%m-%d")
  
  # calculate the available date
  if (grepl("phenos-field", fileName)){
    maxDate <- strptime(paste0(data.table::year(requestDate), "-", constants$max.upload.pheno.field), format = "%Y-%m-%d")
    availDate <- seq(from=maxDate, by=paste0(constants$duration.pheno.field, " month"), length.out=2)[2]
    if (requestDate > maxDate){
      availDate <- seq(from=availDate, by="1 year", length.out=2)[2]
    }
    
  }else if (grepl("phenos-patho", fileName)){
    availDate <- seq(from=requestDate, by=paste0(constants$duration.pheno.patho, " month"), length.out=2)[2]
  }else if (grepl("genos-single-snps", fileName)){
    availDate <- seq(from=requestDate, by=paste0(constants$duration.geno.single, " month"), length.out=2)[2]
  }else if (grepl("genos-hd", fileName)){
    availDate <- seq(from=requestDate, by=paste0(constants$duration.geno.hd, " month"), length.out=2)[2]
  }else if (grepl("genos-ld", fileName)){
    availDate <- seq(from=requestDate, by=paste0(constants$duration.geno.ld, " month"), length.out=2)[2]
  }else(stop())
  
  # results
  res <- list()
  res$isAvailable <- availDate <= gameTime
  res$availDate <- availDate
  
  return(res)
  
}

addNewBreeder <- function(breederName, status, psw, progressNewBreeder=NULL){
  
  
  
  #### initialisation:
  initIndsHaplo <- list.files(setup$truth.dir)
  initIndsHaplo <- initIndsHaplo[grep("Coll",initIndsHaplo)]
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  

  #### test if new breeder already exist
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 1,
                           detail = "Check breeder existance")
  }
  
  tbl <- paste0("plant_material_", breederName)
  if(tbl %in% dbListTables(db)){
    stop("breeder already exist")
  }
  
  
  
  #### add breeder in the "breeders" table of database:
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 2,
                           detail = "Add breeder in \"breeders\" table")
  }
  hashed.psw <- digest(psw, "md5", serialize=FALSE)
  
  tbl <- "breeders"
  query <- paste0("INSERT INTO ", tbl, " VALUES",
                  " ('", breederName,"','", status,"','", hashed.psw, "')")
  res <- dbExecute(conn=db, query)
  
  
  
  
  #### create "plant_material_newBreeder"
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 3,
                           detail = "create \"plant_material\" table")
  }
  tbl <- paste0("plant_material_", breederName)
  query <- paste0("CREATE TABLE ", tbl,
                  " (parent1 TEXT",
                  ", parent2 TEXT",
                  ", child TEXT PRIMARY KEY",
                  ", avail_from TEXT)")
  res <- dbExecute(conn=db, query)
  
  
  
  #### fill "plant_material_newBreeder"
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 4,
                           detail = "fill \"plant_material\" table")
  }
  coll.ids <- gsub("_haplos.RData","",initIndsHaplo)
  query <- paste0("INSERT INTO ", tbl,
                  " (parent1, parent2, child, avail_from)",
                  " VALUES",
                  " ('", paste(gsub("Coll","ind", coll.ids),
                               rep(NA, length(coll.ids)),
                               coll.ids,
                               rep(paste0(constants$first.year, "-01-01 00:00:00"),
                                   length(coll.ids)),
                               sep="','",collapse = "'),('")
                  , "')")
  res <- dbExecute(conn=db, query)
  dbDisconnect(db)
  

  #### create folders of the new breeder:
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 5,
                           detail = "create truth and shared folders")
  }
  newTruthDir <- paste0(setup$truth.dir,"/",breederName)
  newSharedDir <- paste0(setup$shared.dir,"/",breederName)
  dir.create(newTruthDir)
  dir.create(newSharedDir)
  

  
  funApply <- function(fileName){
    if (!is.null(progressNewBreeder)){
      progressNewBreeder$set(value = 6,
                             detail = paste0("Create haplo symlink:",fileName))
    }
    
    fromFile <- paste0(setup$truth.dir,"/",fileName) 
    toFile <- paste0(newTruthDir,"/",fileName)
    file.symlink(fromFile,toFile) 
    return()
  }
  
  if (Sys.info()["sysname"]!="Windows"){
    sapply(initIndsHaplo, FUN = funApply)
  }else {
    
    # copy files
    funApply <- function(fileName){
      if (!is.null(progressNewBreeder)){
        progressNewBreeder$set(value = 6,
                               detail = paste0("Create haplo copy:",fileName))
      }
      fromFile <- paste0(setup$truth.dir,"/",fileName) 
      toFile <- paste0(newTruthDir,"/",fileName)
      file.copy(fromFile,toFile) # copy
      return()
    }
    sapply(initIndsHaplo, FUN = funApply)
    
  }
    
}
  
  
  
  
  
  

  










