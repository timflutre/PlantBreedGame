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
  
  
  stopifnot(type =="pheno" || type =="geno"
            || type =="pltMat" || type =="request")
  
  
  dirPath <- paste0("data/shared/",breeder)
  dataFile <- list.files(dirPath)
  
  ## Get the ids of the files
  matchId <- as.logical(lapply(dataFile, FUN=grepl, pattern="Result_pheno"))
  if (type =="pheno"){
    matchId <- which(matchId)
  }else if (type =="geno") {
    matchId <- matchId <- as.logical(lapply(dataFile, FUN=grepl, pattern="Result_geno"))
  }else if (type =="pltMat"){
    matchId <- matchId <- as.logical(lapply(dataFile, FUN=grepl, pattern="IndList_"))
  }else{
    matchId <- matchId <- as.logical(lapply(dataFile, FUN=grepl, pattern="Request"))
  }
  
  
  return(as.list(dataFile[matchId]))

}




availToDwnld <- function(fileName, gameTime){
  # function to check if files are available to download
  # fileName (char) name of the file
  # gameTime ("POSIXlt") (given by getGameTime function)
  
  stopifnot(is.character(fileName),
            fileName!="")
  

  # get the date when the file was requested
  m <- regexpr("[0-9]{4}[-][0-9]{2}[-][0-9]{2}", fileName)
  requestDate <- strptime(regmatches(fileName, m), format = "%Y-%m-%d")
  

  
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


  

  










