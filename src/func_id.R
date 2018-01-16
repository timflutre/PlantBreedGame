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
  
  matchId <- as.logical(lapply(dataFile, FUN=grepl, pattern="pheno"))
  
  if (type =="pheno"){
    matchId <- which(matchId)
  }else matchId <- which(!matchId) # type =="geno"
  
  return(as.list(dataFile[matchId]))

  
  
  
  
  
  
}