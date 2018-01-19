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


## server for information part


## NOT OPERATIONAL #############
output$dwnlIniData <- downloadHandler(
  filename=function () input$iniDataFile, # lambda function
  content=function(file){
    filePath <- paste0("data/shared/initial_data/",input$iniDataFile)
    # get the extention of the file:
    ext <- rev(strsplit(input$iniDataFile, split=".", fixed = TRUE)[[1]])[1]

    if (ext=="gz"){
      write.table(read.table(filePath, header = T),
                  file=gzfile(file), quote=FALSE,
                  sep="\t", row.names=FALSE, col.names=TRUE)
    }else {
      write.table(read_file(filePath, header = T),
                  file=file, quote=FALSE,
                  sep="\t", row.names=FALSE, col.names=TRUE)
    }
  }
)








