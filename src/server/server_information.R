## Copyright 2015,2016,2017,2018,2019 Institut National de la Recherche Agronomique
## and Montpellier SupAgro.
##
## This file is part of PlantBreedGame.
##
## PlantBreedGame is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## PlantBreedGame is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with PlantBreedGame.  If not, see
## <http://www.gnu.org/licenses/>.


## server for information part



output$dwnlIniData <- downloadHandler(
  filename = function() input$iniDataFile, # lambda function
  content = function(file) {
    filePath <- file.path(DATA_INITIAL_DATA, input$iniDataFile)
    file.copy(filePath, file)
  }
)




## Debug
output$infoDebug <- renderPrint({
  print("----")
  print(input$iniDataFile)
  print(class(input$iniDataFile))
})
