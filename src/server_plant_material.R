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



## Function
source("src/func_plant_material.R", local=TRUE, encoding = "UTF-8")$value


## server for "plant material"

# read uploaded file
readQryPlmat <- reactive({
  if(is.null(input$file.plmat))
    return(NULL)
  # read.table(input$file.plmat$datapath, header=TRUE, sep="\t",
  #            nrows=10)
  try(readCheckBreedPlantFile(input$file.plmat$datapath))

})



# check
output$plmatUploaded <- renderPrint({
  
  if (is.data.frame(readQryPlmat())){
    print("GOOD")
  } else if (is.null(readQryPlmat())){
    print("No file uploaded")
  } else print("Not good")
  
  
})



# summary
output$plmatSmy <- renderPrint({
  if (is.data.frame(readQryPlmat())){
    dat <- readQryPlmat()
    summary(as.data.frame(apply(readQryPlmat(), MARGIN = 2, FUN = as.factor)))
  }
})

output$plmatStr <- renderPrint({
  if (is.data.frame(readQryPlmat())){
    dat <- readQryPlmat()
    str(readQryPlmat())
  }
})



# data
output$qryPlmat <- renderTable({
  if (is.data.frame(readQryPlmat())){
    readQryPlmat()
  }
  
})








# output
## no output ##

plantMatRequested <- eventReactive(input$requestPlmat, {
  if (is.data.frame(readQryPlmat())){
    create_plant_material(breeder(), readQryPlmat(), year)
    return("Plant material are growing up !")
  }
  return("Please check your file")
})



output$outPlmat <- renderPrint({

    plantMatRequested()
})





