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
  test <- try(df <- readCheckBreedPlantFile(input$file.plmat$datapath))
  if (is.data.frame(test)){
    indList <- unique(c(as.character(df$parent1),as.character(df$parent2)))
    indList <- indList[!is.na(indList)]
    indAvail <- indAvailable(indList, getGameTime(setup), breeder())
    if ((indAvail$indGrown | breederStatus()=="game master") & indAvail$indExist){# check if individuals are available
      return(df)
    }else {return("error - Individuals not availables")}
  }else {return("error")}

})



# check
output$plmatUploaded <- renderPrint({
  
  if (is.data.frame(readQryPlmat())){
    print("GOOD")
  } else if (is.null(readQryPlmat())){
    print("No file uploaded")
  } else print(readQryPlmat())
  
  
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
    create_plant_material(breeder(), readQryPlmat(), getGameTime(setup))
    reset("file.plmat")
    return("Plant material are growing up !")
  }
  return("Please check your file")
})



output$outPlmat <- renderPrint({

    plantMatRequested()
})



## Breeder information :
output$breederBoxPltMat <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = "-",
    icon = icon("user-o"),
    color = "yellow",
    width = 4
  )
})

output$dateBoxPltMat <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format= "%Y-%m-%d"),
    icon = icon("calendar"),
    color = "yellow",
    width = 4
  )
})



output$budgetBoxPltMat <- renderValueBox({
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow",
    width = 4
  )
})

output$UIbreederInfoPltMat <- renderUI({
  if (breeder()!="No Identification"){
    list(infoBoxOutput("breederBoxPltMat"),
         infoBoxOutput("dateBoxPltMat"),
         infoBoxOutput("budgetBoxPltMat"))
  }
  
})





## DEBUG
# 
output$plmatDebug <- renderPrint({
  print("-----")
  if(is.null(input$file.plmat))
    return(NULL)
  test <- try(df <- readCheckBreedPlantFile(input$file.plmat$datapath))
  if (is.data.frame(test)){
    indList <- unique(c(as.character(df$parent1),as.character(df$parent2)))
    indList <- indList[!is.na(indList)]
  }
  print(df)
  print(indList)
  
})


