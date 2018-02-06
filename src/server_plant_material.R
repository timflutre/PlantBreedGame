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

  # no input fileI
  if(is.null(input$file.plmat)){
    return(NULL)
  }else if (breeder()=="No Identification"){
    return("error - You are not connected")
  }


  # read input file
  maxHD <- ifelse(breederStatus()=="game master",Inf,constants$max.nb.haplodiplos)
  test <- try(df <- readCheckBreedPlantFile(input$file.plmat$datapath, max.nb.hd=maxHD))

  if (is.data.frame(test)){
    # the file is ok

    # list individuals
    indList <- unique(c(as.character(df$parent1),as.character(df$parent2)))
    indList <- indList[!is.na(indList)]
    indAvail <- indAvailable(indList, getGameTime(setup), breeder())

    # check if individuals are available
    if ((indAvail$indGrown | breederStatus()=="game master")
        & indAvail$indExist){
      return(df)
    }else {return("error - Individuals not availables")}
  }else {return("error - wrong file format")}

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
output$PltmatInvoice <- renderTable({
  if (is.data.frame(readQryPlmat())){
    createInvoicePltmat(readQryPlmat())
  }
})



# output$plmatSmy <- renderPrint({
#   if (is.data.frame(readQryPlmat())){
#     dat <- readQryPlmat()
#     summary(as.data.frame(apply(readQryPlmat(), MARGIN = 2, FUN = as.factor)))
#   }
# })
# 
# output$plmatStr <- renderPrint({
#   if (is.data.frame(readQryPlmat())){
#     dat <- readQryPlmat()
#     str(readQryPlmat())
#   }
# })



# data
output$qryPlmat <- renderDataTable({
  if (is.data.frame(readQryPlmat())){
    readQryPlmat()
  }

})




# output
plantMatRequested <- eventReactive(input$requestPlmat,{
  if (is.data.frame(readQryPlmat())){
    
    # Create a Progress object
    progressPltMat <- shiny::Progress$new(session, min=0, max=5)
    progressPltMat$set(value = 0,
                       message = "Create Plant Material",
                       detail = "Initialisation...")
    
    
    res <- try(create_plant_material(breeder(),
                                     readQryPlmat(),
                                     getGameTime(setup),
                                     progressPltMat))

    if (res=="done"){
      progressPltMat$set(value = 5,
                         detail = "Done !")

      return(res)
    }else{
      progressPltMat$set(detail = "ERROR !")
      return("error")
    }

  }else return(NULL)
})


output$plmatRequestResultUI <- renderUI({
  if (!is.null(plantMatRequested()) && plantMatRequested()=="done"){
    # reset inputs
    reset("file.plmat")
    session$sendCustomMessage(type = "resetValue", message = "file.plmat")
    
    # display message
    
    p("\n Great ! Your plants are growing up !")
  } else if (!is.null(plantMatRequested()) && plantMatRequested()=="error"){
    p("\n Something went wrong. Please check your file.")
  } else  p("")

})



## Breeder information :
output$breederBoxPltMat <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = "Breeder",
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


})
