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



## Function
source("src/fun/func_plant_material.R", local=TRUE, encoding = "UTF-8")$value


## server for "plant material"

## identification message
output$idMessagePltMat <- renderUI({
  if(breeder()=="No Identification"){
    p("You need to identify yourself in order to make a request: click on the 'Identification' tab.",
      style="color:red;")
  }
})



# read uploaded file
readQryPlmat <- reactive({

  # no input fileI
  if(is.null(input$file.plmat)){
    return(NULL)
  }else if (breeder()=="No Identification"){
    return("error - You are not connected")
  }


  # read input file
  maxHD <- ifelse(breederStatus()!="player",
                  Inf,constants$max.nb.haplodiplos)
  maxReq <- ifelse(breederStatus()!="player",
                  Inf,constants$max.nb.pltmatReq)

  test <- try(df <- readCheckBreedPlantFile(input$file.plmat$datapath,
                                            max.nb=maxReq,
                                            max.nb.hd=maxHD))

  # check file
  if (!is.data.frame(test)){
    return(test)
  }
  # the file is ok


  # check new individuals not already exist
  childList <- unique(c(as.character(df$child)))

  if (indExist(childList, breeder())){
    return("error - Individuals already exist")
  }

  # list individuals
  indList <- unique(c(as.character(df$parent1),as.character(df$parent2)))
  indList <- indList[!is.na(indList)]
  indAvail <- indAvailable(indList, getGameTime(setup), breeder())

  # check if individuals are available
  if ((indAvail$indGrown | breederStatus()!="player")
      & indAvail$indExist){
    return(df)
  }else {return("error - Individuals not availables")}
})



# check
output$plmatUploaded <- renderPrint({

  if (is.data.frame(readQryPlmat())){
    writeLines("GOOD")
  } else if (is.null(readQryPlmat())){
    writeLines("No file uploaded")
  } else writeLines(readQryPlmat())


})



# summary
output$PltmatInvoice <- renderTable({
  if (is.data.frame(readQryPlmat())){
    createInvoicePltmat(readQryPlmat())
  }
})


# data
output$qryPlmat <- renderDataTable({
  if (is.data.frame(readQryPlmat())){
    readQryPlmat()
  }

},options = list(lengthMenu = c(10, 20, 50), pageLength = 10))

# submit button
output$submitPlmatRequest <- renderUI({

  colorButton <- ifelse(is.data.frame(readQryPlmat()),"#00A65A","#ff0000") # yes:green, no:red

  list(
    tags$head(
      tags$style(HTML(paste0('#requestPlmat{background-color:',colorButton,'; color: white}')))
    ),
    p("Do you really want this plant material?"),
    actionButton("requestPlmat", "Yes, I do!") # style="background-color:red"
  )

})


# output
plantMatRequested <- eventReactive(input$requestPlmat,{
  if (is.data.frame(readQryPlmat())){

    # Create a Progress object
    progressPltMat <- shiny::Progress$new(session, min=0, max=5)
    progressPltMat$set(value = 0,
                       message = "Create Plant Material:",
                       detail = "Initialisation...")


    res <- try(create_plant_material(breeder(),
                                     readQryPlmat(),
                                     getGameTime(setup),
                                     progressPltMat))

    if (res=="done"){
      writeRequest(readQryPlmat(),breeder(),input$file.plmat$name)
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
    subtitle = paste("Status:", breederStatus()),
    icon = icon("user-o"),
    color = "yellow"
  )
})

output$dateBoxPltMat <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format= "%d %b %Y"),
    icon = icon("calendar"),
    color = "yellow"
  )
})



output$budgetBoxPltMat <- renderValueBox({
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow"
  )
})

output$serverIndicPltMat <- renderValueBox({
  ## this bow will be modified by some javascript
  valueBoxServer(
    value = "",
    subtitle = "Server load",
    icon = icon("server"),
    color = "yellow"
  )
})

output$UIbreederInfoPltMat <- renderUI({
  if (breeder()!="No Identification"){
    list(infoBoxOutput("breederBoxPltMat", width = 3),
         infoBoxOutput("dateBoxPltMat", width = 3),
         infoBoxOutput("budgetBoxPltMat", width = 3),
         infoBoxOutput("serverIndicPltMat", width = 3))
  }
})





## DEBUG
#
output$plmatDebug <- renderPrint({
  print("-----")


})
