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
source("src/func_pheno.R", local=TRUE, encoding = "UTF-8")$value


## server for "phenotyping"

## identification message
output$idMessagePheno <- renderUI({
  if(breeder()=="No Identification"){
    p("You need to identify yourself in order to make a request: click on the 'Breeder identification' tab.",
      style="color:red;")
  }
})


# read uploaded file
readQryPheno <- reactive({

  # no input fileI
  if(is.null(input$file.pheno)){
    return(NULL)
  }else if (breeder()=="No Identification"){
    return("error - You are not connected")
  }

  # read input file
  max.nb.plots <- ifelse(breederStatus()!="player",
                         Inf , constants$nb.plots)
  test <-  try(df <- readCheckBreedDataFile(input$file.pheno$datapath,
                                            subset.snps=subset.snps,
                                            max.nb.plots = max.nb.plots))


  if (is.data.frame(test)){
    # the file is ok

    # keep only "pheno" requests
    df <- df[(df$task == "pheno-field" | df$task == "pheno-patho"),]

    # list individuals
    indList <- unique(as.character(df$ind))
    indAvail <- indAvailable(indList, getGameTime(setup), breeder())
    
    # check if plot are available
    plotAvail <- plotAvailable(breeder(), df, getGameTime(setup))

    # check if individuals are available
    if (((indAvail$indGrown & plotAvail) | breederStatus()!="player")
        & indAvail$indExist){
      return(df)
    }else {return("error - Individuals or Plots not availables")}
  }else {return(test)}
})



# check
output$PhenoUploaded <- renderPrint({
  if (is.data.frame(readQryPheno())){
    writeLines("GOOD")
  } else if (is.null(readQryPheno())){
    writeLines("No file uploaded")
  } else writeLines(readQryPheno())

})



# summary
output$PhenoInvoice <- renderTable({
  if (is.data.frame(readQryPheno())){
    createInvoicePheno(readQryPheno())
  }
})

# data
output$qryPheno <- renderDataTable({
  if (is.data.frame(readQryPheno())){
    readQryPheno()
  }
},options = list(lengthMenu = c(10, 20, 50), pageLength = 10))

# submit button
output$submitPhenoRequest <- renderUI({
  
  colorButton <- ifelse(is.data.frame(readQryPheno()),"#00A65A","#ff0000") # yes:green, no:red
  
  list(
    tags$head(
      tags$style(HTML(paste0('#requestPheno{background-color:',colorButton,'; color: white}')))
    ),
    p("Do you really want these phenotyping data?"),
    actionButton("requestPheno", "Yes, I do!") # style="background-color:red"
  )
  
})

# output
pheno_data <- eventReactive(input$requestPheno,{
  if (is.data.frame(readQryPheno())){
    
    
    # Create a Progress object
    progressPheno <- shiny::Progress$new(session, min=0, max=4)
    progressPheno$set(value = 0,
                     message = "Process Pheno request:",
                     detail = "Initialisation...")
    
    res <- try(phenotype(breeder(),
                         readQryPheno(),
                         getGameTime(setup),
                         progressPheno,
                         input$file.pheno$name))
    if (res=="done"){
      writeRequest(readQryPheno(),breeder(),input$file.pheno$name)
      progressPheno$set(value = 4,
                        detail = "Done")
      return(res)
    }else{
      progressPheno$set(detail = "ERROR !")
      return("error")
    } 

  }else return(NULL)

})


output$phenoRequestResultUI <- renderUI({
  if (!is.null(pheno_data()) && pheno_data()=="done"){
    reset("file.pheno")
    session$sendCustomMessage(type = "resetValue", message = "file.pheno")
    p("Great ! Your results will be available soon.")
  }  else if (!is.null(pheno_data()) && pheno_data()=="error"){
    p("Something went wrong. Please check your file.")
  }

})




## Breeder information :
output$breederBoxPheno <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = "Breeder",
    icon = icon("user-o"),
    color = "yellow"
  )
})

output$dateBoxPheno <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format= "%d %b %Y"),
    icon = icon("calendar"),
    color = "yellow"
  )
})



output$budgetBoxPheno <- renderValueBox({
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow"
  )
})

output$serverIndicPheno <- renderValueBox({
  ## this bow will be modified by some javascript
  valueBoxServer(
    value = "", 
    subtitle = "Server load",
    icon = icon("server"),
    color = "yellow"
  )
})

output$UIbreederInfoPheno <- renderUI({
  if (breeder()!="No Identification"){
    list(infoBoxOutput("breederBoxPheno", width = 3),
         infoBoxOutput("dateBoxPheno", width = 3),
         infoBoxOutput("budgetBoxPheno", width = 3),
         infoBoxOutput("serverIndicPheno", width = 3))
  }
})

## DEBUG
#
output$PhenoDebug <- renderPrint({
  print("-----")
  print(readQryPheno())

})
