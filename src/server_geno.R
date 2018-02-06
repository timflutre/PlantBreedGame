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
source("src/func_geno.R", local=TRUE, encoding = "UTF-8")$value


###### server for "genotyping" ######



## read uploaded file
readQryGeno <- reactive({
  
  # no input fileI
  if(is.null(input$file.geno)){
    return(NULL)
  }else if (breeder()=="No Identification"){
    return("error - You are not connected")
  }

  # read input file
  test <-  try(df <- readCheckBreedDataFile(input$file.geno$datapath,
                                            subset.snps=subset.snps))


  if (is.data.frame(test)){
    # the file is ok

    df <- df[df$task == "geno",] # keep only "geno" requests

    # list individuals
    indList <- unique(as.character(df$ind))
    indAvail <- indAvailable(indList, getGameTime(setup), breeder())

    # check if individuals are available
    if ((indAvail$indGrown | breederStatus()=="game master")
        & indAvail$indExist){
      return(df)
    }else {return("error - Individuals not availables")}

  }else {return("error - wrong file format")}# wrong file
})



## check
output$GenoUploaded <- renderPrint({
  if (is.data.frame(readQryGeno())){
    print("GOOD")
  } else if (is.null(readQryGeno())){
    print("No file uploaded")
  } else print(readQryGeno())

})



## summary
output$GenoInvoice <- renderTable({
  if (is.data.frame(readQryGeno())){
    createInvoiceGeno(readQryGeno())
  }
})

# output$GenoSmy <- renderPrint({
#   if (is.data.frame(readQryGeno())){
#     summary(as.data.frame(apply(readQryGeno(), MARGIN = 2, FUN = as.factor)))
#   }
# })
# output$GenoStr <- renderPrint({
#   if (is.data.frame(readQryGeno())){
#     print(str(readQryGeno()))
#   }
# })


## data
output$qryGeno <- renderDataTable({
  if (is.data.frame(readQryGeno())){
    readQryGeno()
  }
})



## output
geno_data <- eventReactive(input$requestGeno,{

  if (is.data.frame(readQryGeno())){
    # Create a Progress object
    progressGeno <- shiny::Progress$new(session, min=0, max=4)
    progressGeno$set(value = 0,
                       message = "Process Geno request:",
                       detail = "Initialisation...")
    
    res <- try(genotype(breeder(),
                        readQryGeno(),
                        getGameTime(setup),
                        progressGeno))
    if (res=="done"){
      progressGeno$set(value = 4,
                       detail = "Done")

      return(res)
    }else{
      progressGeno$set(detail = "ERROR !")
      return("error")
    }

  }else return(NULL)
})

output$genoRequestResultUI <- renderUI({
  
  if (!is.null(geno_data()) && geno_data()=="done"){
    # reset inputs
    reset("file.geno")
    session$sendCustomMessage(type = "resetValue", message = "file.geno")
    
    # display message
    p("Great ! Your results will be available in ",
      constants$duration.geno.hd, " months.")
    
  } else if (!is.null(geno_data()) && geno_data()=="error"){
    p("Something went wrong. Please check your file.")
  } else p("")

})





## Breeder information :
output$breederBoxGeno <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = "Breeder",
    icon = icon("user-o"),
    color = "yellow",
    width = 4
  )
})

output$dateBoxGeno <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format= "%Y-%m-%d"),
    icon = icon("calendar"),
    color = "yellow",
    width = 4
  )
})

output$budgetBoxGeno <- renderValueBox({
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow",
    width = 4
  )
})

output$UIbreederInfoGeno <- renderUI({
  if (breeder()!="No Identification"){
    list(infoBoxOutput("breederBoxGeno"),
         infoBoxOutput("dateBoxGeno"),
         infoBoxOutput("budgetBoxGeno"))
  }

})



##  DEBUG
output$GenoDebug <- renderPrint({
  print("---------")
  print(ls())


})
