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

# read uploaded file
readQryPheno <- reactive({

  # no input fileI
  if(is.null(input$file.pheno)){
    return(NULL)
  }

  # read input file
  test <-  try(df <- readCheckBreedDataFile(input$file.pheno$datapath,
                                            subset.snps=subset.snps))


  if (is.data.frame(test)){
    # the file is ok

    # keep only "pheno" requests
    df <- df[(df$task == "pheno-field" | df$task == "pheno-patho"),]

    # list individuals
    indList <- unique(as.character(df$ind))
    indAvail <- indAvailable(indList, getGameTime(setup), breeder())


    # check if individuals are available
    if ((indAvail$indGrown | breederStatus()=="game master")
        & indAvail$indExist){
      return(df)
    }else {return("error - Individuals not availables")}
  }else {return("error - wrong file format")}
})



# check
output$PhenoUploaded <- renderPrint({
  if (is.data.frame(readQryPheno())){
    print("GOOD")
  } else if (is.null(readQryPheno())){
      print("No file uploaded")
  } else print("Not good")

})



# summary
output$PhenoSmy <- renderPrint({
  if (is.data.frame(readQryPheno())){
    summary(as.data.frame(apply(readQryPheno(), MARGIN = 2, FUN = as.factor)))
  }
})
output$PhenoStr <- renderPrint({
  if (is.data.frame(readQryPheno())){
    print(str(readQryPheno()))
  }
})


# data
output$qryPheno <- renderDataTable({
  if (is.data.frame(readQryPheno())){
    readQryPheno()
  }
})



# output
pheno_data <- eventReactive(input$requestPheno,{
  if (is.data.frame(readQryPheno())){
    res <- try(phenotype(breeder(), readQryPheno(), getGameTime(setup)))
    beep()
    if (res=="done"){
      return(res)
    }else return("error")

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
    color = "yellow",
    width = 4
  )
})

output$dateBoxPheno <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format= "%Y-%m-%d"),
    icon = icon("calendar"),
    color = "yellow",
    width = 4
  )
})



output$budgetBoxPheno <- renderValueBox({
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow",
    width = 4
  )
})

output$UIbreederInfoPheno <- renderUI({
  if (breeder()!="No Identification"){
    list(infoBoxOutput("breederBoxPheno"),
         infoBoxOutput("dateBoxPheno"),
         infoBoxOutput("budgetBoxPheno"))
  }

})


## DEBUG
#
output$PhenoDebug <- renderPrint({
  print("-----")
  print(readQryPheno())

})
