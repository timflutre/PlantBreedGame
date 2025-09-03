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
source("src/fun/func_geno.R", local = TRUE, encoding = "UTF-8")$value


###### server for "genotyping" ######
output$geno_main_UI <- renderUI({
  if (!gameInitialised()) {
    return(source("./src/ui/ui_gameNotInitialised.R", local = TRUE, encoding = "UTF-8")$value)
  }
  return(source("./src/ui/ui_geno.R", local = TRUE, encoding = "UTF-8")$value)
})

## identification message
output$idMessageGeno <- renderUI({
  if (breeder() == "No Identification") {
    p("You need to identify yourself in order to make a request: click on the 'Identification' tab.",
      style = "color:red;"
    )
  }
})



## read uploaded file
readQryGeno <- reactive({
  # no input fileI
  if (is.null(input$file.geno)) {
    return(NULL)
  } else if (breeder() == "No Identification") {
    return("error - You are not connected")
  }

  # read input file
  max.nb.inds <- ifelse(breederStatus() != "player",
    Inf, getBreedingGameConstants()$max.nb.inds
  )
  subset.snps <- getSNPsubset()
  test <- try(df <- readCheckBreedDataFile(input$file.geno$datapath,
    subset.snps = subset.snps,
    max.nb.inds = max.nb.inds
  ))


  if (is.data.frame(test)) {
    # the file is ok

    df <- df[df$task == "geno", ] # keep only "geno" requests

    # list individuals
    indList <- unique(as.character(df$ind))
    geno_start_date <- getGameTime()
    indAvail <- indAvailable(indList, geno_start_date, breeder())

    if (!indAvail$indExist) {
      return("error - Some requested individuals do not exist")
    }
    if (breederStatus() != "player") {
      return(df)
    }
    if (!indAvail$indGrown) {
      return("error - Some requested individuals are not available for phenotyping this season")
    }
    return(df)
  } else {
    return(test)
  }
})



## check
output$GenoUploaded <- renderPrint({
  if (is.data.frame(readQryGeno())) {
    writeLines("GOOD")
  } else if (is.null(readQryGeno())) {
    writeLines("No file uploaded")
  } else {
    writeLines(readQryGeno())
  }
})



## summary
output$GenoInvoice <- renderTable({
  if (is.data.frame(readQryGeno())) {
    createInvoiceGeno(readQryGeno())
  }
})


## data
output$qryGeno <- renderDataTable(
  {
    if (is.data.frame(readQryGeno())) {
      readQryGeno()
    }
  },
  options = list(lengthMenu = c(10, 20, 50), pageLength = 10)
)

## submit button
output$submitGenoRequest <- renderUI({
  colorButton <- ifelse(is.data.frame(readQryGeno()), "#00A65A", "#ff0000") # yes:green, no:red

  list(
    tags$head(
      tags$style(HTML(paste0("#requestGeno{background-color:", colorButton, "; color: white}")))
    ),
    p("Do you really want these genotyping data?"),
    actionButton("requestGeno", "Yes, I do!") # style="background-color:red"
  )
})

## output
geno_data <- eventReactive(input$requestGeno, {
  request_time <- getGameTime()
  if (is.data.frame(readQryGeno())) {
    # Create a Progress object
    progressGeno <- shiny::Progress$new(session, min = 0, max = 4)
    progressGeno$set(
      value = 0,
      message = "Process Geno request:",
      detail = "Initialisation..."
    )


    request_name <- get_unique_request_name(breeder(), tools::file_path_sans_ext(input$file.geno$name))
    db_add_request(id = NA,
                   breeder = breeder(),
                   name = request_name,
                   type = "geno",
                   game_date = request_time)
    new_request <- db_get_game_requests(breeder = breeder(), name = request_name)
    db_add_geno_req_data(req_id = new_request$id, request_data = readQryGeno())
    res <- try(process_geno_request(new_request$id, progressGeno = progressGeno))


    if (res == "done") {
      writeRequest(readQryGeno(), breeder(), input$file.geno$name)
      progressGeno$set(
        value = 4,
        detail = "Done"
      )

      return(res)
    } else {
      progressGeno$set(detail = "ERROR !")
      return("error")
    }
  } else {
    return(NULL)
  }
})



output$genoRequestResultUI <- renderUI({
  if (!is.null(geno_data()) && geno_data() == "done") {
    # reset inputs
    reset("file.geno")
    session$sendCustomMessage(type = "resetValue", message = "file.geno")

    # display message
    p(
      "Great ! Your results will be available in ",
      getBreedingGameConstants()$duration.geno.hd, " months."
    )
  } else if (!is.null(geno_data()) && geno_data() == "error") {
    p("Something went wrong. Please check your file.")
  } else {
    p("")
  }
})





## Breeder information :
output$breederBoxGeno <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = paste("Status:", breederStatus()),
    icon = icon("user"),
    color = "yellow"
  )
})

output$dateBoxGeno <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format = "%d %b %Y"),
    icon = icon("calendar"),
    color = "yellow"
  )
})

output$budgetBoxGeno <- renderValueBox({
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow"
  )
})

output$serverIndicGeno <- renderValueBox({
  ## this bow will be modified by some javascript
  valueBoxServer(
    value = "",
    subtitle = "Server load",
    icon = icon("server"),
    color = "yellow"
  )
})

output$UIbreederInfoGeno <- renderUI({
  if (breeder() != "No Identification") {
    list(
      infoBoxOutput("breederBoxGeno", width = 3),
      infoBoxOutput("dateBoxGeno", width = 3),
      infoBoxOutput("budgetBoxGeno", width = 3),
      infoBoxOutput("serverIndicGeno", width = 3)
    )
  }
})


##  DEBUG
output$GenoDebug <- renderPrint({
  print("---------")
  print(breeder())
})
