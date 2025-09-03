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
source("src/fun/func_pheno.R", local = TRUE, encoding = "UTF-8")$value


## server for "phenotyping"
output$pheno_main_UI <- renderUI({
  if (!gameInitialised()) {
    return(source("./src/ui/ui_gameNotInitialised.R", local = TRUE, encoding = "UTF-8")$value)
  }
  return(source("./src/ui/ui_pheno.R", local = TRUE, encoding = "UTF-8")$value)
})

## identification message
output$idMessagePheno <- renderUI({
  if (breeder() == "No Identification") {
    p("You need to identify yourself in order to make a request: click on the 'Identification' tab.",
      style = "color:red;"
    )
  }
})


# read uploaded file
readQryPheno <- reactive({
  # no input fileI
  if (is.null(input$file.pheno)) {
    return(NULL)
  } else if (breeder() == "No Identification") {
    return("error - You are not connected")
  }

  # read input file
  max.nb.plots <- ifelse(breederStatus() != "player",
    Inf, getBreedingGameConstants()$nb.plots
  )

  subset.snps <- getSNPsubset()
  test <- try(df <- readCheckBreedDataFile(input$file.pheno$datapath,
    subset.snps = subset.snps,
    max.nb.plots = max.nb.plots
  ))


  if (is.data.frame(test)) {
    # the file is ok

    # keep only "pheno" requests
    df <- df[(df$task == "pheno-field" | df$task == "pheno-patho"), ]

    # list individuals
    indList <- unique(as.character(df$ind))
    pheno_start_date <- as.Date(paste(get_phenotyping_year(getGameTime()),
                                      getBreedingGameConstants()$max.upload.pheno.field,
                                      sep = "-"))

    indAvail <- indAvailable(indList, pheno_start_date, breeder())

    # check if plot are available
    plotAvail <- plotAvailable(breeder(), df, getGameTime())

    # check if individuals are available
    if (!indAvail$indExist) {
      return("error - Some requested individuals do not exist")
    }
    if (breederStatus() != "player") {
      return(df)
    }
    if (!indAvail$indGrown) {
      return("error - Some requested individuals are not available for phenotyping this season")
    }
    if (!plotAvail) {
      return("error - No more phenotyping plots are available for this season")
    }
    return(df)
  } else {
    return(test)
  }
})



# check
output$PhenoUploaded <- renderPrint({
  if (is.data.frame(readQryPheno())) {
    writeLines("GOOD")
  } else if (is.null(readQryPheno())) {
    writeLines("No file uploaded")
  } else {
    writeLines(readQryPheno())
  }
})



# summary
output$PhenoInvoice <- renderTable({
  if (is.data.frame(readQryPheno())) {
    createInvoicePheno(readQryPheno())
  }
})

# data
output$qryPheno <- renderDataTable(
  {
    if (is.data.frame(readQryPheno())) {
      readQryPheno()
    }
  },
  options = list(lengthMenu = c(10, 20, 50), pageLength = 10)
)

# submit button
output$submitPhenoRequest <- renderUI({
  colorButton <- ifelse(is.data.frame(readQryPheno()), "#00A65A", "#ff0000") # yes:green, no:red

  list(
    tags$head(
      tags$style(HTML(paste0("#requestPheno{background-color:", colorButton, "; color: white}")))
    ),
    p("Do you really want these phenotyping data?"),
    actionButton("requestPheno", "Yes, I do!") # style="background-color:red"
  )
})

# output
pheno_data <- eventReactive(input$requestPheno, {
  request_time <- getGameTime()
  if (is.data.frame(readQryPheno())) {
    # Create a Progress object
    progressPheno <- shiny::Progress$new(session, min = 0, max = 4)
    progressPheno$set(
      value = 0,
      message = "Process Pheno request:",
      detail = "Initialisation..."
    )

    request_name <- get_unique_request_name(breeder(), tools::file_path_sans_ext(input$file.pheno$name))

    db_add_request(id = NA,
                   breeder = breeder(),
                   name = request_name,
                   type = "pheno",
                   game_date = request_time)
    new_request <- db_get_game_requests(breeder = breeder(), name = request_name)
    add_pheno_req_data(req_id = new_request$id, request_data = readQryPheno())

    res <- try(process_pheno_request(new_request$id, progressPheno = progressPheno))

    if (res == "done") {
      writeRequest(readQryPheno(), breeder(), input$file.pheno$name)
      progressPheno$set(
        value = 4,
        detail = "Done"
      )
      return(res)
    } else {
      progressPheno$set(detail = paste0("error in phenotype (", res, ")"))
      return("error")
    }
  } else {
    return(NULL)
  }
})


output$phenoRequestResultUI <- renderUI({
  if (!is.null(pheno_data()) && pheno_data() == "done") {
    reset("file.pheno")
    session$sendCustomMessage(type = "resetValue", message = "file.pheno")
    p("Great ! Your results will be available soon.")
  } else if (!is.null(pheno_data()) && pheno_data() == "error") {
    p("Something went wrong. Please check your file.")
  }
})




## Breeder information :
output$breederBoxPheno <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = paste("Status:", breederStatus()),
    icon = icon("user"),
    color = "yellow"
  )
})

output$dateBoxPheno <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format = "%d %b %Y"),
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
  if (breeder() != "No Identification") {
    list(
      infoBoxOutput("breederBoxPheno", width = 3),
      infoBoxOutput("dateBoxPheno", width = 3),
      infoBoxOutput("budgetBoxPheno", width = 3),
      infoBoxOutput("serverIndicPheno", width = 3)
    )
  }
})

## DEBUG
#
output$PhenoDebug <- renderPrint({
  print("-----")
  print(readQryPheno())
})
