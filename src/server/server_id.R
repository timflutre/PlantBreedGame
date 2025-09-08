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


## server for "identification"

## Function
source("src/fun/func_id.R", local = TRUE, encoding = "UTF-8")$value


## Call ui_id_loggedIn.R ----
output$id_main_UI <- renderUI({
  if (!gameInitialised()) {
    return(source("./src/ui/ui_gameNotInitialised.R", local = TRUE, encoding = "UTF-8")$value)
  }
  if (accessGranted()) {
    return(source("./src/ui/ui_id_loggedIn.R", local = TRUE, encoding = "UTF-8")$value)
  }
  return(source("./src/ui/ui_id_askForLogin.R", local = TRUE, encoding = "UTF-8")$value)
})


## get breeder list and create select input ----
breederList <- reactive({
  values$lastDBupdate # add a dependency to the db updates
  getBreederList()
})
breeder_list_server("login_breeder_list", "breederName", breederList)



## log in ----
accessGranted <- eventReactive(input$submitPSW,
  ignoreNULL = FALSE,
  {
    # The access is granted if this conditions are true:
    # 1. the md5 sum of the given password match with the one in the data base
    #    and doesn't correspond to the empty string;
    # 2. the size of "data" folder is under the maximum disk usage limit.
    # If any condition is false, a javascript "alert" will show up explaining why
    # it is not possible to log in.
    #
    # EXCEPTIONS:
    # * The game master can log in even if the second condition is false,
    # but the javascript "alert" will explain that the server is full.
    # * A "tester" is the only status allowed to have an empty password.

    if (is.null(input$submitPSW)) { # button not yet available
      return(FALSE)
    }
    if (input$submitPSW == 0) { # button not pressed
      return(FALSE)
    }

    # 1. get breeder status
    breeder <- db_get_breeder(input$breederName)

    # 2. check given password
    hashPsw <- breeder$h_psw

    if (hashPsw == digest(input$psw, "md5", serialize = FALSE)) {
      goodPswd <- TRUE
    } else {
      goodPswd <- FALSE
      alert("Error: wrong password")
    }

    # 3. check disk usage
    goodDiskUsage <- FALSE
    if (goodPswd && breeder$status != "game master") {
      withProgress(
        {
          maxDiskUsage <- getBreedingGameConstants()$max.disk.usage

          allDataFiles <- list.files(DATA_SESSION, all.files = TRUE, recursive = TRUE, full.names = FALSE)
          currentSize <- get_folder_size(DATA_ROOT) / 10^9

          if (currentSize < maxDiskUsage) {
            goodDiskUsage <- TRUE
          } else if (breeder$status != "game master") {
            goodDiskUsage <- FALSE
            alert("Sorry, the game is currently not available because of disk usage.\nPlease contact your game master to figure out what to do.")
          } else {
            goodDiskUsage <- TRUE
            alert(paste0(
              "Warning! The size of the \"data\" folder exceeds the specified limit\n",
              paste("of", round(currentSize, 2), "GB (maximum size allowed:", maxDiskUsage, "GB).\n"),
              "To preserve your server, players can't log in anymore (but connected users can still play).\n",
              "If you want to resume the game, please raise the maximum disk usage limit.\n",
              "Go to the Admin tab, then \"Disk usage\", and raise the threshold."
            ))
          }
        },
        message = "Connecting..."
      )
    } else if (goodPswd && breeder$status == "game master") {
      # the game master can always log in
      goodDiskUsage <- TRUE
    }

    # 5. output
    if (goodPswd && goodDiskUsage) {
      removeUI("#logInDiv")

      submitted_inds <- db_get_individual(breeder = breeder$name,
                                          selected_for_eval = 1,
                                          public_columns = TRUE)
      submittedInds(submitted_inds[,c("Name", "Parent 1", "Parent 2")])

      return(TRUE)
    } else {
      return(FALSE)
    }
  }
)


breeder <- reactive({
  if (accessGranted()) {
    input$breederName
  } else {
    "No Identification"
  }
})

breederStatus <- reactive({
  if (accessGranted()) {
    return(db_get_breeder(input$breederName)$status)
  } else {
    return("No Identification")
  }
})

budget <- reactive({
  input$leftMenu
  input$requestGeno
  input$id_submitInds
  if (breeder() != "No Identification") {

    req_history <- db_get_game_requests_history(breeder = breeder())
    expenses <- sum(req_history$costs, na.rm = TRUE)

    initialBuget <- getBreedingGameConstants()$initialBudget
    return(round(initialBuget - expenses, 2))
  }
})







## download files ----
# list of avaiable files (this must be reactive value to be refresh)
phenoFiles <- reactive({
  input$leftMenu
  db_get_game_requests(breeder = breeder(), type = "pheno")$name
  # getDataFileList(type = "pheno", breeder = breeder())
})
genoData <- reactive({
  input$leftMenu
  full_files <- db_get_genotypes_data_list(breeder = breeder())
  tools::file_path_sans_ext(basename(full_files), compression = TRUE)
})
pltMatFiles <- reactive({
  input$leftMenu
  # choices <- getDataFileList(type = "pltMat", breeder = breeder())
})
requestFiles <- reactive({
  input$leftMenu
  # choices <- getDataFileList(type = "request", breeder = breeder())
})


# dwnl buttons ----
output$dwnlPheno <- downloadHandler(
  filename = function() input$phenoFile, # lambda function
  content = function(file) {
    initFiles <- list.files(DATA_INITIAL_DATA)
    if (input$phenoFile %in% initFiles) {
      folder <- DATA_INITIAL_DATA
    } else {
      folder <- file.path(DATA_SHARED, breeder())
    }
    filePath <- file.path(folder, input$phenoFile)
    file.copy(filePath, file)
  }
)

output$dwnlGeno <- downloadHandler(
  filename = function() paste0(input$genoFile, ".txt.gz"), # lambda function
  content = function(file) {
    gFile <- paste0(input$genoFile, ".txt.gz")
    initFiles <- list.files(DATA_INITIAL_DATA)
    if (gFile %in% initFiles) {
      folder <- DATA_INITIAL_DATA
    } else {
      folder <- file.path(DATA_SHARED, breeder())
    }
    filePath <- file.path(folder, gFile)
    file.copy(filePath, file)
  }
)

output$dwnlGeno_vcf <- downloadHandler(
  filename = function() paste0(input$genoFile, ".vcf.gz"), # lambda function
  content = function(file) {
    gFile_txt <- paste0(input$genoFile, ".txt.gz")
    initFiles <- list.files(DATA_INITIAL_DATA)
    if (gFile_txt %in% initFiles) {
      folder <- DATA_INITIAL_DATA
    } else {
      folder <- file.path(DATA_SHARED, breeder())
    }
    gFile_txt <- file.path(folder, gFile_txt)
    progressVcf <- shiny::Progress$new(session, min = 0, max = 5)
    progressVcf$set(
      value = 0,
      message = "Create VCF File:",
      detail = "Initialisation..."
    )
    txt2Vcf(gFile_txt, file, progressVcf)
    progressVcf$set(
      value = 5,
      detail = "DONE!"
    )
  }
)

output$dwnlPltMat <- downloadHandler(
  filename = function() input$pltMatFile, # lambda function
  content = function(file) {
    initFiles <- list.files(DATA_INITIAL_DATA)
    if (input$pltMatFile %in% initFiles) {
      folder <- DATA_INITIAL_DATA
    } else {
      folder <- file.path(DATA_SHARED, breeder())
    }
    filePath <- file.path(folder, input$pltMatFile)
    file.copy(filePath, file)
  }
)


output$dwnlRequest <- downloadHandler(
  filename = function() input$requestFile, # lambda function
  content = function(file) {
    initFiles <- list.files(DATA_INITIAL_DATA)
    if (input$requestFile %in% initFiles) {
      folder <- DATA_INITIAL_DATA
    } else {
      folder <- file.path(DATA_SHARED, breeder())
    }
    filePath <- file.path(folder, input$requestFile)
    file.copy(filePath, file)
  }
)


output$UIdwnlGeno <- renderUI({
  if (input$genoFile != "") {
    genoFile <- paste0(input$genoFile, ".txt.gz")
    if (breederStatus() == "player" && !availToDwnld(genoFile, currentGTime())$isAvailable) {
      p(paste0(
        "Sorry, your data are not available yet. Delivery date: ",
        availToDwnld(genoFile, currentGTime())$availDate
      ))
    } else {
      div(
        downloadButton("dwnlGeno", "Download as `txt.gz`"),
        downloadButton("dwnlGeno_vcf", "Download as `vcf.gz`")
      )
    }
  } else {
    p("No file selected.")
  }
})


output$UIdwnlRequest <- renderUI({
  if (input$requestFile != "") {
    downloadButton("dwnlRequest", "Download your file")
  } else {
    p("No file selected.")
  }
})


## My plant-material ----
pltmat_preview_filter <- individual_filtering_server("inds_download_ind_filter", breeder = breeder())

plant_mat_preview_data <- reactive({

  # input dependencies
  input$leftMenu
  input$id_submitInds
  input$id_delSubmitInds

  individuals <- db_get_individual(breeder = breeder(),
     ind_id = pltmat_preview_filter$inds_ids(),
     public_columns = TRUE
  )

  # columns_to_keep_as <- c(
  #   "name" = "Name",
  #   "parent1_name" = "Parent 1",
  #   "parent2_name" = "Parent 2",
  #   "avail_from" = "Available date",
  #   "cross_type" = "Crossing type",
  #   "request_name" = "From plant material request",
  #   "control" = "Is control"
  # )
  # individuals <- individuals[, c(
  #   names(columns_to_keep_as)
  # )]
  # colnames(individuals) <- columns_to_keep_as
  return(individuals)
})

output$plant_mat_preview <- DT::renderDataTable({
  DT::datatable(
    plant_mat_preview_data(),
    selection = 'single',
    options = list(
      lengthMenu = c(10, 20, 50),
      pageLength = 10,
      searchDelay = 500
    )
  )
})

.download_inds <- function(){
  downloadHandler(
    filename = "plant-material.tsv",
    content = function(file) {
      write.table(
        plant_mat_preview_data(),
        file = file,
        sep = "\t",
        row.names = FALSE
      )
    }
  )
}

output$dwnlInds_1 <- .download_inds()
output$dwnlInds_2 <- .download_inds()

output$selected_ind_info <- renderUI({
  selected_row <- input$plant_mat_preview_rows_selected

  if (is.null(selected_row)) {
    return(
      div(
        h3("Selected individual information:"),
        p("No individual selected. Click on an individual on the table to view more information.")
      )
    )
  }
  ind_name <- plant_mat_preview_data()[selected_row, "Name"]
  ind_id <- db_get_individuals_ids(breeder = breeder(), name = ind_name)
  ind_info <- db_get_individual(breeder = breeder(), ind_id = ind_id)
  phenotypes <- db_get_phenotypes(breeder = breeder(), ind_id = ind_id, public_columns = TRUE)
  genotypes <- db_get_genotypes(breeder = breeder(), ind_id = ind_id)
  offsprings <- rbind(db_get_individual(breeder = breeder(), parent1 = ind_info$name, public_columns = TRUE),
                  db_get_individual(breeder = breeder(), parent2 = ind_info$name, public_columns = TRUE))
  offsprings <- offsprings[!duplicated(offsprings),]

  div(
    h3(ind_info$name, ":"),
    tags$ul(
      tags$li("Requested on", ind_info$request_date, "with", code(ind_info$request_name)),
      tags$li("Available on", ind_info$avail_from),
      tags$li("Cross type:", ind_info$cross_type),
      tags$li("Parent 1:", code(ind_info$parent1_name)),
      tags$li("Parent 2:", code(ind_info$parent2_name)),
      tags$li("Offsprings:", nrow(offsprings)),
      tags$li("Phenotypic records:", nrow(phenotypes)),
      tags$li("Genotypic records:", nrow(genotypes),
        tags$ul(
          lapply(seq_len(nrow(genotypes)), function(x) {
            geno_res_file <- genotypes$result_file[x]
            geno_res_file <- tools::file_path_sans_ext(
              basename(geno_res_file),
              compression = TRUE
            )
            tags$li(genotypes$type[x], ":", code(geno_res_file))
          })
        )
      ),
    ),
    h4("Phenotypic records:"),
    reactable::reactable(phenotypes),

    h4("Offsprings records:"),
    reactable::reactable(offsprings)
  )
})



pheno_inds_filters <- individual_filtering_server("pheno_download_ind_filter", breeder = breeder())
pheno_pheno_filters <- phenotype_filtering_server("pheno_download_pheno_filter", breeder = breeder())

pheno_data_preview <- reactive({

  download_pheno_button_clicks() # dependency on download buttons
  input$refresh_pheno_preview # dependency on refresh buttons

  breeder <- breeder()
  pheno_data <- db_get_phenotypes(
    breeder = breeder,
    # ind_id = inds_ids,
    ind_id = pheno_inds_filters$inds_ids(),
    request_name = pheno_pheno_filters$pheno_request(),
    pathogen = pheno_pheno_filters$pathogen(),
    year = pheno_pheno_filters$years(),
    public_columns = FALSE
  )

  # mask phenotype data that are not yet available for players
  if (breederStatus() == "player") {
    not_available <- pheno_data$avail_from > getGameTime()
    pheno_data[not_available, c("pathogen", "trait1", "trait2", "trait3")] <- paste0("available on ", pheno_data$avail_from[not_available])
  }
  pheno_data <- pheno_data[, c(
    "ind",
    "control_ind",
    "year",
    "plot",
    "pathogen",
    "trait1",
    "trait2",
    "trait3"
  )]
  return(pheno_data)
})


output$pheno_preview_DT <- DT::renderDataTable({
  DT::datatable(
    isolate(pheno_data_preview()), # use isolate, the data refresh is done with the observer below
    # filter = "top", # this can conflict with manual filters,
                      # add this only if this filtering is taken
                      # into account for data-download (and it is explained in the UI)
    # style = "bootstrap4",
    rownames = FALSE,
    options = list(
      language = list(emptyTable = 'Empty'),
      pageLength = 10,
      lengthMenu = c(10, 25, 50, 100),
      searchDelay = 500
    )
  )
}, server = TRUE)

observe({
  pheno_table_proxy <- DT::dataTableProxy("pheno_preview_DT", deferUntilFlush = FALSE)
  DT::replaceData(pheno_table_proxy, pheno_data_preview(), resetPaging = TRUE, rownames = FALSE)
  # we could use `resetPaging = FALSE` to keep the current page,
  # but the table will be empty if the current page would not exist with
  # the updated data
})


download_pheno_button_clicks <- reactiveVal(value = 0)
.download_pheno <- function(){
  downloadHandler(
    filename = "phenotypes.tsv",
    content = function(file) {

      download_pheno_button_clicks(download_pheno_button_clicks() + 1)
      write.table(
        pheno_data_preview(),
        file = file,
        sep = "\t",
        row.names = FALSE
      )
    }
  )
}

output$dwnlPheno_1 <- .download_pheno()
output$dwnlPheno_2 <- .download_pheno()





## Change Password ----
pswChanged <- eventReactive(input$"changePsw", {
  hashPsw <- db_get_breeder(breeder = breeder())$h_psw
  if (digest(input$prevPsw, "md5", serialize = FALSE) == hashPsw) {
    newHashed <- digest(input$newPsw, "md5", serialize = FALSE)
    db_update_breeder(breeder = breeder(), new_h_psw = newHashed)
    return(TRUE)
  }
  return(FALSE)
})

output$UIpswChanged <- renderUI({
  if (pswChanged()) {
    p("Password Updated")
  } else if (!pswChanged()) {
    p("Wrong password, try again")
  }
})



## Breeder information ----
output$breederBoxID <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = paste("Status:", breederStatus()),
    icon = icon("user"),
    color = "yellow"
  )
})

output$dateBoxID <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format = "%d %b %Y"),
    icon = icon("calendar"),
    color = "yellow"
  )
})



output$budgetBoxID <- renderValueBox({
  x <- input$id_submitInds
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow"
  )
})
outputOptions(output, "budgetBoxID", priority = 9)

output$serverIndicID <- renderValueBox({
  ## this bow will be modified by some javascript
  valueBoxServer(
    value = "",
    subtitle = "Server load",
    icon = icon("server"),
    color = "yellow"
  )
})

output$UIbreederInfoID <- renderUI({
  if (breeder() != "No Identification") {
    list(
      infoBoxOutput("breederBoxID", width = 3),
      infoBoxOutput("dateBoxID", width = 3),
      infoBoxOutput("budgetBoxID", width = 3),
      infoBoxOutput("serverIndicID", width = 3)
    )
  }
})



## Final Individuals submission ----

# add new inds for submission
observeEvent(input$id_submitInds, priority = 10, {
  if (is.null(input$id_evalInds)) {
    return(NULL)
  }

  submitted_inds <- db_get_individual(breeder = breeder(), selected_for_eval = 1)
  ind_ids <- db_get_individuals_ids(breeder = breeder(), names = input$id_evalInds)
  ind_ids <- setdiff(ind_ids, submitted_inds$id)

  if (any(db_get_individual(ind_id = ind_ids)$breeder == "@ALL")) {
    alert("You can not submit individuals from the initial collection.")
    return(NULL)
  }

  constants <- getBreedingGameConstants()
  remaining_submission <- constants$maxEvalInds - nrow(submitted_inds)

  if (length(ind_ids) > remaining_submission) {

    alert(paste("Sorry, you have already submitted", nrow(submitted_inds), "individuals, on a total of", constants$maxEvalInds, ". You can only submit", remaining_submission, "more individuals."))
    return(NULL)
  }

  db_add_evaluation_requests(breeder = breeder(),
                             ind_ids = ind_ids,
                             game_date = getGameTime())

  # update submittedInds table
  submitted_inds <- db_get_individual(breeder = breeder(),
                                      selected_for_eval = 1,
                                      public_columns = TRUE)
  submittedInds(submitted_inds[,c("Name", "Parent 1", "Parent 2")])

  # reset input
  reset("id_evalInds", asis = FALSE)
  return(TRUE)
})




# delete inds for submission
observeEvent(input$id_delSubmitInds, priority = 11, {
  if (is.null(input$submittedIndsDT_rows_selected)) {
    return(NULL)
  }
  names_of_inds_to_delete <- submittedInds()[input$submittedIndsDT_rows_selected, "Name"]
  ind_ids <- db_get_individuals_ids(breeder = breeder(), names = names_of_inds_to_delete)
  db_remove_evlauation_inds(ind_ids)

  # update submittedInds table
  submitted_inds <- db_get_individual(breeder = breeder(),
                                      selected_for_eval = 1,
                                      public_columns = TRUE)
  submittedInds(submitted_inds[,c("Name", "Parent 1", "Parent 2")])
  return(TRUE)
})


submittedInds <- reactiveVal(value = data.frame(
  "Name" = character(),
  `Parent 1` = character(),
  `Parent 2` = character(),
  check.names = FALSE
))

output$submittedIndsDT <- renderDataTable({
  DT::datatable(submittedInds(),
    filter = c("none"),
    style = "bootstrap4",
    options = list(sDom = '<"top">rt<"bottom">')
  )
})




# DEBUG ----

output$IdDebug <- renderPrint({
  print("----")
  print(input$phenoFile)
  print(input$genoFile)
})
