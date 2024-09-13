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

## Server for game's administration


## Function
source("src/fun/func_admin.R", local = TRUE, encoding = "UTF-8")$value


## Main UI: ----
output$adminUI <- renderUI({
  if (breederStatus() == "game master" | !gameInitialised()) {
    return(source("src/ui/ui_admin_loggedIn.R", local = TRUE, encoding = "UTF-8")$value)
  } else {
    return(
      shinydashboard::box(
        width = 12, title = "Content unavailable",
        div(p("Sorry, you need the 'game-master' status to access this."))
      )
    )
  }
})




## Breeders management ----
# add new breeder:
observeEvent(input$addNewBreeder, {
  progressNewBreeder <- shiny::Progress$new(session, min = 0, max = 7)
  progressNewBreeder$set(
    value = 0,
    message = "Adding breeder",
    detail = "Initialisation..."
  )

  t <- try(addNewBreeder(
    input$newBreederName,
    input$newBreederStatus,
    input$newBreederPsw,
    progressNewBreeder
  ))

  if (class(t) != "try-error") {
    progressNewBreeder$set(
      value = 7,
      detail = "Done!"
    )
  } else {
    progressNewBreeder$set(
      value = 1,
      detail = t
    )
  }

  values$lastDBupdate <- Sys.time()
})

# delete breeder:
breeder_list_server("admin_breeder_list_for_deletion", "delBreederName", breederList)

observeEvent(input$deleteBreeder, {
  if (input$delBreederName != "") {
    progressDelBreeder <- shiny::Progress$new(session, min = 0, max = 1)
    progressDelBreeder$set(
      value = 0,
      message = "Deleting breeder"
    )
  }

  if (input$delBreederName != "admin" & input$delBreederName != "test" & input$delBreederName != "") {
    deleteBreeder(input$delBreederName)
    progressDelBreeder$set(
      value = 1,
      message = "Deleting breeder",
      detail = "Done!"
    )
  } else if (input$delBreederName == "admin" | input$delBreederName == "test") {
    progressDelBreeder$set(
      value = 0,
      message = "Deleting breeder",
      detail = paste(
        "Sorry,",
        input$delBreederName, "can't be deleted."
      )
    )
  }
  values$lastDBupdate <- Sys.time()
})



## Sessions managment ----
output$sessionTimeZoneUI <- renderUI({
  default = "UTC"
  if (input$client_time_zone %in% OlsonNames())  {
    default = input$client_time_zone
  }
  selectInput(
    "sessionTimeZone",
    "Time zone",
    choices = as.list(OlsonNames()),
    selected = default)
})

output$deleteSessionUI <- renderUI({
  selectInput("delSession", "Session's id",
    choices = c("", sessionsList()$id),
    selected = "", width = "100%"
  )
})



sessionsList <- reactive({
  # get session table from the data base:
  values$lastDBupdate
  getGameSessions()
})

output$sessionsTable <- renderTable({
  data <- sessionsList()

  if (nrow(data) == 0) {
    return(
      structure(list(
        "id" = character(0),
        "Session start time" = character(0),
        "Session end time" = character(0),
        "Session time zone" = character(0),
        "Year duration (mins)" = character(0),
        "Game time start" = character(0),
        "Game time end" = character(0),
        "Session duration" = character(0)
      ),
      class = "data.frame")
    )
  }

  data$Game_Time_start <- sapply(seq(1, nrow(data)), function(line){
    start <- getGameTime(time_irl = strptime(data[line, "start"],
                                    format = "%Y-%m-%d %H:%M",
                                    tz = data[line, "time_zone"])
                )

  })

  data$Game_Time_end <- sapply(seq(1, nrow(data)), function(line){
    end <- getGameTime(time_irl = strptime(data[line, "end"],
                                    format = "%Y-%m-%d %H:%M",
                                    tz = data[line, "time_zone"])
                )

  })

  game_duration_days <- difftime(data$Game_Time_end, data$Game_Time_start, units = "days")
  game_duration_year <- game_duration_days / 365.2425
  data$Session_duration <- paste("~", round(game_duration_year, 2), "years")

  data$Game_Time_start <- strftime(data$Game_Time_start, format = "%Y-%m-%d")
  data$Game_Time_end <- strftime(data$Game_Time_end, format = "%Y-%m-%d")

  colnames(data) <- c(
    "id",
    "Session start time",
    "Session end time",
    "Year duration (mins)",
    "Session time zone",
    "Game time start",
    "Game time end",
    "Session duration"
  )
  data <- data[,c(
    "id",
    "Session start time",
    "Session end time",
    "Session time zone",
    "Year duration (mins)",
    "Game time start",
    "Game time end",
    "Session duration"
  )]
  return(data)
})


# add session
observeEvent(input$addSession, {
  startDate <- strptime(paste0(input$startDate, " ", input$startHour, ":", input$startMin),
    format = "%Y-%m-%d %H:%M",
    tz = input$sessionTimeZone
  )
  endDate <- strptime(paste0(input$endDate, " ", input$endHour, ":", input$endMin),
    format = "%Y-%m-%d %H:%M",
    tz = input$sessionTimeZone
  )

  # check start date before end date
  if (startDate >= endDate) {
    showNotification("Error: Start date must be earlier than end date.", type = c("error"))
    return(NULL)
  }

  # check overlaps
  gameSessions <- getGameSessions()

  if (nrow(gameSessions) > 0) {
    overlapse <- apply(gameSessions, 1, function(session) {
      (sessionStart <- strptime(session["start"], format = "%Y-%m-%d %H:%M", tz = session["time_zone"] ))
      (sessionEnd <- strptime(session["end"], format = "%Y-%m-%d %H:%M", tz = session["time_zone"] ))
      if ((startDate < sessionStart & endDate <= sessionStart) |
        (startDate >= sessionEnd & endDate > sessionEnd)) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    })

    if (any(overlapse)) {
      showNotification("Error: Sessions must not overlap.", type = c("error"))
      return(NULL)
    }
  }

  # calculate id number:
  id <- 1
  if (nrow(sessionsList()) != 0) {
    id <- max(sessionsList()$id) + 1
  }


  # complete "sessions" table
  addGameSession(
    id = id,
    startDate = as.character(startDate),
    endDate = as.character(endDate),
    yearTime = input$yearTime,
    timeZone = input$sessionTimeZone
  )
  values$lastDBupdate <- Sys.time()
  showNotification("New session added.", type = c("message"))
})

# delete session
observeEvent(input$deleteSession, {
  if (input$delSession != "") {
    # delete entry in sessions' table
    delGameSession(input$delSession)
    values$lastDBupdate <- Sys.time()
    showNotification("Session removed", type = "message")
  }
})



## Constant managment ----

# get the current value
output$admin_currentSYE <- renderText({
  input$admin_button_seedYearEfect # take depedency
  getBreedingGameConstants()$seed.year.effect
})
output$admin_current_initial_budget <- renderText({
  input$admin_button_const_initialBudget # take depedency
  game_constants <- getBreedingGameConstants()
  game_constants$initialBudget / game_constants$cost.pheno.field
})

# update new value
observeEvent(input$admin_button_seedYearEfect, {
  newSeed <- input$admin_seedYearEfect

  error <- valid_positive_integer(newSeed)
  if (is.null(error)) {
    query <- paste0(
      "UPDATE constants SET value = ",
      newSeed, " WHERE item=='seed.year.effect'"
    )
    db_execute_request(query)

    notifMessage <- paste("Year effect seed updated.")
    showNotification(notifMessage,
      duration = 2, closeButton = TRUE,
      type = "default"
    )

    return(NULL)
  }

    notifMessage <- paste("ERROR:", error)
    showNotification(notifMessage,
      duration = 2, closeButton = TRUE,
      type = "error"
    )

})

observeEvent(input$admin_button_const_initialBudget, {

  new_initial_budget <- input$admin_const_initialBudget * getBreedingGameConstants()$cost.pheno.field

  error <- valid_positive_number(new_initial_budget)
  if (is.null(error)) {
    query <- paste0(
      "UPDATE constants SET value = ",
      new_initial_budget, " WHERE item=='initialBudget'"
    )
    db_execute_request(query)

    notifMessage <- paste("Initial budget updated.")
    showNotification(notifMessage,
      duration = 2, closeButton = TRUE,
      type = "default"
    )

    return(NULL)
  }

    notifMessage <- paste("ERROR:", seed_error)
    showNotification(notifMessage,
      duration = 2, closeButton = TRUE,
      type = "error"
    )

})



## Disk usage managment  ----
output$dataFolderTree <- shinyTree::renderTree({
  invalidateLater(60000)
  get_folder_tree("data", exclude_files = FALSE, excluded_files_ext = "RData")
})

output$currentDataUsage <- renderUI({
  invalidateLater(60000)
  current_size <- get_folder_size(DATA_ROOT)
  max_usage <- getBreedingGameConstants()$max.disk.usage
  current_usage <- round(current_size / (max_usage * 10^9) * 100, 0)
  p(paste0("Current usage: ",
    prettyunits::pretty_bytes(current_size),
    " / ",
    max_usage,
    " GB (",
    current_usage,
    "%)"))
})

observeEvent(input$updateMaxDiskUsage, {
  # save maximum disk usage value in the database
  # so that if the admin change the value, it will affect all connected users
  maxDiskUsage <- input$admin_maxDiskUsage

  query <- paste0("UPDATE constants SET value = '", maxDiskUsage, "' WHERE item = 'max.disk.usage'")
  db_execute_request(query)
})

currentMaxDiskUsage <- reactive({
  input$admin_maxDiskUsage # take depedency
  input$updateMaxDiskUsage # take depedency

  maxDiskUsage <- getBreedingGameConstants()$max.disk.usage
  maxDiskUsage
})

output$InfoCurrentMaxDiskUsage <- renderText({
  paste("Current maximum disk usage:", currentMaxDiskUsage(), "GB")
})







## Game progress ----

admin_gameProgressDta <- eventReactive(input$admin_progressButton, {
  progress_bar <- shiny::Progress$new(session, min = 0, max = 4)
  calcGameProgress(progress_bar)
})


output$admin_plotAllIndGameProgress <- renderPlotly({
  dta <- admin_gameProgressDta()

  # extract BV of the requested trait
  if (input$admin_progressTrait == "Trait 1") {
    dta$BV <- dta$trait1
  } else if (input$admin_progressTrait == "Trait 2") {
    dta$BV <- dta$trait2
  }

  plot_ly(
    data = dta,
    type = "scatter",
    mode = "markers",
    x = ~ jitter(gen),
    y = ~BV,
    color = ~breeder,
    opacity = 0.75,
    hoverinfo = "text",
    text = ~ paste0(
      "<b>", ind, "</b>", # (in bold)
      "\nparent1: ", parent1,
      "\nparent2: ", parent2,
      "\nBV trait1 = ", round(trait1, 2),
      "\nBV trait2 = ", round(trait2, 2),
      "\nBV trait1 x trait2 = ", round(t1t2, 2)
    )
  ) %>%
    layout(
      title = paste0("All individuals (", input$admin_progressTrait, ")"),
      xaxis = list(
        title = "Generation",
        dtick = 1
      ),
      yaxis = list(
        title = "Breedind values"
      )
    )
})


output$admin_plotMaxIndGameProgress <- renderPlotly({
  dta <- admin_gameProgressDta()

  # extract BV of the requested trait
  if (input$admin_progressTrait == "Trait 1") {
    dta$BV <- dta$trait1
  } else if (input$admin_progressTrait == "Trait 2") {
    dta$BV <- dta$trait2
  }


  # get the best individuals per breeder per generation
  bestBV <- NULL
  for (breeder in unique(dta$breeder)) {
    tmp1 <- dta[dta$breeder == breeder, ]
    for (gen in unique(tmp1$gen)) {
      tmp2 <- tmp1[tmp1$gen == gen, ]
      bestBV <- rbind(
        bestBV,
        tmp2[tmp2$BV == max(tmp2$BV), ]
      )
    }

    # add initial collection ind to all breeders
    if (breeder != "Initial collection") {
      bestIni <- bestBV[bestBV$breeder == "Initial collection", ]
      bestIni$breeder <- breeder
      bestBV <- rbind(bestBV, bestIni)
    }
  }

  # remove "Initial collection" breeder
  bestBV <- bestBV[bestBV$breeder != "Initial collection", ]


  plot_ly(
    data = bestBV[order(bestBV$gen), ],
    type = "scatter",
    mode = "lines+markers",
    x = ~gen,
    y = ~BV,
    color = ~breeder,
    hoverinfo = "text",
    text = ~ paste0(
      "<b>", ind, "</b>", # (in bold)
      "\nparent1: ", parent1,
      "\nparent2: ", parent2,
      "\nBV trait1 = ", round(trait1, 2),
      "\nBV trait2 = ", round(trait2, 2),
      "\nBV trait1 x trait2 = ", round(t1t2, 2)
    )
  ) %>%
    layout(
      title = paste0("Best individuals (", input$admin_progressTrait, ")"),
      xaxis = list(
        title = "Generation",
        dtick = 1
      ),
      yaxis = list(
        title = "Breedind values"
      )
    )
})


output$admin_boxPlotGameProgress <- renderPlotly({
  dta <- admin_gameProgressDta()

  # extract BV of the requested trait
  if (input$admin_progressTrait == "Trait 1") {
    dta$BV <- dta$trait1
  } else if (input$admin_progressTrait == "Trait 2") {
    dta$BV <- dta$trait2
  }

  plot_ly(
    data = dta,
    type = "box",
    x = ~gen,
    y = ~BV,
    hoverinfo = "y",
    color = ~breeder
  ) %>%
    layout(
      boxmode = "group",
      title = paste0("All individuals (", input$admin_progressTrait, ")"),
      xaxis = list(
        title = "Generation",
        dtick = 1
      ),
      yaxis = list(
        title = "Breedind values"
      )
    )
})



breeder_list_server("admin_breeder_list_gameProgress", "admin_T1T2Breeder", breederList)
output$admin_T1T2GameProgress <- renderPlotly({
  dta <- admin_gameProgressDta()

  # extract BV of the requested breeder
  dta <- dta[dta$breeder %in% c(input$admin_T1T2Breeder, "Initial collection"), ]

  dta$gen <- as.character(dta$gen)

  plot_ly(
    data = dta,
    type = "scatter",
    mode = "markers",
    x = ~trait1,
    y = ~trait2,
    color = ~gen,
    opacity = 0.75,
    hoverinfo = "text",
    text = ~ paste0(
      "<b>", ind, "</b>", # (in bold)
      "\nparent1: ", parent1,
      "\nparent2: ", parent2,
      "\nBV trait1 = ", round(trait1, 2),
      "\nBV trait2 = ", round(trait2, 2),
      "\nBV trait1 x trait2 = ", round(t1t2, 2)
    )
  ) %>%
    layout(
      title = paste0("Trait 1 vs Trait 2 (", input$admin_T1T2Breeder, ")"),
      xaxis = list(
        title = "Trait 1"
      ),
      yaxis = list(
        title = "Trait 2"
      )
    )
})


output$download_game_init_report <- downloadHandler(
  filename = paste0("plantBreedGame_initialisation_report_", strftime(Sys.time(),format = "%Y-%m-%d"), ".html"), # lambda function
  content = function(file) file.copy(GAME_INIT_REPORT, file),
  contentType = "text/html"
)

output$download_actual_marker_effects <- downloadHandler(
  filename = paste0("plantBreedGame_actual_marker_effects_", strftime(Sys.time(),format = "%Y-%m-%d"), ".csv"), # lambda function
  content = function(file) {
    f <- paste0(DATA_TRUTH, "/p0.RData")
    load(f)
    marker_effects <- as.data.frame(p0$Beta)
    marker_effects$SNP <- rownames(marker_effects)
    marker_effects <- marker_effects[, c("SNP", "trait1", "trait2")]
    write.csv(marker_effects, file = file, row.names = FALSE)
  }
)
output$causal_resist_snp <- renderText({
  f <- paste0(DATA_TRUTH, "/p0.RData")
  load(f)
  p0$trait3$qtn.id
})

output$initialisation_button <- renderUI({
  if (!gameInitialised()) {
    return(
      actionButton("initialiseGame", "Initialise Game")
    )
  }

  return(
    div(
      div(
        h3("Important!"),
        p(
          "The game is already initialised. Reinitialising the game",
          strong("will erase all the current game data"),
          ". (All the breeders will be deleted along wiht their data.)"
        ),
        p("To reinitialise the game, write", code("plantbreedgame"), "in the", code("Confirmation"), "field below", "and click on the", code("Re-Initialise Game"), "button below.")
      ),
      div(
        style = "display: table-row",
        div(
          style = "display: table-cell; padding-right: 5px;",
          textInput("initialisation_security_text", label = "Confirmation:", value = "This action will erase all the data.")
        ),
        div(
          style = "display: table-cell; padding-left: 5px; vertical-align: bottom",
          actionButton("initialiseGame", "Re-Initialise Game")
        )
      )
    )
  )
})




gameInit_input_validator <- InputValidator$new()
gameInit_seed <- gameInit_seed_server("gameInit_seed", gameInit_input_validator)
gameInit_costs <- gameInit_costs_server("gameInit_costs", gameInit_input_validator)
gameInit_constraints <- gameInit_request_constraints_server("gameInit_request_constraints", gameInit_input_validator)
gameInit_traits <- gameInit_traits_server("gameInit_geno_pheno_simul", gameInit_input_validator)


gameInit_input_validator$add_rule("initialisation_security_text", function(x) {
  if (is.null(x)) return(NULL)
  if (x != "plantbreedgame") return("")
  }
)

gameInit_input_validator$enable()


observe({
  if (gameInit_input_validator$is_valid()) {
    shinyjs::enable("initialiseGame")
    return(TRUE)
  }
  shinyjs::disable("initialiseGame")
})


observeEvent(input$initialiseGame, {
  progress_bar <- shiny::Progress$new(session, min = 0, max = 18)

  progress_bar$set(
    value = 1,
    message = "Game Initialisation:",
    detail = "Initialisation..."
  )

  if (!gameInit_input_validator$is_valid()) {
    progress_bar$set(
      value = 1,
      message = "Game Initialisation:",
      detail = "ERROR, invalid parameters"
    )
    return(NULL)
  }

  progress_bar$set(
    value = 2,
    message = "Game Initialisation:",
    detail = "game setup..."
  )

  params <- list(
    progressBar = progress_bar,
    rng_seed = gameInit_seed$value(),

    cost.pheno.field = gameInit_costs$value()$cost.pheno.field,
    cost.pheno.patho = gameInit_costs$value()$cost.pheno.patho,
    cost.allof = gameInit_costs$value()$cost.allof,
    cost.autof = gameInit_costs$value()$cost.autof,
    cost.haplodiplo = gameInit_costs$value()$cost.haplodiplo,
    cost.geno.hd = gameInit_costs$value()$cost.geno.hd,
    cost.geno.ld = gameInit_costs$value()$cost.geno.ld,
    cost.geno.single = gameInit_costs$value()$cost.geno.single,
    cost.register = gameInit_costs$value()$cost.register,
    initialBudget = gameInit_costs$value()$initialBudget,

    t1_mu = gameInit_traits$value()$t1_mu,
    t1_min = gameInit_traits$value()$t1_min,
    t1_cv_g = gameInit_traits$value()$t1_cv_g,
    t1_h2 = gameInit_traits$value()$t1_h2,

    t2_mu = gameInit_traits$value()$t2_mu,
    t2_min = gameInit_traits$value()$t2_min,
    t2_cv_g = gameInit_traits$value()$t2_cv_g,
    t2_h2 = gameInit_traits$value()$t2_h2,

    prop_pleio = gameInit_traits$value()$prop_pleio,
    cor_pleio = gameInit_traits$value()$cor_pleio,

    max.nb.haplodiplos = gameInit_constraints$value()$n_pheno_plot,
    max.nb.pltmatReq = gameInit_constraints$value()$n_max_cross,
    nb.plots = gameInit_constraints$value()$n_max_hd,
    maxEvalInds = gameInit_constraints$value()$n_max_registration
  )

  rmd_env <- new.env(parent = globalenv())
  report_build_dir <- tempdir()
  out_report <- tryCatch({
    rmarkdown::render("src/plantbreedgame_setup.Rmd",
      output_file = tempfile(tmpdir = report_build_dir),
      intermediates_dir = report_build_dir, # important for nix pkg
      encoding = "UTF-8",
      params = params,
      knit_root_dir = getwd(),
      envir = rmd_env
    )
  }, error = function(err) {
      progress_bar$close()
      showNotification("Game Initialisation Error.", type = "error")
      showModal(modalDialog(
        size = "l",
        title = tags$span(class = "has-error", "Game initialisation failed."),
        p("The game initialisation script failed with the following error message:"),
        p(code(err$message)),
        p("after step: ", code(rmd_env$progress_detail)),
        p("This problem will likely be fixed by using a",
          strong("different RNG seed"),
          ". If the problem is recurrent, please ",
          a("click here to report this issue on GitHub.",
            href = create_issue_link(
              title = paste0("Game initialisation error: `", err$message, "`"),
              body = paste0(
                "Encountered error message:\n\n```console\n",
                err$message,
                "\n```\n\n",
                "For reproducibility, here are the parameters used",
                " for this failed game initialisation:\n\n```json\n",
                as.character(jsonlite::toJSON(
                  params[names(params) != "progressBar"],
                  pretty = T,
                  auto_unbox = T
                )),
                "\n```\n\n[Please add other information about",
                " how you enconter this problem.]"
              )
            )
          )
        )
      ))
      return("error")
  })

  if (identical(out_report, "error")) {
    return(NULL)
  }

  new_session_id <- rmd_env$session_id
  new_report_path <- file.path(
    DATA_ROOT,
    new_session_id,
    "reports",
    "plantBreedGame_initialisation_report.html"
  )
  file.copy(from = out_report, to = new_report_path)

  addResourcePath("reports", new_report_path)

  CURRENT_SESSION_ID <<- readLines(DATA_IN_USE_FILE)
  DATA_SESSION <<- normalizePath(file.path(DATA_ROOT, CURRENT_SESSION_ID), mustWork = TRUE)

  DATA_TRUTH <<- file.path(DATA_SESSION, "truth")
  DATA_SHARED <<- file.path(DATA_SESSION, "shared")
  DATA_INITIAL_DATA <<- file.path(DATA_SHARED, "initial_data")
  DATA_DB <<- file.path(DATA_SESSION, "breeding-game.sqlite")
  DATA_REPORTS <<- file.path(DATA_SESSION, "reports")
  GAME_INIT_REPORT <<- file.path(DATA_REPORTS, "plantBreedGame_initialisation_report.html")

  if (dir.exists(DATA_SESSION)) {
    addResourcePath("reports", DATA_REPORTS)
  }



  progress_bar$set(
    value = progress_bar$max,
    message = "Game Initialisation:",
    detail = "Done"
  )

  alert("Game initialisation finished. This page will automatically refresh.")
  gameInitialised()
  shinyjs::refresh()
})
