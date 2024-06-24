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
    source("src/ui/ui_admin_loggedIn.R", local = TRUE, encoding = "UTF-8")$value
  } else {
    shinydashboard::box(
      width = 12, title = "Content unavailable",
      div(p("Sorry, you need the 'game-master' status to access this."))
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

# 1. seed.year.effect:
# get the current value
output$admin_currentSYE <- renderText({
  input$admin_button_seedYearEfect # take depedency
  yearEffectSeed <- getBreedingGameConstants()$seed.year.effect
  yearEffectSeed
})

# update new value
observeEvent(input$admin_button_seedYearEfect, {
  newSeed <- input$admin_seedYearEfect

  # check input value (must be a numeric)
  checkOK <- TRUE
  if (is.na(newSeed)) checkOK <- FALSE

  # update data base
  checkDB <- 1
  if (checkOK) {
    query <- paste0(
      "UPDATE constants SET value = ",
      newSeed, " WHERE item=='seed.year.effect'"
    )
    db_execute_request(query)
  }

  # notification messages
  if (checkOK & checkDB == 1) {
    notifMessage <- paste("seed.year.effect updated.")
    showNotification(notifMessage,
      duration = 2, closeButton = TRUE,
      type = "default"
    )
  } else if (!checkOK) { # !checkOK
    notifMessage <- paste("ERROR: Submitted value is not an integer.")
    showNotification(notifMessage,
      duration = 2, closeButton = TRUE,
      type = "error"
    )
  } else { # checkOK & checkDB!=1
    notifMessage <- paste("ERROR during SQL execution")
    showNotification(notifMessage,
      duration = 2, closeButton = TRUE,
      type = "error"
    )
  }
})




## Disk usage managment  ----
output$sizeDataFolder <- renderTable({
  # data frame containing the size of all subfolder of "data"
  invalidateLater(60000)
  withProgress(
    {
      if (breederStatus() == "game master") {
        # get list of all subfolders in "truth" and "shared"
        folderShared <- list.dirs(path = "data/shared", full.names = TRUE, recursive = TRUE)[-1]
        folderTruth <- list.dirs(path = "data/truth", full.names = TRUE, recursive = TRUE)[-1]
        subFolders <- c(folderShared, folderTruth)

        # get size of each subfolders
        funApply <- function(folder) {
          files <- list.files(folder, all.files = TRUE, recursive = TRUE, full.names = T)
          sum(file.info(files)$size)
        }
        infoDataFolder <- as.data.frame(sapply(subFolders, FUN = funApply, USE.NAMES = FALSE),
          col.names = c("size")
        )
        names(infoDataFolder) <- c("size")
        infoDataFolder$path <- subFolders

        # clac size of "shared"
        infoDataFolder <- rbind(
          infoDataFolder,
          data.frame(
            path = "data/shared",
            size = sum(infoDataFolder$size[infoDataFolder$path %in% folderShared])
          )
        )

        # clac size of "truth"
        #   1.sum of all subfolders
        infoDataFolder <- rbind(
          infoDataFolder,
          data.frame(
            path = "data/truth",
            size = sum(infoDataFolder$size[infoDataFolder$path %in% folderTruth])
          )
        )
        #   2. sum of all initial collection
        sizeInitCol <- sum(file.info(list.files("data/truth", recursive = FALSE, full.names = TRUE))$size)
        infoDataFolder[infoDataFolder$path == "data/truth", "size"] <- infoDataFolder[infoDataFolder$path == "data/truth", "size"] + sizeInitCol

        # get size of the database
        infoDataFolder <- rbind(
          infoDataFolder,
          data.frame(
            path = "data/breeding-game.sqlite",
            size = file.info("data/breeding-game.sqlite")$size
          )
        )

        # clac size of all "data" folder
        sizeData <- sum(infoDataFolder[
          infoDataFolder$path %in% c(
            "data/shared",
            "data/truth",
            "data/breeding-game.sqlite"
          ),
          "size"
        ])
        infoDataFolder <- rbind(
          infoDataFolder,
          data.frame(
            path = "data",
            size = sizeData
          )
        )

        # order table
        infoDataFolder <- infoDataFolder[order(infoDataFolder$size, decreasing = T), ]

        # convert in Mo
        infoDataFolder$size <- infoDataFolder$size / 10^6
        infoDataFolder <- rev(infoDataFolder)

        # var names
        names(infoDataFolder) <- c("path", "size (Mo)")

        return(infoDataFolder)
      } else {
        return(NULL)
      }
    },
    message = "Calculating... Please wait."
  )
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
  paste("Current maximum disk usage:", currentMaxDiskUsage(), "Gb")
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

observe({
  if (identical(input$initialisation_security_text, "plantbreedgame")) {
    shinyjs::enable("initialiseGame")
    return(TRUE)
  }
  shinyjs::disable("initialiseGame")
})


observeEvent(input$initialiseGame, {
  progress_bar <- shiny::Progress$new(session, min = 0, max = 1)

  progress_bar$set(
    value = 1 / 4,
    message = "Game Initialisation:",
    detail = "Initialisation..."
  )
  if (dir.exists(DATA_ROOT)) {
    # WARN / TODO --- IMPORTANT ! ---
    # the initialisation script do not allow its execution if "the data" folder
    # already exists.
    # Therefore here we will delete this folder, however, in general,
    # IT IS QUITE RISKY to delete folder with code. For example:
    # - if `DATA_ROOT` have been wrongly defined
    # - if a malicious agent placed files/folder inside DATA_ROOT
    # - if files are currently beeing created
    #
    # A better approach could be instead to only create a new `DATA_ROOT` folder
    # and save in the data-base (that should only be erase, not deleted) the current
    # `DATA_ROOT` to use.
    # The server administrator would then responsible to safely remove the unecessary data.
    #
    # Here, to mitigate the risks the application will remove the files it
    # has created (based on their names). This is not perfect as if one of this
    # file have been is symlinked to another, the unintended file could be deleted.
    #
    # WARN / TODO --- IMPORTANT ! ---
    progress_bar$set(
      value = 1 / 4,
      message = "Game Initialisation:",
      detail = "Delete existing data..."
    )
    clean_data_root()
  }
  progress_bar$set(
    value = 2 / 4,
    message = "Game Initialisation:",
    detail = "game setup..."
  )
  rmarkdown::render("./plantbreedgame_setup.Rmd",
    output_file = "./plantbreedgame_setup.html",
    encoding = "UTF-8"
  )
  progress_bar$set(
    value = 1,
    message = "Game Initialisation:",
    detail = "Done"
  )
  alert("Game initialisation finished. This page will automatically refresh.")
  gameInitialised()
  shinyjs::refresh()
})
