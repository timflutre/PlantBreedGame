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

## Server for game's administration


## Function
source("src/func_admin.R", local=TRUE, encoding = "UTF-8")$value


## Main UI: ----
output$adminUI <- renderUI({
    if (breederStatus()=="game master"){
        source("src/ui_admin_loggedIn.R", local=TRUE, encoding="UTF-8")$value
    }else{
        shinydashboard::box(width=12, title = "Content unavailable",
                            div(p("Sorry, this is only accessible to the game master."))
        )
    }
})




## Breeders management ----
# add new breeder:
observeEvent(input$addNewBreeder,{

    progressNewBreeder <- shiny::Progress$new(session, min=0, max=7)
    progressNewBreeder$set(value = 0,
                           message = "Adding breeder",
                           detail = "Initialisation...")

    t <- try(addNewBreeder(input$newBreederName,
                           input$newBreederStatus,
                           input$newBreederPsw,
                           progressNewBreeder))

    if (class(t)!="try-error"){
        progressNewBreeder$set(value = 7,
                               detail = "Done!")
    }else{progressNewBreeder$set(value = 1,
                                 detail = t)}


})

# delete breeder:
observeEvent(input$deleteBreeder,{
    if (input$delBreederName!=""){
        progressDelBreeder <- shiny::Progress$new(session, min=0, max=1)
        progressDelBreeder$set(value = 0,
                               message = "Deleting breeder")
    }

    if (input$delBreederName!="admin" & input$delBreederName!=""){
        deleteBreeder(input$delBreederName)
        progressDelBreeder$set(value = 1,
                               message = "Deleting breeder",
                               detail = "Done!")
    }else if (input$delBreederName=="admin") {
        progressDelBreeder$set(value = 0,
                               message = "Deleting breeder",
                               detail = "Sorry, admin can't be deleted.")
    }

})



## Sessions managment ----
sessionsList <- eventReactive((input$addSession|input$deleteSession),ignoreNULL=FALSE,{

    # get session table from the data base:
    db <- dbConnect(SQLite(), dbname=setup$dbname)
    query <- paste0("SELECT * FROM sessions")
    res <- dbGetQuery(conn=db, query)
    dbDisconnect(db)
    return(res)

})

output$sessionsTable <- renderTable({
    sessionsList()
})


# add session
observeEvent(input$addSession,{
    startDate <- strptime(paste0(input$startDate," ", input$startHour,":",input$startMin),
                          format="%Y-%m-%d %H:%M")
    endDate <- strptime(paste0(input$endDate," ", input$endHour,":",input$endMin),
                        format="%Y-%m-%d %H:%M")

    error <- 0

    # check start date before end date
    if (startDate >= endDate){
        error <- error + 1
        showNotification("Error: Start date must be earlier than end date.", type = c("error"))
    }

    # check overlaps
    db <- dbConnect(SQLite(), dbname=setup$dbname)
    query <- "SELECT * FROM sessions"
    res <- dbGetQuery(conn=db, query)
    dbDisconnect(db)

    if(nrow(res) > 0){
      overlapse <- apply(res, 1, function(session){
        sessionStart <- strptime(session["start"], format="%Y-%m-%d %H:%M")
        sessionEnd <- strptime(session["end"], format="%Y-%m-%d %H:%M")
        if((startDate < sessionStart &  endDate <= sessionStart)
           | (startDate >= sessionEnd &  endDate > sessionEnd)){
          return(FALSE)
        } else return(TRUE)
      })

      if (any(overlapse)){
        error <- error + 1
        showNotification("Error: Sessions must not be overlapped.", type = c("error"))
      }
    }

    #calculate id number:
    if( error == 0){
        if (nrow(sessionsList()) != 0){
            numId <- max(sessionsList()$num)+1
        }else numId <- 1


        # complete "sessions" table
        db <- dbConnect(SQLite(), dbname=setup$dbname)
        query <- paste0("INSERT INTO sessions", " VALUES",
                        " ('", numId,"','", startDate,"','", endDate,"','", input$yearTime,"')")
        res <- dbExecute(conn=db, query)
        dbDisconnect(db)
        showNotification("Session added.", type = c("message"))
    }
})

# delete session
observeEvent(input$deleteSession,{
    if (input$delSession!=""){

        # delete entry in sessions' table
        db <- dbConnect(SQLite(), dbname=setup$dbname)
        query <- paste0("DELETE FROM sessions",
                        " WHERE num = ",input$delSession)
        res <- dbExecute(conn=db, query)
        dbDisconnect(db)
        showNotification("Session removed",type = "message")

    }

})



## Constant managment ----

# 1. seed.year.effect:
# get the current value
output$admin_currentSYE <- renderText({
    input$admin_button_seedYearEfect # take depedency
    db <- DBI::dbConnect(RSQLite::SQLite(), dbname = setup$dbname)
    query <- paste0("SELECT value FROM constants WHERE item=='seed.year.effect'")
    yearEffectSeed <- as.numeric(DBI::dbGetQuery(db, query))
    DBI::dbDisconnect(db)
    yearEffectSeed
})

# update new value
observeEvent(input$admin_button_seedYearEfect,{
    newSeed <- input$admin_seedYearEfect

    # check input value (must be a numeric)
    checkOK=TRUE
    if(is.na(newSeed)) checkOK=FALSE

    # update data base
    checkDB <- 1
    if(checkOK){
        db <- DBI::dbConnect(RSQLite::SQLite(), dbname = setup$dbname)
        query <- paste0("UPDATE constants SET value = ",
                        newSeed," WHERE item=='seed.year.effect'")
        checkDB <- DBI::dbExecute(db, query)
        DBI::dbDisconnect(db)


    }

    # notification messages
    if(checkOK & checkDB==1){
        notifMessage <- paste("seed.year.effect updated.")
        showNotification(notifMessage,
                         duration = 2, closeButton = TRUE,
                         type = "default")
    }else if(!checkOK){# !checkOK
        notifMessage <- paste("ERROR: Submitted value is not an integer.")
        showNotification(notifMessage,
                         duration = 2, closeButton = TRUE,
                         type = "error")
    }else { # checkOK & checkDB!=1
        notifMessage <- paste("ERROR during SQL execution")
        showNotification(notifMessage,
                         duration = 2, closeButton = TRUE,
                         type = "error")
    }

})




## Disk usage managment  ----
output$sizeDataFolder <- renderTable({
    # data frame containing the size of all subfolder of "data"
    invalidateLater(60000)
    withProgress({
        if (breederStatus()=="game master"){
            # get list of all subfolders in "truth" and "shared"
            folderShared <- list.dirs(path = "data/shared", full.names = TRUE, recursive = TRUE)[-1]
            folderTruth <- list.dirs(path = "data/truth", full.names = TRUE, recursive = TRUE)[-1]
            subFolders <- c(folderShared,folderTruth)

            # get size of each subfolders
            funApply <- function(folder){
                files <- list.files(folder, all.files = TRUE, recursive = TRUE, full.names=T)
                sum(file.info(files)$size)
            }
            infoDataFolder <- as.data.frame(sapply(subFolders, FUN=funApply,USE.NAMES = FALSE),
                                            col.names=c("size"))
            names(infoDataFolder) <- c("size")
            infoDataFolder$path <- subFolders

            # clac size of "shared"
            infoDataFolder <- rbind(infoDataFolder,
                                    data.frame(path="data/shared",
                                               size=sum(infoDataFolder$size[infoDataFolder$path %in% folderShared]))
            )

            # clac size of "truth"
            #   1.sum of all subfolders
            infoDataFolder <- rbind(infoDataFolder,
                                    data.frame(path="data/truth",
                                               size=sum(infoDataFolder$size[infoDataFolder$path %in% folderTruth]))
            )
            #   2. sum of all initial collection
            sizeInitCol <- sum(file.info(list.files("data/truth", recursive = FALSE, full.names = TRUE))$size)
            infoDataFolder[infoDataFolder$path=="data/truth","size"] <- infoDataFolder[infoDataFolder$path=="data/truth","size"] + sizeInitCol

            # get size of the database
            infoDataFolder <- rbind(infoDataFolder,
                                    data.frame(path="data/breeding-game.sqlite",
                                               size=file.info("data/breeding-game.sqlite")$size)
            )

            # clac size of all "data" folder
            sizeData <- sum(infoDataFolder[infoDataFolder$path %in% c("data/shared",
                                                                      "data/truth",
                                                                      "data/breeding-game.sqlite"),
                                           "size"])
            infoDataFolder <- rbind(infoDataFolder,
                                    data.frame(path="data",
                                               size=sizeData)
            )

            # order table
            infoDataFolder <- infoDataFolder[order(infoDataFolder$size, decreasing = T),]

            # convert in Mo
            infoDataFolder$size <- infoDataFolder$size/10^6
            infoDataFolder <- rev(infoDataFolder)

            # var names
            names(infoDataFolder) <- c("path", "size (Mo)")

            return(infoDataFolder)
        }else return(NULL)
    },message = "Calculating... Please wait.")
})


observeEvent(input$updateMaxDiskUsage,{
    # save maximum disk usage value in the database
    # so that if the admin change the value, it will affect all connected users
    maxDiskUsage <- input$admin_maxDiskUsage

    db <- dbConnect(SQLite(), dbname=setup$dbname)
    query <- paste0("UPDATE constants SET value = '", maxDiskUsage,"' WHERE item = 'max.disk.usage'" )
    dbExecute(conn=db, query)
    dbDisconnect(db)
})

currentMaxDiskUsage <- reactive({
    input$admin_maxDiskUsage # take depedency
    input$updateMaxDiskUsage # take depedency

    db <- dbConnect(SQLite(), dbname=setup$dbname)
    tbl <- "breeders"
    query <- paste0("SELECT value FROM constants WHERE item = 'max.disk.usage'")
    maxDiskUsage <- dbGetQuery(conn=db, query)[,1]
    dbDisconnect(db)

    maxDiskUsage
})

output$InfoCurrentMaxDiskUsage <- renderText({
    paste("Current maximum disk usage:", currentMaxDiskUsage(), "Gb")
})
