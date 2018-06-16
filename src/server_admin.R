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


## generate the UI:
output$adminUI <- renderUI({
  if (breederStatus()=="game master"){
    list(
      shinydashboard::tabBox(width=12, title = "Admin", id = "admin_tabset", side="left",
                             tabPanel("Manage sessions",
                                      div(style="margin-bottom:50px;",
                                        h3("Current sessions:"),
                                        tableOutput("sessionsTable")
                                      ),

                                      div(# add New session
                                        div(style="margin-bottom: 20px;",# inputs
                                          h3("Add a new session:"),
                                          div(style="display: inline-block; vertical-align:top; width: 33%; min-width:300px;", #start
                                            h4("Start"),
                                            tags$table(style = "width: 300px; border-collapse: collapse;", # start table 1
                                              tags$td(style = "width: 34%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                      dateInput("startDate", "date",
                                                                width = "100px")
                                              ),
                                              tags$td(style = "width: 33%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                      numericInput("startHour", "hour", value=9, min=0, max=23, step=1,
                                                                   width = "75px")
                                              ),
                                              tags$td(style = "width: 33%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                      numericInput("startMin", "minute", value=0, min=0, max=59, step=1,
                                                                   width = "75px")
                                              )
                                            )# end table 1
                                          ),# end div "start"

                                          div(style="display: inline-block; vertical-align:top; width: 33%; min-width:300px;", #end
                                            h4("End"),
                                            tags$table(style = "width: 300px; border-collapse: collapse;", # start table 2
                                              tags$td(style = "width: 34%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                      dateInput("endDate", "date",
                                                                width = "100px")
                                              ),
                                              tags$td(style = "width: 33%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                      numericInput("endHour", "hour", value=9, min=0, max=23, step=1,
                                                                   width = "75px")
                                              ),
                                              tags$td(style = "width: 33%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                      numericInput("endMin", "minute", value=0, min=0, max=59, step=1,
                                                                   width = "75px")
                                              )
                                            )# end table 2
                                          ),# end div "end"

                                          div(style="display: inline-block; vertical-align:top; width:33%; min-width:300px;", #year time
                                              h4("Year time"),
                                              tags$table(style = "border-collapse: collapse;", # start table 3
                                               tags$td(style = "width: 33%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                       numericInput("yearTime", "Duration of one year (in minutes)", value=60, min=0, max=Inf, step=1)
                                               )
                                              )# end table 3

                                          )# end div "year time"
                                        ),# end div inputs

                                        div(style="display: inline-block; vertical-align:top; width:25%; margin-bottom: 50px; padding-left: 10px;", #button
                                            actionButton("addSession", "Add this new session")

                                        )# end div "button"


                                      ),# end div "add New session"



                                      div(style = "margin-bottom:100px;",# delete session
                                        h3("Delete sessions:"),
                                        tags$table(style = "width: 100%; border-collapse: collapse;",
                                                   tags$td(style = "width: 50%; vertical-align: bottom; padding: 10px;",
                                                           selectInput("delSession", "Session's number",choices=c("",sessionsList()$num),
                                                                       selected="", width="100%")
                                                   ),
                                                   tags$td(style = "width: 50%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                           actionButton("deleteSession", "DO NOT click! (unless you are sure to delete this session)",
                                                                        width="100%", style="margin-bottom: 0px;",
                                                                        style="background-color:#ff3333; color:white;")
                                                   )
                                        )
                                      )# end div "delete session"
                             ),# end tabPanel sessions managment






                             tabPanel("Manage breeders",
                                      div(# add New breeders
                                        h3("Add a new breeder:"),
                                        tags$head(
                                          tags$style(HTML('.shiny-input-container{margin-bottom: 0px;}
                                                          .selectize-control{margin-bottom: 0px;}')
                                          )
                                          ),
                                        tags$table(style = "width: 100%; border-collapse: collapse;",
                                                   tags$tr(
                                                     tags$td(style = "width: 25%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                             textInput("newBreederName", "Breeder's name", placeholder="spaces forbidden",
                                                                       width="100%")
                                                     ),
                                                     tags$td(style = "width: 25%; vertical-align: bottom; padding: 10px;",
                                                             selectInput("newBreederStatus", "Status", choices=c("player","tester","game master"),
                                                                         width="100%")
                                                     ),
                                                     tags$td(style = "width: 25%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                             passwordInput("newBreederPsw", "Password",
                                                                           width="100%")
                                                     ),
                                                     tags$td(style = "width: 25%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                             actionButton("addNewBreeder", "Add this new breeder",
                                                                          width="100%", style="margin-bottom: 0px;")
                                                     )
                                                   )
                                        )# end tags$table
                                        ),# end div "add new breeder"


                                      div(# delete breeders
                                        h3("Delete a breeder:"),
                                        tags$table(style = "width: 100%; border-collapse: collapse;",
                                                   tags$td(style = "width: 50%; vertical-align: bottom; padding: 10px;",
                                                           selectInput("delBreederName", "Breeder's name",choices=c("",breederList()),
                                                                       selected="", width="100%")
                                                   ),
                                                   tags$td(style = "width: 50%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                                                           actionButton("deleteBreeder", "DO NOT click! (unless you are sure to delete this breeder)",
                                                                        width="100%", style="margin-bottom: 0px;",
                                                                        style="background-color:#ff3333; color:white;")
                                                   )
                                        )
                                      )# end div "delete breeders"
                             ),# end tabPanel "Add/Delete Breeders"





                             tabPanel("Disk usage",
                                      div(
                                        h3("Disk usage:"),
                                        tableOutput("sizeDataFolder")
                                      )

                             )# end tabPanel "Disk usage"
      )# close tabBox
    ) # close list


  }else{
    shinydashboard::box(width=12, title = "Content unavailable",
                        div(p("Sorry, this is only accessible to the game master."))
    )
  }
})



## add new breeder:
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

## delete breeder:
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



## sessions managment
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
  browser()
  startDate <- strptime(paste0(input$startDate," ", input$startHour,":",input$startMin),
                        format="%Y-%m-%d %H:%M")
  endDate <- strptime(paste0(input$endDate," ", input$endHour,":",input$endMin),
                        format="%Y-%m-%d %H:%M")
  
  if (startDate < endDate){
      #calculate in number:
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
      return(res)

  }else{
    showNotification("Error: Start date must be earlier than end date", type = c("error"))
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








## Disk usage:
output$sizeDataFolder <- renderTable({
  # data frame containing the size of all subfolder of "data"
  invalidateLater(30000)
  withProgress({
    if (breederStatus()=="game master"){
      folderShared <- list.dirs(path = "data/shared", full.names = TRUE, recursive = TRUE)[-1]
      folderTruth <- list.dirs(path = "data/truth", full.names = TRUE, recursive = TRUE)[-1]
      subFolders <- c(folderShared,folderTruth)

      funApply <- function(folder){
        files <- list.files(folder, all.files = TRUE, recursive = TRUE, full.names=T)
        sum(file.info(files)$size)
      }
      infoDataFolder <- as.data.frame(sapply(subFolders, FUN=funApply,USE.NAMES = FALSE),
                                      col.names=c("size"))
      names(infoDataFolder) <- c("size")
      infoDataFolder$path <- subFolders


      infoDataFolder <- rbind(infoDataFolder,
                              data.frame(path="data/shared",
                                         size=sum(infoDataFolder$size[infoDataFolder$path %in% folderShared]))
      )
      infoDataFolder <- rbind(infoDataFolder,
                              data.frame(path="data/truth",
                                         size=sum(infoDataFolder$size[infoDataFolder$path %in% folderTruth]))
      )
      infoDataFolder <- rbind(infoDataFolder,
                              data.frame(path="data/breeding-game.sqlite",
                                         size=file.info("data/breeding-game.sqlite")$size)
      )
      infoDataFolder <- rbind(infoDataFolder,
                              data.frame(path="data",
                                         size=sum(infoDataFolder$size))
      )
      infoDataFolder <- infoDataFolder[order(infoDataFolder$size, decreasing = T),]


      infoDataFolder$size <- infoDataFolder$size/10^6
      infoDataFolder <- rev(infoDataFolder)
      names(infoDataFolder) <- c("path", "size (Mo)")

      return(infoDataFolder)
    }else return(NULL)
  },message = "Calculating... Please wait.")
})










