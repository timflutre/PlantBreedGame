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



## server for "identification"

## Function
source("src/func_id.R", local=TRUE, encoding = "UTF-8")$value




## get breeder list and create select input
output$selectBreeder <- renderUI({
  # get the breeders list
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  tbl <- "breeders"
  query <- paste0("SELECT name FROM ", tbl)
  breederNames <- dbGetQuery(conn=db, query)[,1]
  dbDisconnect(db)
  
  selectInput("breederName", "Breeder", choices=as.list(breederNames))
  
})



## log in
goodPsw <- eventReactive(input$submitPSW,
                         ignoreNULL=FALSE,{

  if (input$submitPSW==0){
    return(FALSE)
  } else{
    db <- dbConnect(SQLite(), dbname=setup$dbname)
    tbl <- "breeders"
    query <- paste0("SELECT h_psw FROM ", tbl, " WHERE name = '", input$breederName,"'")
    hashPsw <- dbGetQuery(conn=db, query)[,1]
    dbDisconnect(db)
    if (hashPsw==digest(input$psw, "md5", serialize = FALSE)){
      removeUI("#logInDiv")
      return (TRUE)
    }else return(FALSE)
  }
})


breeder <- reactive({
  if (goodPsw()){
    input$breederName
  }else {"No Identification"}

})

breederStatus <- reactive({
  if (goodPsw()){
    db <- dbConnect(SQLite(), dbname=setup$dbname)
    tbl <- "breeders"
    query <- paste0("SELECT status FROM ", tbl, " WHERE name = '", input$breederName,"'")
    status <- dbGetQuery(conn=db, query)[,1]
    dbDisconnect(db)
    return(status)
  }else {return("No Identification")}

})

budget <- reactive({
  input$leftMenu
  input$requestGeno
  if (breeder()!="No Identification"){

    db <- dbConnect(SQLite(), dbname=setup$dbname)
    tbl <- "log"
    query <- paste0("SELECT * FROM ", tbl, " WHERE breeder='", breeder(), "'")
    res <- dbGetQuery(conn=db, query)
    dbDisconnect(db)

    if (nrow(res)>0){
      funApply <- function(x){
        prices[x[3]][[1]]*as.integer(x[4])
      }
      expenses <- sum(apply(res, MARGIN = 1, FUN = funApply))
    }else{expenses <- 0}

    iniTialBuget <- constants$cost.pheno.field*constants$nb.plots*10*1.3
    return(round(iniTialBuget-expenses,2))

  }
})



## breeder menu
output$userAction <- renderUI({
  if(goodPsw()){
    shinydashboard::tabBox(width=12, title =paste0("My account"),
                           tabPanel("My files",
                                    div(style="display: inline-block; vertical-align:top; width: 50%;",
                                      div(
                                        h3("Phenotyping Data:"),
                                        selectInput("phenoFile", "", choices=phenoFiles(),width="75%"),
                                        uiOutput("UIdwnlPheno")
                                      ),
                                      div(
                                        h3("Genotyping Data:"),
                                        selectInput("genoFile", "", choices=genoFiles(),width="75%"),
                                        uiOutput("UIdwnlGeno")
                                      )
                                    ),
                                    div(style="display: inline-block; vertical-align:top; width: 49%;",
                                      div(
                                        h3("Plant material Data:"),
                                        selectInput("pltMatFile", "", choices=pltMatFiles(),width="75%"),
                                        uiOutput("UIdwnlPltMat")
                                      ),
                                      div(
                                        h3("My requests:"),
                                        selectInput("requestFile", "", choices=requestFiles(),width="75%"),
                                        uiOutput("UIdwnlRequest")
                                      )
                                    )
                                    

                           ),

                           tabPanel("My plant material",
                                    dataTableOutput("myPltMatDT")
                           ),

                           tabPanel("Change my password",
                                    div(style="display: inline-block; vertical-align:top;  width: 30%; min-height: 100%;",
                                      passwordInput("prevPsw", "Previous Password")
                                    ),
                                    div(style="display: inline-block; vertical-align:top;   min-width: 5%; min-height:: 100%;",
                                        p()
                                    ),
                                    div(style="display: inline-block; vertical-align:top;  width: 30%; min-height:: 100%;",
                                        passwordInput("newPsw", "New Password")
                                    ),
                                    div(style="display: inline-block; vertical-align: top;   min-width: 100%;",
                                        tags$head(
                                          tags$style(HTML('#changePsw{background-color:gray; color: white}'))
                                        ),
                                        actionButton("changePsw", "Change my password ! ")
                                    ),
                                    div(style="vertical-align: top;   min-width: 20%;", id="id_4",
                                        uiOutput("UIpswChanged")
                                    )
                           ),
                           tabPanel("Admin",
                                    uiOutput("UIadmin")
                           )
                           

          )
  }
})




## download files
# list of avaiable files (this must be reactive value to be refresh)
phenoFiles <- reactive({
  input$leftMenu
  getDataFileList(type="pheno", breeder=breeder())
})
genoFiles <- reactive({
  input$leftMenu
  choices=getDataFileList(type="geno", breeder=breeder())
})
pltMatFiles <- reactive({
  input$leftMenu
  choices=getDataFileList(type="pltMat", breeder=breeder())
})
requestFiles <- reactive({
  input$leftMenu
  choices=getDataFileList(type="request", breeder=breeder())
})


# dwnl buttons
output$dwnlPheno <- downloadHandler(
  filename=function () input$phenoFile, # lambda function
  content=function(file){
    filePath <- paste0("data/shared/",breeder(), "/",input$phenoFile)
    write.table(read.table(filePath, header = T),
                file=gzfile(file), quote=FALSE,
                sep="\t", row.names=FALSE, col.names=TRUE)
  }
)

output$dwnlGeno <- downloadHandler(
  filename=function () input$genoFile, # lambda function
  content=function(file){
    filePath <- paste0("data/shared/",breeder(), "/",input$genoFile)
    write.table(read.table(filePath, header = T),
                file=gzfile(file), quote=FALSE,
                sep="\t", row.names=TRUE, col.names=TRUE)
  }
)

output$dwnlPltMat <- downloadHandler(
  filename=function () input$pltMatFile, # lambda function
  content=function(file){
    filePath <- paste0("data/shared/",breeder(), "/",input$pltMatFile)
    write.table(read.table(filePath, sep="\t", header = T),
                file=file, quote=FALSE,
                sep="\t", row.names=FALSE, col.names=TRUE)
  }
)


output$dwnlRequest <- downloadHandler(
  filename=function () input$requestFile, # lambda function
  content=function(file){
    filePath <- paste0("data/shared/",breeder(), "/",input$requestFile)
    write.table(read.table(filePath, sep="\t", header = T),
                file=file, quote=FALSE,
                sep="\t", row.names=FALSE, col.names=TRUE)
  }
)

# UI of dwnl buttons
output$UIdwnlPheno <- renderUI({
  if (input$phenoFile!=""){
    if (breederStatus()=="player" && !availToDwnld(input$phenoFile,currentGTime())$isAvailable ){
      p(paste0("Sorry, your data are not available yet. Delivery date: ",
               availToDwnld(input$phenoFile,currentGTime())$availDate))
    }else{
      downloadButton("dwnlPheno", "Download your file")
    }

  }else p("No file selected.")

})

output$UIdwnlGeno <- renderUI({
  if (input$genoFile!=""){
    if (breederStatus()=="player" && !availToDwnld(input$genoFile,currentGTime())$isAvailable ){
      p(paste0("Sorry, your data are not available yet. Delivery date: ",
               availToDwnld(input$genoFile,currentGTime())$availDate))
    }else{
           downloadButton("dwnlGeno", "Download your file")
      
    }
  }else p("No file selected.")
})

output$UIdwnlPltMat <- renderUI({
  if (input$pltMatFile!=""){
    downloadButton("dwnlPltMat", "Download your file")
  }else p("No file selected.")
  
})

output$UIdwnlRequest <- renderUI({
  if (input$requestFile!=""){
    downloadButton("dwnlRequest", "Download your file")
  }else p("No file selected.")
  
})


## My plant-material
myPltMat <- reactive({
  if (input$leftMenu=="id"){
    db <- dbConnect(SQLite(), dbname=setup$dbname)
    tbl <- paste0("plant_material_", breeder())
    stopifnot(tbl %in% dbListTables(db))
    query <- paste0("SELECT * FROM ", tbl)
    res <- dbGetQuery(conn=db, query)
    # disconnect db
    dbDisconnect(db)
    res$avail_from <- strftime(res$avail_from, format= "%Y-%m-%d")
    res
  }
})

output$myPltMatDT <- renderDataTable(myPltMat(),
                                     searchDelay = 500,
                                     options = list(lengthMenu = c(10, 20, 50), pageLength = 10))







## Change Password
pswChanged <- eventReactive(input$"changePsw", {
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  tbl <- "breeders"
  query <- paste0("SELECT h_psw FROM ", tbl, " WHERE name = '", input$breederName,"'")
  hashPsw <- dbGetQuery(conn=db, query)[,1]
  dbDisconnect(db)
  if (digest(input$prevPsw, "md5", serialize = FALSE)==hashPsw){
    newHashed <- digest(input$newPsw, "md5", serialize = FALSE)

    db <- dbConnect(SQLite(), dbname=setup$dbname)
    tbl <- "breeders"
    query <- paste0("UPDATE ", tbl, " SET h_psw = '", newHashed,"' WHERE name = '", breeder(), "'" )
    dbExecute(conn=db, query)
    dbDisconnect(db)

    return(TRUE)

  }else {return(FALSE)}

})

output$UIpswChanged <- renderUI({
  if(pswChanged()){
    p("Password Updated")
  } else if (!pswChanged()){
    p("Wrong password, try again")
  }

})




## memory information:
output$sizeDataFolder <- renderTable({
  # data frame containing the size of all subfolder of "data"
  invalidateLater(30000)
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
})
  
## add new breeder:
observeEvent(input$addNewBreeder,{
  
  progressNewBreeder <- shiny::Progress$new(session, min=0, max=7)
  progressNewBreeder$set(value = 0,
                   message = "Adding breeder",
                   detail = "Initialisation...")
  
  addNewBreeder(input$newBreederName,
                input$newBreederStatus,
                input$newBreederPsw,
                progressNewBreeder)
  
  progressNewBreeder$set(value = 7,
                         detail = "Done !")
})

output$UIadmin <- renderUI({
  
  if (breederStatus()=="game master"){
    list(
      div(style="display: inline-block; vertical-align: top; width: 30%;",
          h3("Disk usage:"),
          tableOutput("sizeDataFolder")),
      
      tags$head(
          tags$style(HTML('.shiny-input-container{margin-bottom: 0px;}
                          .selectize-control{margin-bottom: 0px;}'))
        ),

      div(style="display: inline-block; vertical-align: top; width: 69%;",
          h3("Add new breeder:"),
          tags$table(style = "width: 100%; border-collapse: collapse;",
              tags$tr(
                tags$td(style = "width: 25%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                        textInput("newBreederName", "Breeder's name",
                                  width="100%")
                        ),
                tags$td(style = "width: 25%; vertical-align: bottom; padding: 10px;",
                        selectInput("newBreederStatus", "Status", choices=c("player", "game master"),
                                    width="100%")
                        ),
                tags$td(style = "width: 25%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                        passwordInput("newBreederPsw", "Password",
                                      width="100%")
                        ),
                tags$td(style = "width: 25%; vertical-align: bottom; padding: 10px; padding-bottom: 13.8px;",
                        actionButton("addNewBreeder", "Add Breeder",
                                     width="100%", style="margin-bottom: 0px;")
                        )
                )
          )
      )

    )
    
  }else return(p("Sorry, this is only accessible to the game master."))

})





## Breeder information :
output$breederBoxID <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = "Breeder",
    icon = icon("user-o"),
    color = "yellow"
  )
})

output$dateBoxID <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format= "%d %B %Y"),
    icon = icon("calendar"),
    color = "yellow"
  )
})



output$budgetBoxID <- renderValueBox({
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow"
  )
})

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
  if (breeder()!="No Identification"){
    list(infoBoxOutput("breederBoxID", width = 3),
         infoBoxOutput("dateBoxID", width = 3),
         infoBoxOutput("budgetBoxID", width = 3),
         infoBoxOutput("serverIndicID", width = 3))
  }

})









# DEBUG

output$IdDebug <- renderPrint({
  print("----")
  print(input$phenoFile)
  print(input$genoFile)
})
