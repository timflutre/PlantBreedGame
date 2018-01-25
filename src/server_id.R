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



## log in
goodPsw <- eventReactive(input$submitPSW, {
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  tbl <- "breeders"
  query <- paste0("SELECT h_psw FROM ", tbl, " WHERE name = '", input$breederName,"'")
  hashPsw <- dbGetQuery(conn=db, query)[,1]
  dbDisconnect(db)
  if (hashPsw==digest(input$psw, "md5", serialize = FALSE)){
    removeUI("#logInDiv")
    return (TRUE)
  }else return(FALSE)

})

breeder <- eventReactive(input$submitPSW,{
  if (goodPsw()){
    input$breederName
  }else {"No Identification"}

})

breederStatus <- eventReactive(input$submitPSW,{
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
    
    
    # return(constants$cost.pheno.field*300*10*1.3-expenses)
    return(0-expenses)
  }
})



## breeder menu
output$userAction <- renderUI({
  if(goodPsw()){
    shinydashboard::tabBox(width=12, title =paste0("My account"),
                           tabPanel("My files",
                                    div(
                                      h3("Phenotyping Data:"),
                                      selectInput("phenoFile", "", choices=phenoFiles()),
                                      uiOutput("UIdwnlPheno")
                                    ),
                                    div(
                                      h3("Genotyping Data:"),
                                      selectInput("genoFile", "", choices=genoFiles()),
                                      uiOutput("UIdwnlGeno")
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
                sep="\t", row.names=FALSE, col.names=TRUE)
  }
)

# UI of dwnl buttons
output$UIdwnlPheno <- renderUI({
  if (breederStatus()=="player" && !availToDwnld(input$phenoFile,currentGTime())$isAvailable ){
    p(paste0("Sorry, your data are not available yet. Delivery date: ",
             availToDwnld(input$phenoFile,currentGTime())$availDate))
  }else{
    downloadButton("dwnlPheno", "Download your file")
  }
})

output$UIdwnlGeno <- renderUI({
  if (breederStatus()=="player" && !availToDwnld(input$genoFile,currentGTime())$isAvailable ){
    p(paste0("Sorry, your data are not available yet. Delivery date: ",
             availToDwnld(input$genoFile,currentGTime())$availDate))
  }else{
    downloadButton("dwnlGeno", "Download your file")
  }
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

output$myPltMatDT <- renderDataTable(myPltMat(), searchDelay = 500)







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






## Breeder information :
output$breederBoxID <- renderValueBox({
  valueBox(
    value = breeder(),
    subtitle = "-",
    icon = icon("user-o"),
    color = "yellow",
    width = 4
  )
})

output$dateBoxID <- renderValueBox({
  valueBox(
    subtitle = "Date",
    value = strftime(currentGTime(), format= "%Y-%m-%d"),
    icon = icon("calendar"),
    color = "yellow",
    width = 4
  )
})



output$budgetBoxID <- renderValueBox({
  valueBox(
    value = budget(),
    subtitle = "Budget",
    icon = icon("credit-card"),
    color = "yellow",
    width = 4
  )
})

output$UIbreederInfoID <- renderUI({
  if (breeder()!="No Identification"){
    list(infoBoxOutput("breederBoxID"),
         infoBoxOutput("dateBoxID"),
         infoBoxOutput("budgetBoxID"))
  }

})









# DEBUG

output$IdDebug <- renderPrint({
  print("----")
  print(UIbreederInfo())
})
