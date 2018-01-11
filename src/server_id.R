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






goodPsw <- eventReactive(input$submitPSW, {
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  tbl <- "breeder_info"
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


output$UIchangePsw <- renderUI({
  if(goodPsw()){
    shinydashboard::box(width=12, title =paste0("Change my password"), 
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
  }
})


pswChanged <- eventReactive(input$"changePsw", {
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  tbl <- "breeder_info"
  query <- paste0("SELECT h_psw FROM ", tbl, " WHERE name = '", input$breederName,"'")
  hashPsw <- dbGetQuery(conn=db, query)[,1]
  dbDisconnect(db)
  if (digest(input$prevPsw, "md5", serialize = FALSE)==hashPsw){
    newHashed <- digest(input$newPsw, "md5", serialize = FALSE)
    
    db <- dbConnect(SQLite(), dbname=setup$dbname)
    tbl <- "breeder_info"
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





# Breeder information :
UIbreederInfo <- reactive({
  if(goodPsw()){
    shinydashboard::box(width=12, title =paste0("You are connected as \"", breeder(),"\""), status = "warning", solidHeader = TRUE,
                        p("Some information about the breeder...")
    )
  }
})

output$UIbreederInfo1 <- renderUI({UIbreederInfo()})
output$UIbreederInfo2 <- renderUI({UIbreederInfo()})
output$UIbreederInfo3 <- renderUI({UIbreederInfo()})
output$UIbreederInfo4 <- renderUI({UIbreederInfo()})
output$UIbreederInfo5 <- renderUI({UIbreederInfo()})
output$UIbreederInfo6 <- renderUI({UIbreederInfo()})








# DEBUG

output$IdDebug <- renderPrint({
  print("----")
  print(goodPsw())
  print(breeder())

})