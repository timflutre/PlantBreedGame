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


##### File information #####
## This file contain the UI code for logged user for the admin tab
## this file is sourced in "server_admin.R" in a renderUI() function
############################

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


    tabPanel("Manage constants",
             # tabPanel to manage some game constants

             # see.year.effct
             div(id="admin_seedYearEffect",
                 style="margin: 0px 0px 40px 0px;",

                 # input:
                 div(id="admin_div_numInput_seedYearEfect",
                     style="display: inline-block;
                     vertical-align: top;",
                     numericInput("admin_seedYearEfect", "seed.year.effect",
                                  value = 4321,
                                  min = 0,
                                  max=NA,
                                  step = 1)
                 ),

                 # button to request update:
                 div(id="admin_div_button_seedYearEfect",
                     style="display: inline-block;
                     vertical-align: top;
                     padding-top: 25px", # button align with numInput
                     actionButton("admin_button_seedYearEfect", "update seed.year.effect")
                 ),

                 # current value:
                 div(id="admin_currentSYE",
                 "Current", code("seed.year.effect"),":", textOutput("admin_currentSYE", container=span)
                 )
             ) # end div "admin_seedYearEffect"


    ), # end tabPanel "Manage constants"







    tabPanel("Disk usage",
             div(id="admin_diskU_data",
                 style="display: inline-block;
                 vertical-align:top;
                 width: 33%;
                 min-width:300px;",
                 h3("Disk usage:"),
                 tableOutput("sizeDataFolder")
             ),
             div(id="admin_diskU_input",
                 style="display: inline-block;
                 vertical-align:top;
                 width: 66%;",
                 p("To prevent over disk usage on your server, you can specifiy here the maximum size for all game data.",
                   style="margin-top:20px;"),
                 p("In case the size of all data exceeds this threshold, players will not be allowed to connect any more, and you will have to delete haplotypes of some breeders."),
                 p(textOutput("InfoCurrentMaxDiskUsage")),
                 div(style="width: 50%;
                     display: inline-block;
                     vertical-align: top;",
                     numericInput("admin_maxDiskUsage",
                                  label= "Maximum disk usage (in Gb)",
                                  value= 10,
                                  min = 2)
                 ),
                 div(style="width: 30%;
                     padding-top: 26px;
                     display: inline-block;
                     vertical-align: top;",
                     actionButton("updateMaxDiskUsage",
                                  label="Update")
                 )


             ) # end div "admin_diskU_input"
    ),# end tabPanel "Disk usage"



#----- Game progress -----
# This tab displays the progression of the players.
    tabPanel("Game progress",
             fluidRow(
                 div(class = "col-sm-12 col-md-12 col-lg-12",

                     selectInput(inputId = "admin_progressTrait",
                                 label = "Trait",
                                 choices = c("Trait 1", "Trait 2"),
                                 selected = "Trait 1"),


                     actionButton(inputId = "admin_progressButton",
                                  label = "Refresh !",
                                  icon = icon("refresh"),
                                  style = "background-color: #00a65a;
                                      color: #ffffff;")
                 ),
                 div(class = "col-sm-12 col-md-12 col-lg-6",
                     plotlyOutput("admin_plotAllIndGameProgress") %>% withSpinner()
                 ),
                 div(class = "col-sm-12 col-md-12 col-lg-6",
                     plotlyOutput("admin_plotMaxIndGameProgress") %>% withSpinner()
                 )
             )# end fluidRow
    )# end tabPanel "Game progress"




    )# close tabBox
) # close list
