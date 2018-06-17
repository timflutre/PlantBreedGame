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



# UI of "identification" part


# UI
tabItem(tabName="id",
        fluidRow(
          uiOutput("UIbreederInfoID"),
          div( id = "logInDiv",
            shinydashboard::box(width=12, title = NULL,
                                div(style="display: inline-block; vertical-align:top;  width: 30%; min-height: 100%;", id="id_1",
                                    uiOutput("selectBreeder")
                                ),
                                div(style="display: inline-block; vertical-align:top;   min-width: 5%; min-height:: 100%;", id="id_2",
                                    br()
                                ),
                                div(style="display: inline-block; vertical-align:top;  width: 30%; min-height:: 100%;", id="id_3",
                                    passwordInput("psw", "Password")
                                ),
                                div(style="vertical-align: top;   min-width: 20%;", id="id_4",
                                    actionButton("submitPSW", "Log in", style="background-color:#00A65A; color: white")
                                ),
                                div(style="vertical-align: top; margin-top:10px; min-width: 20%;", id="id_5",
                                    p("Breeders can have differnts status:"),
                                    tags$ul(
                                        tags$li(strong("Game-Master:"), "Highest privilege. You have access to \"Admin\" and \"Evaluation\" tabs. Your data files are available without any time restriction."),
                                        tags$li(strong("Tester:"), "Is for people who want to test this game. You have access to \"Evaluation\" tab and your data files are available without any time restriction."),
                                        tags$li(strong("Player:"), "You have neither access to \"Admin\" nor \"Evaluation\" tabs. Your data file are under time restriction.")
                                    )
                                )

            )
          ),
          uiOutput("userAction"),
          if (debugDisplay){
            shinydashboard::box(width=12, title = "Debug",
                                verbatimTextOutput("IdDebug")
                                )
          }





      ) # close fluidRow
) # close tabItem






