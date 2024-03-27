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



# UI of "identification" part


# UI
tabItem(
  tabName = "id",
  fluidRow(
    uiOutput("UIbreederInfoID"),
    div(
      id = "logInDiv",
      shinydashboard::box(
        width = 12, title = NULL,
        div(
          style = "vertical-align: top; margin-top:10px; min-width: 20%;", id = "id_5",
          p("Depending on your status, you are granted with different permissions:"),
          tags$ul(
            tags$li(strong("game-master (such as breeder 'admin'):"), "has the highest privileges. Has access to the \"Admin\" and \"Evaluation\" tabs. Data files available without any time restriction."),
            tags$li(strong("tester (such as breeder 'test'):"), "used to test the game without needing a password. Has access to the \"Evaluation\" tab. Data files available without any time restriction."),
            tags$li(strong("player:"), "used when playing in a common session. Has access neither to the \"Admin\" nor \"Evaluation\" tabs. Data files available under time restriction.")
          )
        ),
        br(),
        div(
          style = "display: inline-block; vertical-align:top;  width: 30%; min-height: 100%;", id = "id_1",
          uiOutput("selectBreeder")
        ),
        div(
          style = "display: inline-block; vertical-align:top;   min-width: 5%; min-height:: 100%;", id = "id_2",
          br()
        ),
        div(
          style = "display: inline-block; vertical-align:top;  width: 30%; min-height:: 100%;", id = "id_3",
          passwordInput("psw", "Password")
        ),
        div(
          style = "vertical-align: top;   min-width: 20%;", id = "id_4",
          actionButton("submitPSW", "Log in", style = "background-color:#00A65A; color: white")
        )
      )
    ),
    uiOutput("userAction"),
    if (debugDisplay) {
      shinydashboard::box(
        width = 12, title = "Debug",
        verbatimTextOutput("IdDebug")
      )
    }
  ) # close fluidRow
) # close tabItem
