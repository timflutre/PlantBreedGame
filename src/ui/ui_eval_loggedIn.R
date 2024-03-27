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
## This file contain the UI code for logged user for the evaluation tab
## this file is sourced in "server_eval.R" in a renderUI() function
############################

list(
  # shinydashboard::box(width=12, title = "Choose an evaluation file:",
  #     div( id="eval_file",
  #          fileInput(inputId="file.eval",
  #                    label = NULL,
  #                    multiple=FALSE,
  #                    accept=c(".txt", ".tsv")),
  #          numericInput("nRep", "Choose the number of plot(s) per genotype:", 20, min = 1, max = 100),
  #          actionButton("requestEval", "Launch evaluation!")
  #     )
  # ),
  shinydashboard::box(
    width = 12, title = "Evaluation:",
    div(
      id = "eval_file",
      h4("Submitted individuals:"),
      dataTableOutput("evalFileDT"),
      numericInput("nRep", "Choose the number of plot(s) per genotype:", 20, min = 1, max = 100),
      actionButton("requestEval", "Launch evaluation!")
    )
  ),
  shinydashboard::tabBox(
    width = 12, title = "Graphs", id = "eval_graphs", side = "left", selected = "Trait 1",
    tabPanel(
      "Trait 1",
      div(
        plotlyOutput("evalGraphT1", height = "100%", width = "100%") %>% withSpinner()
      )
    ),
    tabPanel(
      "Trait 2",
      div(
        plotlyOutput("evalGraphT2", height = "100%", width = "100%") %>% withSpinner()
      )
    ),
    tabPanel(
      "Trait 3",
      div(
        p("1 = symptoms, 0 = no symptoms"),
        plotlyOutput("evalGraphT3", height = "100%", width = "100%") %>% withSpinner()
      )
    ),
    tabPanel(
      "Traits 1 vs 2",
      div(
        plotlyOutput("evalGraphT1vT2", height = "100%", width = "100%") %>% withSpinner()
      )
    ),
    tabPanel(
      "Pedigree",
      div(
        uiOutput("evalUIpedigree")
      )
    ),
    tabPanel(
      "AFs",
      div(
        uiOutput("evalUIAfsPlot")
      )
    ),
    tabPanel(
      "Additive relationships",
      div(
        uiOutput("evalUIaddRelation")
      )
    ),
    tabPanel(
      "Requests history",
      div(
        uiOutput("evalUIrequestHistory")
      )
    ),
    tabPanel(
      "Players' scores",
      div(
        uiOutput("evalUIgameScores")
      )
    ),
    tabPanel(
      "Download Report",
      p(
        "By clicking on the button below you can download",
        "a complete HTML report about this game session."
      ),
      p("(The first report generation can take a bit of time.)"),
      downloadButton("elvalReport")
    )
  ),

  # debug box
  if (debugDisplay) {
    shinydashboard::box(
      width = 12, title = "Debug",
      verbatimTextOutput("evalDebug")
    )
  }
) # end list
