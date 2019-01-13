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



# UI of "pheno" part


tabItem(tabName="pheno",
    fluidRow(
    useShinyjs(),
    uiOutput("UIbreederInfoPheno"),
    tags$script("Shiny.addCustomMessageHandler(
                              'resetValue',function(variableName){
                              Shiny.onInputChange(variableName, null);});"
                ),


    shinydashboard::box(width=12, title = "Choose an instruction file for phenotyping:",
                        div( id="pheno_file",
                             uiOutput("idMessagePheno"),
                             fileInput(inputId="file.pheno",
                                       label = NULL,
                                       multiple=FALSE,
                                       accept=c(".txt", ".tsv"))
                        )
    ),



    shinydashboard::tabBox(width=12, title = "Info", id = "pheno_tabset", side="right", selected = "Check",
                           tabPanel("Request",
                                      div(
                                        uiOutput("submitPhenoRequest")
                                      ),
                                      div(
                                        uiOutput("phenoRequestResultUI")
                                      )
                                    ),

                           tabPanel("Data",
                                    dataTableOutput(outputId="qryPheno")),
                           tabPanel("Summary",
                                    tableOutput("PhenoInvoice")),
                                    # verbatimTextOutput("PhenoSmy"),
                                    # verbatimTextOutput("PhenoStr")),

                           tabPanel("Check",
                                    verbatimTextOutput("PhenoUploaded"))

    ),
    if (debugDisplay){
      shinydashboard::box(width=12, title = "Debug",
                          verbatimTextOutput("PhenoDebug")
      )
    }
  ) # close fluidRow
) # close tabItem
