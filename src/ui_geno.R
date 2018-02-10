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


# UI of "geno" part


tabItem(tabName="geno",
        fluidRow(
          useShinyjs(),
          tags$script("Shiny.addCustomMessageHandler(
                                    'resetValue',function(variableName){
                                    Shiny.onInputChange(variableName, null);});"
                      ),
          uiOutput("UIbreederInfoGeno"),

          shinydashboard::box(width=12, title = "Choose an instruction file for genotyping:",
                              div( id="geno_file",
                                   fileInput(inputId="file.geno",
                                             label = NULL,
                                             multiple=FALSE,
                                             accept=c(".txt", ".tsv"))
                              )
          ),



          shinydashboard::tabBox(width=12, title = "Info", id = "geno_tabset", side="right", selected = "Check",
                                 tabPanel("Request",
                                          div(
                                            tags$head(
                                              tags$style(HTML('#requestGeno{background-color:#00A65A; color: white}'))
                                              ),
                                            p("Do you really want these genotyping data?"),
                                            actionButton("requestGeno", "Yes, I do!")
                                          ),
                                          div(
                                            uiOutput("genoRequestResultUI"))
                                          ),

                                 tabPanel("Data",
                                          dataTableOutput(outputId="qryGeno")),

                                 tabPanel("Summary",
                                          tableOutput("GenoInvoice")),
                                          # verbatimTextOutput("GenoSmy"),
                                          # verbatimTextOutput("GenoStr")),

                                 tabPanel("Check",
                                          verbatimTextOutput("GenoUploaded"))

          ),
          if (debugDisplay){
            shinydashboard::box(width=12, title = "Debug",
                                verbatimTextOutput("GenoDebug")
            )
          }


        ) # close fluidRow
) # close tabItem







