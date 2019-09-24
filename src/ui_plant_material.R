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


# UI of "cross" part

tabItem(tabName="plant_mat",
        fluidRow(
        useShinyjs(),
        tags$script("Shiny.addCustomMessageHandler(
                                  'resetValue',function(variableName){
                                  Shiny.onInputChange(variableName, null);});"
                    ),

  uiOutput("UIbreederInfoPltMat"),

  shinydashboard::box(width = 12, title = "Request plant material",
    div(id = "cross_info1",
        p("In this module, you can request new plant materials."),
        p("A greenhouse can be used", strong("all year long"), " to perform crosses:"),
        tags$ul(
          tags$li(strong("Allofecundation"),": ", constants$duration.allof,"-month delay and costs", constants$cost.allof, " plot (", format(constants$cost.allof * constants$cost.pheno.field, digits = 2), " Mendels )."),
          tags$li(strong("Autofecundation"),": ", constants$duration.autof,"-month delay and costs", constants$cost.autof, " plot (", format(constants$cost.autof * constants$cost.pheno.field, digits = 2), " Mendels ).")
        ),
        p("A laboratory can also be used to perform ", strong("haplodiploidisation"), ". It has a ", constants$duration.haplodiplo, "-month delay, costs ", constants$cost.haplodiplo, " plot (", format(constants$cost.haplodiplo * constants$cost.pheno.field, digits=2), " Mendels ), and a maximum of ", constants$max.nb.haplodiplos, " can be requested at once.")
    ),

    div(id = "cross_info2",
        p("The request file for this module should be similar to the following example:"),
        tags$pre(HTML("<table>
                        <tr>
                        <td>parent1\t</td>
                        <td>parent2\t</td>
                        <td>child\t</td>
                        <td>explanations</td>
                        </tr>
                        <tr>
                        <td>Coll0001\t</td>
                        <td>Coll0002\t</td>
                        <td>0001-0002.1\t</td>
                        <td>allofecundation</td>
                        </tr>
                        <tr>
                        <td>Coll0001\t</td>
                        <td>Coll0001\t</td>
                        <td>0001-0001.1\t</td>
                        <td>autofecundation</td>
                        </tr>
                        <tr>
                        <td>Coll0001\t</td>
                        <td>\t</td>
                        <td>0001-HD.1\t</td>
                        <td>haplodiploidization</td>
                        </tr>
                        </table>")
                ),

        p(tags$ul(
          tags$li("The file should be in", code(".txt"), "format with", strong("tabulations"), "separator and ", strong(code("UTF-8"), "encoding.")),
          tags$li("Each row corresponds to a child."),
          tags$li("Only columns", code("parent1"),", ",code("parent2"),", and ", code("child"), " are compulsory."),
          tags$li("Only the ",code("parent2")," column can be empty (which means haplodiploidization request)."),
          tags$li("Individual names should only use ",code("[a-z], [A-Z], [0-9], [._-]")," (no space, comma, etc)."),
          tags$li("Individual names should be unique."),
          tags$li("Lines starting with ",code("#")," will be ignored.")

        ))
    )

  ),

  shinydashboard::box(width=12, title = "Choose an instruction file for plant material:",
                      div( id="cross_file",
                           uiOutput("idMessagePltMat"),
                           fileInput(inputId="file.plmat",
                                     label = NULL,
                                     multiple=FALSE,
                                     accept=c(".txt", ".tsv"))
                      )
  ),



  shinydashboard::tabBox(width=12, title = "Info", id = "cross_tabset", side="right", selected = "Check",
                 tabPanel("Request",
                            div(
                              uiOutput("submitPlmatRequest")
                            ),
                            div(
                              uiOutput("plmatRequestResultUI")
                            )
                          ),

                 tabPanel("Data",
                          dataTableOutput(outputId="qryPlmat")),
                 tabPanel("Summary",
                          tableOutput("PltmatInvoice")),
                          # verbatimTextOutput("plmatSmy"),
                          # verbatimTextOutput("plmatStr")),
                 tabPanel("Check",
                          verbatimTextOutput("plmatUploaded"))

  ),

  if (debugDisplay){
    shinydashboard::box(width=12, title = "Debug",
                        verbatimTextOutput("plmatDebug")
    )
  }


 ) # close fluidRow
) # close tabItem
