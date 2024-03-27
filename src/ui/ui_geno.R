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


# UI of "geno" part


tabItem(
  tabName = "geno",
  fluidRow(
    useShinyjs(),
    tags$script("Shiny.addCustomMessageHandler(
                                    'resetValue',function(variableName){
                                    Shiny.onInputChange(variableName, null);});"),
    uiOutput("UIbreederInfoGeno"),
    shinydashboard::box(
      width = 12, title = "Request genotyping",
      div(
        id = "geno_info1",
        p("In this module you, can request genotyping data."),
        p("A laboratory can be used", strong("all year long"), " to perform genotyping. Two SNP chips are available:"),
        tags$ul(
          tags$li(strong("High-density"), ": ", constants$nb.snps.hd, " SNP, ", constants$duration.geno.hd, "-month delay and costs", constants$cost.geno.hd, " plot (", format(constants$cost.geno.hd * constants$cost.pheno.field, digits = 2), " Mendels )."),
          tags$li(strong("Low-density"), ": ", constants$nb.snps.ld, " SNP, ", constants$duration.geno.ld, "-month delay and costs", constants$cost.geno.ld, " plot (", format(constants$cost.geno.ld * constants$cost.pheno.field, digits = 2), " Mendels ).")
        ),
        p(strong("Single-SNP"), "genotyping can also be performed: ", constants$duration.geno.single, "-month delay and costs", constants$cost.geno.single, " plot (", format(constants$cost.geno.single * constants$cost.pheno.field, digits = 2), " Mendels ).")
      ),
      div(
        id = "geno_info2",
        p("The request file for this module should be similar to the following example:"),
        tags$pre(HTML("<table>
                        <tr>
                        <td>ind\t</td>
                        <td>task\t</td>
                        <td>details\t</td>
                        </tr>
                        <tr>
                        <td>Coll0001\t</td>
                        <td>geno\t</td>
                        <td>hd\t</td>
                        </tr>
                        <tr>
                        <td>Coll0002\t</td>
                        <td>geno\t</td>
                        <td>ld\t</td>
                        </tr>
                        <tr>
                        <td>Coll0003\t</td>
                        <td>geno\t</td>
                        <td>snp01877\t</td>
                        </tr>
                        </table>")),
        p(tags$ul(
          tags$li("The file should be in", code(".txt"), "format with", strong("tabulations"), "separator and ", strong(code("UTF-8"), "encoding.")),
          tags$li("All columns (", code("ind"), ", ", code("task"), " and ", code("details"), ") are compulsory."),
          tags$li("The ", code("task"), " column should contain 'geno'"),
          tags$li("The ", code("details"), " column should contain 'hd' (for a high-density chip), 'ld' (for a low-density chip) or the SNP identifier (for single SNP genotyping)."),
          tags$li("Individuals should be available."),
          tags$li("Individuals should not be duplicated within each task."),
          tags$li("Lines starting with ", code("#"), " will be ignored.")
        ))
      )
    ),
    shinydashboard::box(
      width = 12, title = "Choose an instruction file for genotyping:",
      div(
        id = "geno_file",
        uiOutput("idMessageGeno"),
        fileInput(
          inputId = "file.geno",
          label = NULL,
          multiple = FALSE,
          accept = c(".txt", ".tsv")
        )
      )
    ),
    shinydashboard::tabBox(
      width = 12, title = "Info", id = "geno_tabset", side = "right", selected = "Check",
      tabPanel(
        "Request",
        div(
          uiOutput("submitGenoRequest")
        ),
        div(
          uiOutput("genoRequestResultUI")
        )
      ),
      tabPanel(
        "Data",
        dataTableOutput(outputId = "qryGeno")
      ),
      tabPanel(
        "Summary",
        tableOutput("GenoInvoice")
      ),
      # verbatimTextOutput("GenoSmy"),
      # verbatimTextOutput("GenoStr")),

      tabPanel(
        "Check",
        verbatimTextOutput("GenoUploaded")
      )
    ),
    if (debugDisplay) {
      shinydashboard::box(
        width = 12, title = "Debug",
        verbatimTextOutput("GenoDebug")
      )
    }
  ) # close fluidRow
) # close tabItem
