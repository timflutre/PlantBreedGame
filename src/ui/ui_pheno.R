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
div(
  uiOutput("UIbreederInfoPheno"),
  shinydashboard::box(
    width = 12, title = "Request phenotyping",
    div(
      id = "pheno_info1",
      p("In this module, you can request phenotyping data."),
      p(
        "One experimental site, Agrom-sur-Lez (AZ), is available with ", strong(constants_ui("pheno_nb.plots"), " plots."),
        " Planting a plot should be requested ", strong("before ", constants_ui("pheno_max.upload.pheno.field")), ".",
        " The data are then available ", constants_ui("pheno_duration.pheno.field"), " months after, on ", strong(constants_ui("pheno_pheno.data.availability.date")), ".",
        " The cost of a single plot (seeding, phenotyping of the three traits and harvesting) is ", strong(constants_ui("pheno_cost.pheno.field"), " Mendels.")
      ),
      p("A", strong("greenhouse"), " can also be used all year long to assess the resistance to", HTML("<em>P.psychedelica.</em>"), "This request takes ", strong(constants_ui("pheno_duration.pheno.patho"), "months"), " and costs ", strong(constants_ui("pheno_cost.pheno.patho.mendels"), " Mendels"), " (which represents", constants_ui("pheno_cost.pheno.patho"), " plot).")
    ),
    div(
      id = "pheno_info2",
      p("The request file for this module should be similar to the following example:"),
      tags$pre(HTML("<table>
                        <tr>
                        <td>ind\t</td>
                        <td>task\t</td>
                        <td>details\t</td>
                        </tr>
                        <tr>
                        <td>Coll0001\t</td>
                        <td>pheno-field\t</td>
                        <td>3\t</td>
                        </tr>
                        <tr>
                        <td>Coll0001\t</td>
                        <td>pheno-patho\t</td>
                        <td>1\t</td>
                        </tr>
                        </table>")),
      p(tags$ul(
        tags$li("The file should be in", code(".txt"), "format with", strong("tabulations"), "separator and ", strong(code("UTF-8"), "encoding.")),
        tags$li("All columns (", code("ind"), ", ", code("task"), ", and ", code("details"), ") are compulsory."),
        tags$li("The ", code("task"), " column should contain 'pheno-field' (for experimental site phenotyping) or 'pheno-patho' (for greenhouse phenotyping)"),
        tags$li("If 'task=pheno-field', the ", code("details"), " column should contain the number of plots (the total number of requested plots should not exceed the total available:", strong(constants_ui("pheno_nb.plots_2"), " plots."), ")"),
        # WIP ---- blocked here as `outputs` can only be used once ! ><
        tags$li("If 'task=pheno-patho', the ", code("details"), " column should contain the number of replicates"),
        tags$li("Individuals should be available."),
        tags$li("Individuals should not be duplicated within each task."),
        tags$li("Lines starting with ", code("#"), " will be ignored.")
      ))
    )
  ),
  shinydashboard::box(
    width = 12, title = "Choose an instruction file for phenotyping:",
    div(
      id = "pheno_file",
      uiOutput("idMessagePheno"),
      fileInput(
        inputId = "file.pheno",
        label = NULL,
        multiple = FALSE,
        accept = c(".txt", ".tsv")
      )
    )
  ),
  shinydashboard::tabBox(
    width = 12, title = "Info", id = "pheno_tabset", side = "right", selected = "Check",
    tabPanel(
      "Request",
      div(
        uiOutput("submitPhenoRequest")
      ),
      div(
        uiOutput("phenoRequestResultUI")
      )
    ),
    tabPanel(
      "Data",
      dataTableOutput(outputId = "qryPheno")
    ),
    tabPanel(
      "Summary",
      tableOutput("PhenoInvoice")
    ),
    tabPanel(
      "Check",
      verbatimTextOutput("PhenoUploaded")
    )
  )
)
