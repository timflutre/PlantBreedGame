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
library(shiny)

list(
  shinydashboard::tabBox(
    width = 12, title = "Data Visualisation", id = "data-viz", side = "left", selected = "From file",
    tabPanel(
      "From file",
      div(
        style = "",
        h4("Import file"),
        fileInput(
          inputId = "file_data_viz",
          label = tooltip_label(
            "Help about file format",
            div(
              p("Accepted format is the", strong("same as the phenotype data"), "provided by this game:"),
              tags$ul(
                tags$li("A", code(".txt"), "or", code(".tsv"), "file (or their gzip compression)"),
                tags$li("Field separator must be", strong("a tabulation")),
                tags$li("File must be encoded in UTF-8")
              )
            )
          ),
          multiple = FALSE,
          accept = c(".txt", ".tsv", ".txt.gz", ".tsv.gz"),
          width = "100%"
        ),
        div(
          style = "display: inline-block; vertical-align: top; margin-right: 10px;",
          selectInput("categ_variables",
            label = tooltip_label(
              "Categorical variables",
              div(
                p("Select here the categorical variables of your data-set.")
              )
            ),
            choices = list(),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block; vertical-align: top;",
          selectInput("quant_variables",
            label = tooltip_label(
              "Quantitative variables",
              div(
                p(
                  "This input is disable, any variable that is not",
                  "categorical is considered quantitative.",
                  "Please remove categorical variables to mark them as",
                  "quantitative."
                )
              )
            ),
            choices = list(),
            multiple = TRUE
          )
        ),
        hr()
      ),
      div(
        data_viz_ui("data-viz_file")
      )
    )
  )
)
