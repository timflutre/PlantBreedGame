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
## This file contain the UI code for logged user for the id tab
## this file is sourced in "server_id.R" in a renderUI() function
############################
div(
  uiOutput("UIbreederInfoID"),
  shinydashboard::tabBox(
    width = 12, title = paste0("My account"),
    tabPanel(
      "My files",
      div(
        style = "display: inline-block; vertical-align:top; width: 50%;",
        div(
          h3("Genotyping data:"),
          selectInput("genoFile", "", choices = genoData(), width = "75%"),
          uiOutput("UIdwnlGeno")
        )
      ),
      div(
        style = "display: inline-block; vertical-align:top; width: 49%;",
        div(
          h3("Other:"),
          p(
            "You can download here:",
            tags$ul(
              tags$li("SNP coordinates of the HD and LD chips"),
              tags$li("List of controls"),
              tags$li("Examples of requests files"),
              tags$li("List of your requests")
            )
          ),
          selectInput("requestFile", "", choices = requestFiles(), width = "75%"),
          uiOutput("UIdwnlRequest")
        )
      )
    ),

    tabPanel(
      "Phenotype data",
      h2("Phenotype data"),
      div(id = "pheno_info",
        p("Three phenotypic traits are investigated:"),
        tags$ul(
          tags$li(code("trait1"), ": flower production in kg/ha"),
          tags$li(code("trait2"), ": sepmetin content in g/kg"),
          tags$li(code("trait3"), ": presence of symptoms caused by P. psychedelica",
            tags$ul(
              tags$li(code("1"), "indicates the individual showed symptoms"),
              tags$li(code("0"), "indicates the individual did not show symptoms.")
            ),
            "Note: If", code("pathogen"), "is", code("FALSE"),
            ", the pathogen was not observed and therefore not any individuals will show  symptoms.")
        ),

        p("Additionally, the phenotypic data provides the following variables:"),
        tags$ul(
          tags$li(code("ind"), ": the individual name"),
          tags$li(code("control_ind"), ": boolean indicating if the individual will be used as control for the final evaluation"),
          tags$li(code("year"), ": the year when this phenotyping happens."),
          tags$li(code("plot"), ": the plot id of the phenotyping observation"),
          tags$li(code("pathogen"), ": boolean value indicating if the pathogen have been observed during the phenotyping."),
        )
      ),

      # h3("Summary"),
      div(id = "pheno_filters",
        h3("Filters"),
        p("Records matching", strong("all conditions"), "are shown."),
        individual_filtering_ui("pheno_download_ind_filter", breeder = breeder()),
        phenotype_filtering_ui("pheno_download_pheno_filter", breeder = breeder()),
        downloadButton("dwnlPheno_1", "Download")
      ),

      div(id = "pheno_preview_div",
        h3("Preview"),
        dataTableOutput("pheno_preview_DT")
      ),
      downloadButton("dwnlPheno_2", "Download"),
    ),

    tabPanel(
      "My plant material",
      h2("Plant material"),
      div(id = "inds_filters",
        h3("Filters"),
        p("Records matching", strong("all conditions"), "are shown."),
        individual_filtering_ui("inds_download_ind_filter", breeder = breeder()),
        downloadButton("dwnlInds_1", "Download")
      ),

      div(id = "inds_preview",
        h3("Preview"),
        dataTableOutput("plant_mat_preview")
      ),
      downloadButton("dwnlInds_2", "Download"),

      div(id = "inds_ind_info",
        uiOutput("selected_ind_info")
      ),
    ),

    tabPanel(
      "Change my password",
      div(
        style = "display: inline-block; vertical-align:top;  width: 30%; min-height: 100%;",
        passwordInput("prevPsw", "Previous Password")
      ),
      div(
        style = "display: inline-block; vertical-align:top;   min-width: 5%; min-height:: 100%;",
        p()
      ),
      div(
        style = "display: inline-block; vertical-align:top;  width: 30%; min-height:: 100%;",
        passwordInput("newPsw", "New Password")
      ),
      div(
        style = "display: inline-block; vertical-align: top;   min-width: 100%;",
        tags$head(
          tags$style(HTML("#changePsw{background-color:gray; color: white}"))
        ),
        actionButton("changePsw", "Change my password!")
      ),
      div(
        style = "vertical-align: top;   min-width: 20%;", id = "id_4",
        uiOutput("UIpswChanged")
      )
    ),
    tabPanel(
      "Register final individuals",
      div(
        style = "display: inline-block; vertical-align:top; width: 40%;",
        div(
          h3("Individuals submission:"),
          p("You can specify here the individuals you want to submit for the final evaluation."),
          p(
            "A maximum of ",
            strong(constants_ui("home_maxEvalInds"), "individuals"),
            "can be registered."
          ),
          p("The registration fee is ", strong(constants_ui("home_cost.register.mendels"), "Mendels"), "per genotype. No refund are possible, thank-you for your understanding.")
        ),
        div(
          selectInput("id_evalInds",
            HTML("Select your best individuals<sup>*</sup>:"),
            choices = myPltMat()$child,
            multiple = TRUE
          ),
          actionButton("id_submitInds", "Submit"),
          p(tags$sup("*"), "The drop-down menu is limited to 1000 propositions. Write the name of your individuals to find them.")
        )
      ),
      div(
        style = "display: inline-block; vertical-align:top; width: 50%;",
        div(
          h4("Your submitted individuals:"),
          dataTableOutput("submittedIndsDT"),
          p("Click on the individuals to delete them."),
          actionButton("id_delSubmitInds", "Delete")
        )
      ),
    )
  ) # end shinydashboard::tabBox
)
