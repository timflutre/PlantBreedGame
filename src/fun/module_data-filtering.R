
# this file list all the game initialisation parameters (implemented in the
# application)
# It takes the forms of "shiny modules" to:
#   - include the "input validation" in the module
#   - have more complex UI that provides informations to users based on several
#     "groups of inputs" (eg. all inputs related to budgets)
#   - lighten the code related to "Admin server".

# in the "server" parts of these modules, the `iv` argument is an
# `InputValidator` object

library(shinyvalidate)
library(shiny)
library(plotly)


individual_filtering_ui <- function(id, breeder) {
  ns <- NS(id)
  div(
    selectInput(
      ns("pheno_inds"),
      "Individuals",
      choices = c(
        "All",
        db_get_game_requests(breeder = breeder, type = "pltmat")$name,
        "Controls"
      ),
      width = "75%"),
  )
}

individual_filtering_server <- function(id, breeder) {
  moduleServer(id, function(input, output, session) {
    return(
      list(
        inds_ids = reactive({
          ind_filter <- input$pheno_inds
          if (is.null(ind_filter)) {
            return(NULL)
          }

          inds_ids <- NULL
          if (ind_filter == "Controls") {
            inds_ids <- db_get_individual(
              breeder = breeder,
              control = TRUE
            )$id
          } else  if (ind_filter != "All") {
            inds_ids <- db_get_individual(
              breeder = breeder,
              request_name = ind_filter
            )$id
          }
          inds_ids
        })
      )
    )

  })
}



phenotype_filtering_ui <- function(id, breeder) {
  ns <- NS(id)
  pheno_summary <- db_get_pheno_summary(breeder)
  div(
    selectizeInput(
      ns("pheno_requests"),
      "Phenotype requests",
      choices = db_get_game_requests(breeder = breeder, type = "pheno")$name,
      multiple = TRUE,
      width = "75%",
      options = list(
        placeholder = "All"
      )
    ),

    selectizeInput(
      ns("pheno_year"),
      "Phenotype year",
      choices = seq(pheno_summary$minYear, pheno_summary$maxYear),
      multiple = TRUE,
      width = "75%",
      options = list(
        placeholder = "All"
      )
    ),

    selectInput(
      ns("pheno_pahtogen"),
      "Pathogen",
      choices = c("All", TRUE, FALSE),
      multiple = FALSE,
      width = "75%"
    ),
  )
}

phenotype_filtering_server <- function(id, breeder) {
  moduleServer(id, function(input, output, session) {
    return(
      list(
        pheno_request = reactive({
          input$pheno_requests
        }),
        pathogen = reactive({
          if (is.null(input$pheno_pahtogen)) {
            return(NULL)
          }
          if (input$pheno_pahtogen != "All") {
            return(input$pheno_pahtogen)
          }
          return(NULL)
        }),
        years = reactive({
          input$pheno_year
        })
      )
    )
  })
}




