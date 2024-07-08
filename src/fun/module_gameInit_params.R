
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

tooltip_label <- function(label, description){

  tooltip <- tags$details(
    tags$summary(style = "cursor: pointer;",
     label, bsicons::bs_icon("question-circle")
    ),
    tags$blockquote(style = "font-weight: normal; font-size: inherit; font-style: italic;",
      description
    )
  )
}

gameInit_seed_ui <- function(id) {
  ns <- NS(id)

  label <- tooltip_label(
    "RNG seed",
    div(
      p("Random number generation seed."),
      p("A positive integer used as a seed for random generation.",
        "This ensures reproducibility of the game initialisation")
    )
  )
  div(
    # shiny::numericInput(ns("seed"), span("RNG seed", tooltip), value = 1993, step = 1)
    shiny::numericInput(ns("seed"), label = label, value = 1993, step = 1)
  )
}

gameInit_seed_server <- function(id, iv) {
  moduleServer(id, function(input, output, session) {

    iv$add_rule("seed", valid_rng_seed)
    return(
      list(
        value = reactive({
          if (is.null(iv$validate()[[session$ns('seed')]])) {
            return(input$seed)
          }
          return(NA)
        }),
        iv = iv
      )
    )

  })
}
