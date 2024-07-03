
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


gameInit_seed_ui <- function(id) {
  ns <- NS(id)
  numericInput(ns("seed"), "RNG seed", value = 1993, step = 1)
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
