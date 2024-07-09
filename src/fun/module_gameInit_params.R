
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

collapible_section <- function(title, title_id, content) {

  tags$details(style = "margin-top: 30px;",
    tags$summary(
      h3(title, "(click to expand)",
         style = "margin-top: 0px; display: inline-block;",
         id = title_id
      )
    ),
    content
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
    h3("", style = "margin-top: 30px;"),
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



gameInit_costs_ui <- function(id) {
  ns <- NS(id)


  width_numInput <- "90%"

  initBudget_label <- tooltip_label(
    "Initial budget",
    div(
      p("The budget is not limiting for players.",
        "If a player make a request without sufficient budget, the request",
        "will be processed and player's remaining budget will be negative."
      ),
      p('This value can be changed durring the game in "Manage constant" tab.')
    )
  )

  div(

    collapible_section(
      title = "Costs and budget:",
      title_id = ns("cost_budget_title"),
      div(
        tags$blockquote(style = "font-weight: normal; font-size: inherit; font-style: italic;",
          p("Note: Unless explicitly mentioned, all costs are expressed",
            strong("relative to the cost of phenotyping one plot"),
            ".")
        ),

        div(style = "display: flex;",
          div(style = "flex: 1;",
            h4("Phenotyping:", style = "margin-top: 20px;"),
            shiny::numericInput(ns("cost.pheno.field"), label = "Field phenotyping for 1 plot (in Mendels)", value = 50, step = 1, width = width_numInput),
            shiny::numericInput(ns("cost.pheno.patho"), label = 'Pathogene phenotyping', value = 0.1, step = 0.1, width = width_numInput),

            h4("Crossing:", style = "margin-top: 20px;"),
            shiny::numericInput(ns("cost.allof"), label = 'Allo-fecundation', value = 0.1, step = 0.1, width = width_numInput),
            shiny::numericInput(ns("cost.autof"), label = 'Auto-fecundation', value = 0.25, step = 0.1, width = width_numInput),
            shiny::numericInput(ns("cost.haplodiplo"), label = 'Haplo-diploidisation', value = 1, step = 0.1, width = width_numInput),

            h4("Genotyping:", style = "margin-top: 20px;"),
            shiny::numericInput(ns("cost.geno.hd"), label = 'Genotyping HD', value = 1, step = 0.1, width = width_numInput),
            shiny::numericInput(ns("cost.geno.ld"), label = 'Genotyping LD', value = 0.5, step = 0.1, width = width_numInput),
            shiny::numericInput(ns("cost.geno.single"), label = 'Genotyping single SNP', value = 0.02, step = 0.1, width = width_numInput),

            h4("Other:", style = "margin-top: 20px;"),
            shiny::numericInput(ns("cost.register"), label = 'Final evaluation registration', value = 4, step = 0.1, width = width_numInput),
            shiny::numericInput(ns("initialBudget"), label = initBudget_label, value = 3900, step = 100, width = width_numInput)
            # 3900 = 300 plots * 10 years + 30%
          ),

          div(style = "flex: 1;",
            h4("Cost and budget summary", style = "margin-top: 20px;"),
            tableOutput(ns("costs_table")),
            tags$blockquote(style = "font-weight: normal; font-size: inherit; font-style: italic;",
              textOutput(ns("init_budget_summary"))
            )
          )
        )
      )
    )
  )


}

gameInit_costs_server <- function(id, iv) {
  moduleServer(id, function(input, output, session) {

    output$costs_table <- renderTable({
      cost_df <- data.frame(
        Plot = c(
          1,
          input$cost.pheno.patho,
          input$cost.allof,
          input$cost.autof,
          input$cost.haplodiplo,
          input$cost.geno.hd,
          input$cost.geno.ld,
          input$cost.geno.single,
          input$cost.register,
          input$initialBudget
        ),
        row.names = c(
          "Phenotyping plot",
          "Phenotyping pathogene",
          "Allo-fecundation",
          "Auto-fecundation",
          "Haplo-diploidisation",
          "Genotyping HD",
          "Genotyping LD",
          "Genotyping single SNP",
          "Registration",
          "Initial Budget"
        )
      )
      cost_df$Mendels <- cost_df$Plot * input$cost.pheno.field
      return(cost_df)
    }, rownames = TRUE)

    output$init_budget_summary <- renderText({
      n_plot <- 300 # TODO used game init parameter instead
      n_year <- 10
      initB <- input$initialBudget

      bonus <- ((initB / (n_plot * n_year)) - 1) * 100

      paste0("Initial budget represents ",
        n_plot, " phenotyping plots for ",
        n_year, " years ",
        sprintf("%+.0f%%", bonus), "."
      )
    })

    cost_validator <- InputValidator$new()
    cost_validator$add_rule("cost.pheno.field", valid_positive_number)
    cost_validator$add_rule("cost.pheno.patho", valid_positive_number)
    cost_validator$add_rule("cost.allof", valid_positive_number)
    cost_validator$add_rule("cost.autof", valid_positive_number)
    cost_validator$add_rule("cost.haplodiplo", valid_positive_number)
    cost_validator$add_rule("cost.geno.hd", valid_positive_number)
    cost_validator$add_rule("cost.geno.ld", valid_positive_number)
    cost_validator$add_rule("cost.geno.single", valid_positive_number)
    cost_validator$add_rule("cost.register", valid_positive_number)
    cost_validator$add_rule("initialBudget", valid_positive_number)

    iv$add_validator(cost_validator)

    observe({
      id = session$ns('cost_budget_title')
      if (cost_validator$is_valid()) {
        # shinyjs::removeClass(id, "has-error") # not working in modules
        shinyjs::runjs(code = paste0('$("#', id, '").removeClass("has-error");'))
      } else {
        # shinyjs::addClass(id, "has-error") # not working in modules
        shinyjs::runjs(code = paste0('$("#', id, '").addClass("has-error");'))
      }
    })

    return(
      list(
        value = reactive({
          if (cost_validator$is_valid()) {
            return(list(
              cost.pheno.field = input$cost.pheno.field,
              cost.pheno.patho = input$cost.pheno.patho,
              cost.allof = input$cost.allof,
              cost.autof = input$cost.autof,
              cost.haplodiplo = input$cost.haplodiplo,
              cost.geno.hd = input$cost.geno.hd,
              cost.geno.ld = input$cost.geno.ld,
              cost.geno.single = input$cost.geno.single,
              cost.register = input$cost.register,
              initialBudget = input$initialBudget
            ))
          }
          return(NA)
        }),
        iv = iv
      )
    )

  })
}

