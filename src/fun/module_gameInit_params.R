
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




gameInit_traits_ui <- function(id) {
  ns <- NS(id)

  width_numInput <- "90%"

  tooltip_label_min <- function(t){
    # t is either 1 or 2
    tooltip_label(
      paste0("Min T", t),
      div(
        p(paste("Minimum value for Trait", t)),
        p(HTML("Approximately 99% of the phenotypes of the initial population",
          "will be above this value.",
          "<br/>It will be used to calculate the variance of the phenotypes:",
          "\\({\\sigma_p}^2 = ((\\mu - {Min_t}^T) / 3)^2\\)")
        )
      )
    )
  }
  tooltip_label_mu <- function(t){
    # t is either 1 or 2
    tooltip_label(
      paste0("μ T", t),
      paste("Intercept for trait", t)
    )
  }
  tooltip_label_gCV <- function(t){
    # t is either 1 or 2
    tooltip_label(
      paste0("gCV T", t),
      div(
        p(paste("Genotypic coefficient of variation for trait", t)),
        p(HTML("This value is used to calculate the genetic variance:",
          "<br/>\\({\\sigma_a}^2 = (gCV * \\mu )^2\\)")
        )
      )
    )
  }
  tooltip_label_h2 <- function(t){
    # t is either 1 or 2
    tooltip_label(
      paste0("Heritability T", t),
      div(
        p("Intra-annual heritability"),
        p(HTML("This value is used to calculate the variance of the errors \\(\\epsilon\\):",
          "<br/>\\(\\sigma = \\frac{1-h^2}{h^2} {\\sigma_a}^2\\)")
        )
      )
    )
  }

  div(
    collapible_section(
      title = "Phenotype simulation:",
      title_id = ns("pheno_simul_title"),
      div(
        tags$blockquote(style = "font-weight: normal; font-size: inherit; font-style: italic;",
        div(
          h4("Quantitative traits simulation (T1 and T2)"),
          p("The general model for simulating the phenotypes of quantitative traits (T1 and T2) is as follow:"),
          p(withMathJax("$$y_{i,y,r} = \\mu + g_i + y_y + \\epsilon_{i,y,r}$$")),
          p("Where:",
            tags$ul(
              tags$li(
                "\\(y_{i,y,r}\\) is the phenotype value of the individual \\(i\\) at",
                "year \\(y\\) for repetition \\(r\\)"
              ),
              tags$li(
                "\\(\\mu\\) is the intercept values of the phenotype"
              ),
              tags$li(HTML(
                "\\(g_i\\) is the genotypic value of the individual \\(i\\).",
                "<br/>",
                "This value is calculated based on the individual's genotypes",
                "\\(x_{i,snp}\\) and the markers effects",
                "\\(\\beta_{snp}\\): \\(g_i = \\sum_{snp} x_{i,snp}\\times\\beta_{snp}\\).",
                "<br/> These markers effects \\(\\beta\\) are generated such as",
                "the \\(g_i\\) of the initial population seems to be drawn from a normal distribution",
                "\\(\\mathcal{N}(0, {\\sigma_{a}}^2)\\)"
              )),
              tags$li(
                "\\(y_y\\) is the effect of the year \\(y\\) drawn from a normal distribution ",
                "\\(\\mathcal{N}(0, {\\sigma_y}^2)\\)"
              ),
              tags$li(
                "\\(\\epsilon_{i,y,r}\\) is a random noise drawn from a normal distribution ",
                "\\(\\mathcal{N}(0, \\sigma^2)\\)"
              )
            )
          ),
          p("Some remarks:",
            tags$ul(
              tags$li(
                  "T1 and T2 have pure additive infinitesimal genetic architecture",
                  "and all SNPs (even the rarest) have a non-zero effect"
                ),
              tags$li(
                  "There is no year/genotype interaction"
                ),
              tags$li(
                  'There is no "plot" effect nor spatial heterogeneity',
                  '(even if the phenotype data have a "plot" column)'
                ),
              tags$li(HTML(
                  "The phenotypic variance of the initial population is:",
                  "<br/>\\({\\sigma_p}^2 = {\\sigma_a}^2 + {\\sigma_y}^2 + \\sigma^2\\)")
                )
            )
          )
        )
        ),

        div(style = "display: flex;",
          div(style = "flex: 1;",
            h4("Trait 1 (Yield):", style = "margin-top: 20px;"),
            shiny::numericInput(ns("t1_mu"), value = 100, step = 0.5, width = width_numInput,
              label = tooltip_label_mu(1)
            ),
            shiny::numericInput(ns("t1_min"), value = 20, step = 0.5, width = width_numInput,
              label = tooltip_label_min(1)
            ),
            shiny::numericInput(ns("t1_cv_g"), value = 0.10, step = 0.01, width = width_numInput,
              label = tooltip_label_gCV(1)
            ),
            shiny::numericInput(ns("t1_h2"), value = 0.30, step = 0.01, width = width_numInput,
              label = tooltip_label_h2(1)
            ),

            h4("Trait 2 (Quality):", style = "margin-top: 20px;"),
            shiny::numericInput(ns("t2_mu"), value = 15, step = 0.5, width = width_numInput,
              label = tooltip_label_mu(2)
            ),
            shiny::numericInput(ns("t2_min"), value = 5, step = 0.5, width = width_numInput,
              label = tooltip_label_min(2)
            ),
            shiny::numericInput(ns("t2_cv_g"), value = 0.06, step = 0.01, width = width_numInput,
              label = tooltip_label_gCV(2)
            ),
            shiny::numericInput(ns("t2_h2"), value = 0.60, step = 0.01, width = width_numInput,
              label = tooltip_label_h2(2)
            ),

            h4("Trait 1/2 pleiotropy:", style = "margin-top: 20px;"),
            shiny::numericInput(ns("prop_pleio"), value = 0.4, step = 0.01, width = width_numInput, label = 'Proportion of pleiotropy'),
            shiny::numericInput(ns("cor_pleio"), value = -0.7, step = 0.01, width = width_numInput, label = 'Pleiotropy correlation'),

          ),

          div(style = "flex: 1;",
            h4("Informations", style = "margin-top: 20px;"),
            tags$blockquote(style = "font-weight: normal; font-size: inherit; font-style: italic;",
              h4("Models parameters:"),
              div(style = "display: flex;",
                div(style = "flex: 1;",
                  h5("Trait 1:"),
                  tags$ul(
                    tags$li("\\(\\mu =\\)", textOutput(ns("T1_mu"), inline = T)),
                    tags$li("\\({\\sigma_p}^2 =\\)", textOutput(ns("T1_sig_p"), inline = T)),
                    tags$li("\\({\\sigma_a}^2 =\\)", textOutput(ns("T1_sig_a"), inline = T)),
                    tags$li("\\({\\sigma}^2 =\\)", textOutput(ns("T1_sig"), inline = T)),
                    tags$li("\\({\\sigma_y}^2 =\\)", textOutput(ns("T1_sig_y"), inline = T))
                  )
                ),
                div(style = "flex: 1;",
                  h5("Trait 2:"),
                  tags$ul(
                    tags$li("\\(\\mu =\\)", textOutput(ns("T2_mu"), inline = T)),
                    tags$li("\\({\\sigma_p}^2 =\\)", textOutput(ns("T2_sig_p"), inline = T)),
                    tags$li("\\({\\sigma_a}^2 =\\)", textOutput(ns("T2_sig_a"), inline = T)),
                    tags$li("\\({\\sigma}^2 =\\)", textOutput(ns("T2_sig"), inline = T)),
                    tags$li("\\({\\sigma_y}^2 =\\)", textOutput(ns("T2_sig_y"), inline = T))
                  )
                )
              )
            ),
            plotlyOutput(ns("pheno_plot"), width = "100%")
          )
        )
      )
    )
  )
}

quick_pheno_simul <- function(mu, min, cv_g, h2) {

  saved_seed <- .Random.seed
  set.seed(1993)
  n_inds <- 100
  n_years <- 5
  first_year <- 1

  sig_p <- calc_sigma_p(mu, min)
  sig_a <- calc_sigma_a(cv_g, mu)
  sig <- calc_sigma(h2, sig_a)
  sig_y <- calc_sigma_y(sig_p, sig_a, sig)

  df <- data.frame(
    i = seq(1, n_inds),
    g = rnorm(n_inds, 0, sqrt(sig_a))
  )

  df <- do.call(rbind, lapply(seq(first_year, first_year + n_years), function(year) {
    y_eff <- rnorm(1, 0, sqrt(sig_y))
    df$year <- year
    df$year_eff <- y_eff
    df$pheno <- mu + df$g + y_eff + rnorm(n_inds, 0, sqrt(sig))
    df
  }))
  df$year <- as.character(df$year)

  set.seed(saved_seed)
  return(df)
}

calc_sigma_p <- function(mu, min) {((mu - min)/3)^2}
calc_sigma_a <- function(cv_g, mu) {(cv_g * mu)^2}
calc_sigma <- function(h2, sigma_a) {((1 - h2) / h2) * sigma_a}
calc_sigma_y <-  function(sigma_p, sigma_a, sigma) {sigma_p - sigma_a - sigma}

gameInit_traits_server <- function(id, iv) {
  moduleServer(id, function(input, output, session) {


    pheno_params_validator <- InputValidator$new()
    # pheno_params_validator$add_rule("t1_mu", sv_integer())

    iv$add_validator(pheno_params_validator)


    output$T1_mu <- renderText({input$t1_mu})
    output$T1_sig_p <- renderText({
      round(calc_sigma_p(input$t1_mu, input$t1_min), 4)
    })
    output$T1_sig_a <- renderText({calc_sigma_a(input$t1_cv_g, input$t1_mu)})
    output$T1_sig <- renderText({
      T1_sig_a <- calc_sigma_a(input$t1_cv_g, input$t1_mu)
      round(calc_sigma(input$t1_h2, T1_sig_a), 4)
    })
    output$T1_sig_y <- renderText({
      T1_sig_p <- calc_sigma_p(input$t1_mu, input$t1_min)
      T1_sig_a <- calc_sigma_a(input$t1_cv_g, input$t1_mu)
      T1_sig <- calc_sigma(input$t1_h2, T1_sig_a)
      round(calc_sigma_y(T1_sig_p, T1_sig_a, T1_sig), 4)
    })
    output$T2_mu <- renderText({input$t2_mu})
    output$T2_sig_p <- renderText({
      round(calc_sigma_p(input$t2_mu, input$t2_min), 4)
    })
    output$T2_sig_a <- renderText({calc_sigma_a(input$t2_cv_g, input$t2_mu)})
    output$T2_sig <- renderText({
      T2_sig_a <- calc_sigma_a(input$t2_cv_g, input$t2_mu)
      round(calc_sigma(input$t2_h2, T2_sig_a), 4)
    })
    output$T2_sig_y <- renderText({
      T2_sig_p <- calc_sigma_p(input$t2_mu, input$t2_min)
      T2_sig_a <- calc_sigma_a(input$t2_cv_g, input$t2_mu)
      T2_sig <- calc_sigma(input$t2_h2, T2_sig_a)
      round(calc_sigma_y(T2_sig_p, T2_sig_a, T2_sig), 4)
    })


    output$pheno_plot <- renderPlotly({
      T1_mu <- input$t1_mu
      T1_min <- input$t1_min
      T1_cv_g <- input$t1_cv_g
      T1_h2 <- input$t1_h2

      T2_mu <- input$t2_mu
      T2_min <- input$t2_min
      T2_cv_g <- input$t2_cv_g
      T2_h2 <- input$t2_h2

      data_t1 <- quick_pheno_simul(T1_mu, T1_min, T1_cv_g, T1_h2)
      data_t2 <- quick_pheno_simul(T2_mu, T2_min, T2_cv_g, T2_h2)

      fig_T1 <- plot_ly(
        data_t1,
        x = ~year,
        y = ~pheno,
        type = 'box',
        name = "Trait 1") %>% add_lines(
          data = NULL,
          type = "scatter",
          y = T1_mu,
          mode = "lines",
          name = "μ T1") %>% layout(
          xaxis = list(
            title = "Year"
          ),
          yaxis = list(title = "Phenotypes Trait 1")
        )

      fig_T2 <- plot_ly(
        data_t2,
        x = ~year,
        y = ~pheno,
        type = 'box',
        name = "Trait 2") %>% add_lines(
          data = NULL,
          type = "scatter",
          y = T2_mu,
          mode = "lines",
          name = "μ T2") %>% layout(
          xaxis = list(
            title = "Year"
          ),
          yaxis = list(title = "Phenotypes Trait 2")
        )

      fig <- subplot(fig_T1, fig_T2, nrows = 2, titleX = TRUE, titleY = TRUE, margin = 0.1) %>% layout(
        title = "Simulated phenotypic values for the initial population"
      )

      return(fig)
    })
  })
}






