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


## Function
source("src/fun/func_data-viz.R", local = TRUE, encoding = "UTF-8")



###### server for "Data visualization" ######

## Main UI ----
output$data_viz_UI <- renderUI({
  if (!gameInitialised()) {
    return(
      source("src/ui/ui_gameNotInitialised.R", local = TRUE, encoding = "UTF-8")$value
    )
  }

  if (breeder() != "No Identification" & breederStatus() != "player") {
    return(source("src/ui/ui_data-viz_loggedIn.R", local = TRUE, encoding = "UTF-8")$value)
  }

  return(
    shinydashboard::box(
      width = 12, title = "Content unavailable",
      div(p("Sorry, you need the 'game-master' status or the 'tester' status to access this."))
    )
  )
})

## Phenotypes visualization ----
dta_viz_pheno_inds_filters <- individual_filtering_server("pheno_data_viz_ind_filter",
  breeder = breeder()
)
dta_viz_pheno_pheno_filters <- phenotype_filtering_server("pheno_data_viz_pheno_filter",
  breeder = breeder()
)

filtered_pheno_data <- reactive({
  input$refresh_pheno_dta_viz
  breeder <- breeder()
  pheno_data <- db_get_phenotypes(
    breeder = breeder,
    ind_id = dta_viz_pheno_inds_filters$inds_ids(),
    request_name = dta_viz_pheno_pheno_filters$pheno_request(),
    pathogen = dta_viz_pheno_pheno_filters$pathogen(),
    year = dta_viz_pheno_pheno_filters$years(),
    public_columns = FALSE
  )

  # mask phenotype data that are not yet available for players
  if (breederStatus() == "player") {
    not_available <- pheno_data$avail_from > getGameTime()
    pheno_data[not_available, c("pathogen", "trait1", "trait2", "trait3")] <- NA
  }
  pheno_data <- pheno_data[, c(
    "ind",
    "control_ind",
    "year",
    "plot",
    "pathogen",
    "trait1",
    "trait2",
    "trait3"
  )]

  categ_vars <- c(
    "control_ind",
    "year",
    "plot",
    "trait3"
  )
  for (var in categ_vars) {
    pheno_data[, var] <- as.character(pheno_data[, var])
  }

  return(pheno_data)
})


data_viz_server("data-viz_pheno", filtered_pheno_data)



## File visualization ----
data_viz_file_validator <- InputValidator$new()
data_viz_file_validator$add_rule("categ_variables", function(x) {
  data <- raw_data_from_file()
  must_be_categorical_var <- colnames(data)[sapply(colnames(data), function(var) {
    numeric_values <- as.numeric(data[, var])
    any(is.na(numeric_values))
  })]
  if (!all(must_be_categorical_var %in% x)) {
    missing_var <- must_be_categorical_var[!must_be_categorical_var %in% x]
    return(paste0(
      "`", paste0(missing_var, collapse = "`, `"),
      "`, must be categorical"
    ))
  }
  return(NULL)
})
data_viz_file_validator$enable()

raw_data_from_file <- reactive({
  if (is.null(input$file_data_viz)) {
    return(NULL)
  }

  df <- utils::read.table(
    input$file_data_viz$datapath,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE
  )

  can_be_numeric_var <- sapply(colnames(df), function(var) {
    numeric_values <- as.numeric(df[, var])
    !any(is.na(numeric_values))
  })
  numeric_var <- colnames(df)[can_be_numeric_var]
  categ_var <- colnames(df)[!can_be_numeric_var]

  updateSelectInput(session, "quant_variables",
    choices = numeric_var,
    selected = numeric_var
  )
  updateSelectInput(session, "categ_variables",
    choices = colnames(df),
    selected = categ_var
  )
  shinyjs::disable("quant_variables")
  df
})

# observeEvent(input$quant_variables, {
#   # TODO: This part is not greate the observed is called 2 times which is not greate
#   if (is.null(input$quant_variables)) {
#     return(NULL)
#   }
#   data <- req(raw_data_from_file())
#   quant_var <- input$quant_variables
#   categ_var <- colnames(data)[!colnames(data) %in% quant_var]
#   if (!setequal(categ_var, input$categ_variables)) {
#     updateSelectInput(session, "categ_variables",
#       selected = categ_var
#     )
#   }
# })

observeEvent(input$categ_variables, {
  if (is.null(input$categ_variables)) {
    return(NULL)
  }
  data <- req(raw_data_from_file())
  categ_var <- input$categ_variables
  quant_var <- colnames(data)[!colnames(data) %in% categ_var]
  updateSelectInput(session, "quant_variables",
    selected = quant_var
  )
})


data_from_file <- reactive({
  data <- raw_data_from_file()
  categ_vars <- input$categ_variables
  quant_vars <- colnames(data)[!colnames(data) %in% categ_vars]

  for (var in quant_vars) {
    data[, var] <- as.numeric(data[, var])
  }
  for (var in categ_vars) {
    data[, var] <- as.character(data[, var])
  }
  data
})

data_viz_server("data-viz_file", data_from_file)




## Breeder information ----
breeder_info_server("breederInfoDtaViz",
  breeder = breeder,
  breederStatus = breederStatus,
  requests_progress_bars = requests_progress_bars,
  currentGTime = currentGTime
)
