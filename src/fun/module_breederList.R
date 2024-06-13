breeder_list_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("breederList"))
}
breeder_list_server <- function(id, input_id, reactive_breederList) {
  moduleServer(id, function(input, output, session) {
    output$breederList <- renderUI({
      breederList <- reactive_breederList()
      selectInput(input_id, "Breeder", choices = as.list(breederList))
    })
  })
}
