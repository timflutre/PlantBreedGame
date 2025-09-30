breeder_info_UI <- function(id) {
  ns <- NS(id)
  list(
    # valueBoxOutput(ns("breederBox"), width = 3),
    # valueBoxOutput(ns("dateBox"), width = 3),
    # valueBoxOutput(ns("budgetBox"), width = 3),
    # valueBoxOutput(ns("serverIndic"), width = 3),
    uiOutput(ns("breederBox")),
    uiOutput(ns("dateBox")),
    uiOutput(ns("budgetBox")),
    uiOutput(ns("serverIndic")),
    uiOutput(ns("next_request_progress"))
  )
}

breeder_info_server <- function(id,
                                breeder = NULL,
                                breederStatus = NULL,
                                requests_progress_bars = NULL,
                                currentGTime = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    # output$breederBox <- renderValueBox({
    output$breederBox <- renderUI({
      if (breeder() == "No Identification") {
        return(NULL)
      }
      valueBox(
        value = breeder(),
        subtitle = paste("Status:", breederStatus()),
        icon = icon("user"),
        color = "yellow",
        width = 3
      )
    })

    # output$dateBox <- renderValueBox({
    output$dateBox <- renderUI({
      if (breeder() == "No Identification") {
        return(NULL)
      }
      valueBox(
        subtitle = "Date",
        value = strftime(currentGTime(), format = "%d %b %Y"),
        icon = icon("calendar"),
        color = "yellow",
        width = 3
      )
    })

    # output$budgetBox <- renderValueBox({
    output$budgetBox <- renderUI({
      if (breeder() == "No Identification") {
        return(NULL)
      }
      valueBox(
        value = db_get_budget(breeder = breeder())$remaining_budget,
        subtitle = "Budget",
        icon = icon("credit-card"),
        color = "yellow",
        width = 3
      )
    })

    # output$serverIndic <- renderValueBox({
    output$serverIndic <- renderUI({
      if (breeder() == "No Identification") {
        return(NULL)
      }
      ## this bow will be modified by some javascript
      valueBoxServer(
        value = "",
        subtitle = "Server load",
        icon = icon("server"),
        color = "yellow",
        width = 3
      )
    })

    output$next_request_progress <- renderUI({
      current_breeder <- breeder()
      if (breeder() == "No Identification") {
        return(NULL)
      }
      req_progress_bar <- requests_progress_bars()
      prog_bar <- NULL
      if (length(req_progress_bar) > 0) {
        prog_bar <- shinydashboard::box(req_progress_bar[[1]],
          width = 12,
          title = "Next request:"
        )
      }
    })
  })
}
