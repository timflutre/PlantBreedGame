shinydashboard::box(
  width = 12, title = NULL,
  div(
    style = "vertical-align: top; margin-top:10px; min-width: 20%;", id = "id_5",
    p("Depending on your status, you are granted with different permissions:"),
    tags$ul(
      tags$li(strong("game-master (such as breeder 'admin'):"), "has the highest privileges. Has access to the \"Admin\" and \"Evaluation\" tabs. Data files available without any time restriction."),
      tags$li(strong("tester (such as breeder 'test'):"), "used to test the game without needing a password. Has access to the \"Evaluation\" tab. Data files available without any time restriction."),
      tags$li(strong("player:"), "used when playing in a common session. Has access neither to the \"Admin\" nor \"Evaluation\" tabs. Data files available under time restriction.")
    )
  ),
  br(),
  div(
    style = "display: inline-block; vertical-align:top;  width: 30%; min-height: 100%;", id = "id_1",
    breeder_list_ui("login_breeder_list")
  ),
  div(
    style = "display: inline-block; vertical-align:top;   min-width: 5%; min-height: 100%;", id = "id_2",
    br()
  ),
  div(
    style = "display: inline-block; vertical-align:top;  width: 30%; min-height: 100%;", id = "id_3",
    passwordInput("psw", "Password")
  ),
  div(
    style = "vertical-align: top;   min-width: 20%;", id = "id_4",
    actionButton("submitPSW", "Log in", style = "background-color:#00A65A; color: white")
  )
)
