shinydashboard::box(
  width = 12, title = "Game not initialised", status = "danger", solidHeader = TRUE,
  div(
    p("The game have not been initialised. It is therefore currently impossible to play."),
    p(
      'To initialise the game, go to the "Admin" menu and in the "Game setup" tab.',
      "From there you will be able to initialise a new game."
    )
  )
)
