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


tabItem(
  tabName = "about",
  fluidRow(
    shinydashboard::box(
      width = 12, title = NULL,
      column(12,
        id = "about_1",
        h3("Context"),
        p("This is the ", code("PlantBreedGame"), " software implementing a serious game to teach selective breeding via the example of a fictitious annual plant species to students at the master level."),
        h3("Citation"),
        p("Flutre, T., Diot, J., and David, J. (2019). PlantBreedGame: A Serious Game that Puts Students in the Breeder’s Seat. Crop Science. DOI ", a("10.2135/cropsci2019.03.0183le", href = "https://dl.sciencesocieties.org/publications/cs/abstracts/0/0/cropsci2019.03.0183le", target = "_blank")),
        h3("Copyright"),
        p(
          "2015-2019: ",
          a("INRA", href = "http://inra.fr/", target = "_blank"),
          ", ",
          a("Montpellier SupAgro", href = "http://supagro.fr/", target = "_blank")
        ),
        h3("Authors"),
        p("Timothée Flutre, Julien Diot, Jacques David."),
        h3("Website"),
        p(a("https://sourcesup.renater.fr/plantbreedgame/", href = "https://sourcesup.renater.fr/plantbreedgame/", target = "_blank")),
        h3("Sources"),
        p(
          "The software takes the form of a ", a("Shiny", href = "http://shiny.rstudio.com/", target = "_blank"), " application, benefiting from the ", a("R", href = "https://www.r-project.org/", target = "_blank"), " programming language and software environment for statistical computing.",
          "It is available under a free software license, the ", a("GNU Affero General Public License", href = "https://www.gnu.org/licenses/agpl.html", target = "_blank"), " (version 3 and later)."
        ),
        p(
          "Source code: ", a("GitHub repository", href = url.repo, target = "_blank")
        ),
        h3("Version"),
        p("Current version: ",
          a(code.version$display, href = code.version$link, target = "_blank")
        ),
        br(),
        br(),
        br(),
        br()
      ),
      column(5, offset = 0, img(src = "logo_INRA.png", height = 70, width = 70 * 2.43)),
      column(3, offset = 0, img(src = "logo_SUPAGRO.jpg", height = 70, width = 70 * 2.38)),
      column(12,
        id = "about_3",
        br(),
        br(),
        br(),
        br(),
        h3("Session information"),
        verbatimTextOutput("about_sessionInfo")
      )
    )
  )
)
