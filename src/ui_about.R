## Copyright 2015,2016,2017,2018 Institut National de la Recherche Agronomique
## and Montpellier SupAgro.
##
## This file is part of PlantSelBreedGame.
##
## PlantSelBreedGame is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## PlantSelBreedGame is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with PlantSelBreedGame.  If not, see
## <http://www.gnu.org/licenses/>.


tabItem(tabName="about",

    fluidRow(
        shinydashboard::box(width=12, title=NULL,
        div(id="about_1",
            h3("Copyright "),
            p("2015-2018:",
              a("INRA", href="http://inra.fr/", target="_blank"),
              ", ",
              a("Montpellier SupAgro", href="http://supagro.fr/", target="_blank")),
            h3("Authors"),
            p("In alphabetical order: Jacques David, Julien Diot, Timothée Flutre."),
            h3("Sources"),
            a("GitHub", href="https://github.com/timflutre/PlantSelBreedGame", target="_blank")
            )
        )
    ),

    fluidRow(
        column(2, offset=2, img(src="logo_INRA.png", height=70, width=120), br(), br()),
        column(2, offset=1, img(src="logo_SUPAGRO.jpg", height=70, width=120))
    )

)
