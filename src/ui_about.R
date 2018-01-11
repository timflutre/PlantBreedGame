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


# UI of "about" part


tabItem(tabName="about",
        fluidRow(
    uiOutput("UIbreederInfo6"),

    shinydashboard::box(width=12, title = NULL,
        div(id="about_1",
            h3("Copyright "), p("2015 - 2018 INRA, Montpellier SupAgro"),
            h3("Authors"), p("In alphabetical order: Jacques David, Julien Diot, Timothée Flutre."),
            h3("Sources"), a("GitHub", href="https://github.com/timflutre/PlantSelBreedGame", target="_blank")
        )
    )
          
          
          
          
          
          
))
