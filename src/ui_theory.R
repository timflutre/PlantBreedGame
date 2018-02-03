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


tabItem(tabName="theory",
  fluidRow(
      withMathJax(),

      shinydashboard::box(width=12, title="Notations",
        div(id="about_1",
            p("Phenotypic mean without selection: \\(\\mu_0\\)"),
            p("Phenotypic mean of selected parents: \\(\\mu^{(s)}\\)"),
            p("Differential of selection: \\(S = \\mu^{(s)} - \\mu_0\\)"),
            p("Phenotypic mean of offsprings from selected parents: \\(\\mu_1\\)"),
            p("Response to selection: \\(R = \\mu_1 - \\mu_0\\)")
            )
        ),

      shinydashboard::box(width=12, title="Parameters",
          ## sliderInput("seed", "Seed of the PRNG:",
          ##             min=1600, max=2000, value=1859, step=1),
          sliderInput("h2", "Narrow-sense heritability (\\(h^2\\)):",
                      min=0, max=1, value=0.75, step=0.05),
          sliderInput("y.t", "Phenotypic threshold (\\(y_t\\)):",
                      min=34, max=48, value=42, step=0.5)
          ),

      shinydashboard::box(title="Breeder's equation: \\(R = h^2 S\\)",
                          width=12, height="auto",
          plotOutput("regParsOffs")
          )

  ) # close fluidRow
) # close tabItem
