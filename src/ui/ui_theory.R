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
  tabName = "theory",
  fluidRow(
    withMathJax(),
    shinydashboard::box(
      width = 12, title = "Notations",
      div(
        id = "about_1",
        p("Phenotypic mean and variance without selection: \\(\\mu_0\\) and \\(\\sigma_0^2\\)"),
        p("Phenotypic mean of selected parents: \\(\\mu^{(s)}\\)"),
        p("Differential of selection: \\(S = \\mu^{(s)} - \\mu_0\\)"),
        p("Selection intensity: \\(i = \\frac{S}{\\sigma_0} = \\frac{z}{\\alpha}\\) where \\(\\alpha\\) is the selection rate (proportion of selected parents)"),
        p("Phenotypic mean of offsprings from selected parents: \\(\\mu_1\\)"),
        p("Response to selection: \\(R = \\mu_1 - \\mu_0\\)")
      )
    ),
    shinydashboard::box(
      width = 12, title = "Parameters",
      sliderInput("mu.0", "Phenotypic mean without selection (\\(\\mu_0\\)):",
        min = 0,
        max = 1.2 * 100, # 100 is the default value for mu.trait1
        value = 100, # 100 is the default value for mu.trait1
        step = 1, round = TRUE
      ),
      sliderInput("sigma.0", "Phenotypic standard deviation without selection (\\(\\sigma_0\\)):",
        min = 0,
        max = round(1.2 * sqrt(711)), # 711 is the default value for simga.p2.trait1
        value = sqrt(711), # 711 is the default value for simga.p2.trait1
        step = 1, round = TRUE
      ),
      sliderInput("h2", "Narrow-sense heritability (\\(h^2\\)):",
        min = 0,
        max = 0.99,
        value = 0.75,
        step = 0.01
      ),
      sliderInput("y.t", "Phenotypic threshold (\\(y_t\\)):",
        min = 0,
        max = round(100 + 4 * sqrt(711)), # mu + 4 sigma
        value = round(100 + 1 * sqrt(711)),
        step = 1, round = TRUE
      )
    ),
    shinydashboard::box(
      title = "Breeder's equation: \\(R = h^2 S\\)",
      width = 12, height = "auto",
      plotOutput("regParsOffs", height = "600")
    )
  ) # close fluidRow
) # close tabItem
