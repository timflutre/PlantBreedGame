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


source("src/func_theory.R", local=TRUE, encoding="UTF-8")$value


getDatSel <- reactive({
  dat <- simulDat(mu.0=input$mu.0,
                  sigma2=(1 - input$h2) * input$sigma.0^2,
                  h2=input$h2,
                  I=constants$nb.phenotyped.coll,
                  seed=1859)

  sel <- applySelection(mu.0=dat$mu.0, y=dat$y, y.e=dat$y.e,
                        y.t=input$y.t, sigma.02=dat$sigma.a2 + dat$sigma2)

  return(append(dat, sel))
})

output$regParsOffs <- renderPlot({
  all <- getDatSel()
  plotRegMidparentsOffsprings(mu.0=all$mu.0,
                              sigma.02=all$sigma.a2 + all$sigma2,
                              h2=all$h2,
                              y=all$y, y.e=all$y.e,
                              y.t=all$y.t, is.sel=all$is.sel,
                              mu.s=all$mu.s, S=all$S, i=all$i, alpha=all$alpha,
                              mu.1=all$mu.1, R=all$R)
})
