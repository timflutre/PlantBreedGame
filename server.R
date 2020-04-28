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


source("src/dependencies.R", local=TRUE, encoding="UTF-8")$value


shinyServer(function(input, output, session){

  currentGTime <- reactive({
    ## this variable contain the game time.
    ## it is reevaluated every 250 milliseconds
    ## send a "tic" message to the client to get information about the server status (busy or not)
    invalidateLater(250)
    session$sendCustomMessage("serverTic", "tic")
    getGameTime(setup)
  })

  source("src/server_information.R", local=TRUE, encoding="UTF-8")$value
  source("src/server_id.R", local=TRUE, encoding="UTF-8")$value
  source("src/server_plant_material.R", local=TRUE, encoding="UTF-8")$value
  source("src/server_pheno.R", local=TRUE, encoding="UTF-8")$value
  source("src/server_geno.R", local=TRUE, encoding="UTF-8")$value
  source("src/server_eval.R", local=TRUE, encoding="UTF-8")$value
  source("src/server_theory.R", local=TRUE, encoding="UTF-8")$value
  source("src/server_admin.R", local=TRUE, encoding="UTF-8")$value
  source("src/server_about.R", local=TRUE, encoding="UTF-8")$value
})
