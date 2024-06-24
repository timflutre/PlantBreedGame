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


source("src/dependencies.R", local = TRUE, encoding = "UTF-8")$value


shinyServer(function(input, output, session) {
  currentGTime <- reactive({
    ## this variable contain the game time.
    ## it is reevaluated every 250 milliseconds
    ## send a "tic" message to the client to get information about the server status (busy or not)
    invalidateLater(250)
    session$sendCustomMessage("serverTic", "tic")
    getGameTime()
  })

  values <- reactiveValues(
    lastDBupdate = Sys.time()
  )

  gameInitialised <- function() {
    (dir.exists(DATA_ROOT) &
      dir.exists(DATA_TRUTH) &
      dir.exists(DATA_SHARED) &
      dir.exists(DATA_INITIAL_DATA) &
      file.exists(DATA_DB))
  }

  observe({
    if (!gameInitialised()) {
      alert("Game is not initialised :-(")

      # insertUI("#id_main_ui", where = "beforeBegin", {
      #   source("src/ui/ui_gameNotInitialised.R", local = TRUE, encoding = "UTF-8")$value
      # })
      # removeUI("#id_main_ui")

      # insertUI("#pltmat_main_ui", where = "beforeBegin", {
      #   source("src/ui/ui_gameNotInitialised.R", local = TRUE, encoding = "UTF-8")$value
      # })
      # removeUI("#pltmat_main_ui")

      # insertUI("#pheno_main_ui", where = "beforeBegin", {
      #   source("src/ui/ui_gameNotInitialised.R", local = TRUE, encoding = "UTF-8")$value
      # })
      # removeUI("#pheno_main_ui")

      # insertUI("#geno_main_ui", where = "beforeBegin", {
      #   source("src/ui/ui_gameNotInitialised.R", local = TRUE, encoding = "UTF-8")$value
      # })
      # removeUI("#geno_main_ui")
    }
  })

  source("src/server/server_information.R", local = TRUE, encoding = "UTF-8")$value
  source("src/server/server_id.R", local = TRUE, encoding = "UTF-8")$value
  source("src/server/server_plant_material.R", local = TRUE, encoding = "UTF-8")$value
  source("src/server/server_pheno.R", local = TRUE, encoding = "UTF-8")$value
  source("src/server/server_geno.R", local = TRUE, encoding = "UTF-8")$value
  source("src/server/server_eval.R", local = TRUE, encoding = "UTF-8")$value
  source("src/server/server_theory.R", local = TRUE, encoding = "UTF-8")$value
  source("src/server/server_admin.R", local = TRUE, encoding = "UTF-8")$value
  source("src/server/server_about.R", local = TRUE, encoding = "UTF-8")$value
  source("./src/server/server_constants.R", local = TRUE, encoding = "UTF-8")$value
})
