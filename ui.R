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


## UI parameters
W_sideBar <- 300

shinyUI(
  dashboardPage(
    title = "PlantBreedGame", skin = "green",

    # app title
    dashboardHeader(
      title = tagList(shiny::icon("leaf", "fa-2x"), "\t PlantBreedGame"),
      titleWidth = W_sideBar
    ),

    # dashboard sidebar items
    dashboardSidebar(
      width = W_sideBar,
      sidebarMenu(
        id = "leftMenu",
        menuItem("How to play?",
          tabName = "info",
          icon = icon("leaf")
        ),
        menuItem("Identification / Home",
          tabName = "id",
          icon = icon("house-user")
        ),
        menuItem("Request plant material",
          tabName = "plant_mat",
          icon = icon("seedling")
        ),
        menuItem("Request phenotyping",
          tabName = "pheno",
          icon = icon("flask")
        ),
        menuItem("Request genotyping",
          tabName = "geno",
          icon = icon("dna")
        ),
        menuItem("Evaluation",
          tabName = "eval",
          icon = icon("medal")
        ),
        menuItem("Theory",
          tabName = "theory",
          icon = icon("calculator")
        ),
        menuItem("Admin",
          tabName = "admin",
          icon = icon("cogs")
        ),
        menuItem("About",
          tabName = "about",
          icon = icon("info-circle")
        )
      )
    ),


    # dashboard body
    dashboardBody(
      ## javascript function
      tags$head(
        tags$script(src = "busyServer.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(href = "https://fonts.googleapis.com/css?family=Nunito", rel = "stylesheet")
      ),
      tabItems(
        source("src/ui/ui_information.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value,
        source("src/ui/ui_id_logPage.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value,
        source("src/ui/ui_plant_material.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value,
        source("src/ui/ui_pheno.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value,
        source("src/ui/ui_geno.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value,
        source("src/ui/ui_eval.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value,
        source("src/ui/ui_theory.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value,
        source("src/ui/ui_admin.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value,
        source("src/ui/ui_about.R",
          local = TRUE,
          encoding = "UTF-8"
        )$value
      )
    )
  ) # close dashboardPage
) # close shinyUI
