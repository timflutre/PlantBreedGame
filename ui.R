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


#### Packages ####
library(shiny)
library(shinydashboard)
library(shinyjs)
library(RSQLite)

library(rutilstimflutre) # https://github.com/timflutre/rutilstimflutre
stopifnot(compareVersion("0.156.5",
                         as.character(packageVersion("rutilstimflutre")))
          != 1)


#### Shiny ####

## Parameters:
W_sideBar <- 300




## UI :
shinyUI(
  dashboardPage(title="Breeding Game", skin="green",
                
                
                # app title
                dashboardHeader(title=tagList(shiny::icon("leaf", "fa-2x"), "\t Breeding Game"), titleWidth=W_sideBar),
                
                # Dashboard sidebar items
                dashboardSidebar(width = W_sideBar,
                                 sidebarMenu(id="leftMenu",
                                   menuItem("How to play?", tabName="info", icon=icon("leaf")),
                                   menuItem("Breeder identification", tabName="id", icon=icon("user-circle")),
                                   menuItem("Request plant material", tabName="plant_mat", icon=icon("sitemap")),
                                   menuItem("Request phenotyping",   tabName="pheno", icon=icon("search")),
                                   menuItem("Request genotyping",    tabName="geno",  icon=icon("database")),
                                   menuItem("About",     tabName="about", icon=icon("info-circle"))
                                 )
                ),
                

                
                #Dashboard body
                dashboardBody(
                  tabItems(
                    source("src/ui_information.R", local=TRUE, encoding = "UTF-8")$value,
                    source("src/ui_id.R", local=TRUE, encoding = "UTF-8")$value,
                    source("src/ui_about.R", local=TRUE, encoding = "UTF-8")$value,
                    source("src/ui_plant_material.R", local=TRUE, encoding = "UTF-8")$value,
                    source("src/ui_pheno.R", local=TRUE, encoding = "UTF-8")$value,
                    source("src/ui_geno.R", local=TRUE, encoding = "UTF-8")$value
                  )
                )
  )
)
