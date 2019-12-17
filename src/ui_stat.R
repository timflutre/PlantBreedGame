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




tabItem(tabName = "stat",
        fluidRow(
          useShinyjs(),
          tags$script(
            "Shiny.addCustomMessageHandler(
                                    'resetValue',function(variableName){
                                    Shiny.onInputChange(variableName, null);});"
          ), 
          
          
          # 4 top box specifying:
          #  - breeder
          #  - Date
          #  - Budget
          #  - server status
          uiOutput("stat_UIbreederInfo"),
          
          
          # Information box
          shinydashboard::box(width = 12, title = "Statistical Toolbox",
            div(id = "stat_info1",
                h3("Explainations"),
                p("To do")
            )
          ),
          
          
          
          
          
          # Main tab Box
    shinydashboard::tabBox(width = 12, title = "Statistical tools", id = "stat_tabset", side = "left", selected = "BLUP estimation",
                           tabPanel("BLUP estimation",
                                    div("BLUP to do")
                           ),
                           
                           tabPanel("GWAS",
                                    div(" GWAS to do")
                           ),
                           
                           tabPanel("Genomic Prediction",
                                    div("GP to do")
                           )
    )
))