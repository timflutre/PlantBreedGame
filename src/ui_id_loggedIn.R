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


##### File information #####
## This file contain the UI code for logged user for the id tab
## this file is sourced in "server_id.R" in a renderUI() function
############################



shinydashboard::tabBox(width=12, title =paste0("My account"),
   tabPanel("My files",
            div(style="display: inline-block; vertical-align:top; width: 50%;",
                div(
                    h3("Phenotyping data:"),
                    selectInput("phenoFile", "", choices=phenoFiles(),width="75%"),
                    uiOutput("UIdwnlPheno")
                ),
                div(
                    h3("Genotyping data:"),
                    selectInput("genoFile", "", choices=genoFiles(),width="75%"),
                    uiOutput("UIdwnlGeno")
                )
            ),
            div(style="display: inline-block; vertical-align:top; width: 49%;",
                div(
                    h3("Plant material data:"),
                    selectInput("pltMatFile", "", choices=pltMatFiles(),width="75%"),
                    uiOutput("UIdwnlPltMat")
                ),
                div(
                    h3("Other:"),
                    p("You can download here:",
                      tags$ul(
                         tags$li("SNP coordinates of the HD and LD chips"),
                         tags$li("List of controls"),
                         tags$li("Examples of requests files"),
                         tags$li("List of your requests")
                         )
                      ),
                    selectInput("requestFile", "", choices=requestFiles(),width="75%"),
                    uiOutput("UIdwnlRequest")
                )
            )


   ),

   tabPanel("My plant material",
            dataTableOutput("myPltMatDT")
   ),

   tabPanel("Change my password",
            div(style="display: inline-block; vertical-align:top;  width: 30%; min-height: 100%;",
                passwordInput("prevPsw", "Previous Password")
            ),
            div(style="display: inline-block; vertical-align:top;   min-width: 5%; min-height:: 100%;",
                p()
            ),
            div(style="display: inline-block; vertical-align:top;  width: 30%; min-height:: 100%;",
                passwordInput("newPsw", "New Password")
            ),
            div(style="display: inline-block; vertical-align: top;   min-width: 100%;",
                tags$head(
                    tags$style(HTML('#changePsw{background-color:gray; color: white}'))
                ),
                actionButton("changePsw", "Change my password!")
            ),
            div(style="vertical-align: top;   min-width: 20%;", id="id_4",
                uiOutput("UIpswChanged")
            )
   ),



   tabPanel("Register final individuals",
            div(style="display: inline-block; vertical-align:top; width: 40%;",
                div(
                   h3("Individuals submission:"),
                   p("You can specify here the individuals you want to submit for the final evaluation."),
                   p("A maximum of ",
                     strong(constants$maxEvalInds, " individuals"),
                     "can be registered."
                   ),
                   p("The registration fee is ", strong(format(constants$cost.register * constants$cost.pheno.field, digits=2), " Mendels"),"per genotype. No refund are possible, thank-you for your understanding.")
                ),
                div(
                   selectInput("id_evalInds",
                               HTML("Select your best individuals<sup>*</sup>:"),
                               choices = myPltMat()$child,
                               multiple = TRUE),
                   actionButton("id_submitInds", "Submit"),
                   p(tags$sup("*"), "The drop-down menu is limited to 1000 propositions. Write the name of your individuals to find them.")
                )
            ),


            div(style="display: inline-block; vertical-align:top; width: 50%;",
                div(
                   h4("Your submitted individuals:"),
                   dataTableOutput("submittedIndsDT"),
                   p("Click on the individuals to delete them."),
                   actionButton("id_delSubmitInds", "Delete")
                )
            ),

   )

) # end shinydashboard::tabBox
