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
                    h3("My requests:"),
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
   )
                   
) # end shinydashboard::tabBox