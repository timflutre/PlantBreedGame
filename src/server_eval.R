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


## Function
source("src/func_eval.R", local=TRUE, encoding="UTF-8")$value



###### server for "genotyping" ######

## generate the UI:
output$evalUI <- renderUI({
  if (breederStatus()=="game master"){
    list(
    shinydashboard::box(width=12, title = "Choose an evaluation file:",
                        div( id="eval_file",
                             fileInput(inputId="file.eval",
                                       label = NULL,
                                       multiple=FALSE,
                                       accept=c(".txt", ".tsv")),
                             numericInput("nRep", "Choose the number of plot(s) per genotype:", 5, min = 1, max = 100),
                             actionButton("requestEval", "Launch evaluation!")
                        )
    ),

    shinydashboard::tabBox(width=12,  title = "Graphs", id = "eval_graphs", side="left", selected = "Trait 1",
                        tabPanel("Trait 1",
                                 div(
                                 plotlyOutput("evalGraphT1", height = "100%", width = "100%")
                                 )
                        ),
                        tabPanel("Trait 2",
                                 div(
                                 plotlyOutput("evalGraphT2", height = "100%")
                                 )
                        ),
                        tabPanel("Trait 3",
                                 div(
                                 plotlyOutput("evalGraphT3", height = "100%")
                                 )
                        ),
                        tabPanel("Traits 1 vs 2",
                                 div(
                                 plotlyOutput("evalGraphT1vT2", height = "100%")
                                 )
                        )
    ),
    shinydashboard::box(width=12, title = "Debug",
                        verbatimTextOutput("evalDebug")
    )
    ) # close list


  }else{
    shinydashboard::box(width=12, title = "Content unavailable",
                                div(p("Sorry, this is only accessible to the game master."))
    )
  }


})




## read uploaded file
readQryEval <- reactive({

  # no input fileI
  if(is.null(input$file.eval)){
    return(NULL)
  }

  # read input file
  df <- try(readCheckEvalFile(input$file.eval$datapath))

  if (is.data.frame(df)){
    # add controls in the data.frame
    df.controls <- read.table(paste0(setup$init.dir, "/controls.txt"), col.names="ind")
    df.controls$breeder <- rep("control", length(df.controls))
    df <- rbind(df, df.controls)
    return(df)

  }else {return("error - wrong file format")}

})


dfPhenoEval <- eventReactive(input$requestEval,{
  dfPheno <- phenotype4Eval(readQryEval(), nRep=input$nRep)
  dfPheno$breeder <- sapply(as.character(dfPheno$ind), FUN = function(x){strsplit(x,split = "_")[[1]][1]})
  return(dfPheno)
})




output$evalGraphT1 <- renderPlotly({
  dfPheno <- dfPhenoEval()
  breederOrder <- c(unique(as.character(dfPheno$ind[dfPheno$breeder=="control"])),
                    unique(as.character(dfPheno$ind[dfPheno$breeder!="control"])))

  medControl <- median(dfPheno$trait1[dfPheno$breeder=="control"])

  
  
  ## Plot
  m <- list(
    l = 40,
    r = 80,
    b = 80,
    t = 50,
    pad = 0
  )
  
  
  p <- plot_ly(data=dfPheno,
               type = 'box',
               y = ~trait1,
               x= ~ind,
               color = ~breeder) %>%
      layout(title = "Phenotypic values of trait 1",
             xaxis = list(title = "",
                          categoryorder = "array",
                          categoryarray = breederOrder
                          ),
             yaxis = list(title = ""),
             autosize = T,
             margin = m)%>%
      add_lines(data=NULL,
                type='scatter',
                y=medControl,
                mode='lines',
                color = "control",
                name="Median")

  
})
output$evalGraphT2 <- renderPlotly({
  dfPheno <- dfPhenoEval()
  breederOrder <- c(unique(as.character(dfPheno$ind[dfPheno$breeder=="control"])),
                    unique(as.character(dfPheno$ind[dfPheno$breeder!="control"])))

  medControl <- median(dfPheno$trait2[dfPheno$breeder=="control"])
  
  
  ## Plot
  m <- list(
    l = 40,
    r = 80,
    b = 80,
    t = 50,
    pad = 0
  )

  p <- plot_ly(data=dfPheno,
               type = 'box',
               y = ~trait2,
               x= ~ind,
               color = ~breeder) %>%
    add_lines(data=NULL,
              type='scatter',
              y=medControl,
              mode='lines',
              color = "control",
              name="Median")%>%
    layout(title = "Phenotypic values of trait 2",
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = breederOrder),
           yaxis = list(title = ""),
           autosize = T,
           margin = m)


})
output$evalGraphT3 <- renderPlotly({
  dfPhenoPatho <- dfPhenoEval()
  dfPhenoPatho <- dfPhenoPatho[(as.numeric(dfPhenoPatho$plot) %% input$nRep) == 1,]

  breederOrder <- c(unique(as.character(dfPhenoPatho$ind[dfPhenoPatho$breeder=="control"])),
                    unique(as.character(dfPhenoPatho$ind[dfPhenoPatho$breeder!="control"])))


  ## Plot
  m <- list(
    l = 40,
    r = 80,
    b = 80,
    t = 50,
    pad = 0
  )
  
  
  p <- plot_ly(data=dfPhenoPatho,
               type = 'bar',
               y = ~trait3*2-1,
               x= ~ind,
               color = ~breeder) %>%
    layout(title = "Phenotypic values of trait 3",
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = breederOrder),
           yaxis = list(title = ""),
           autosize = T,
           margin = m)

})




output$evalGraphT1vT2<- renderPlotly({
  dfPheno <- dfPhenoEval()
  dfPheno <- dfPheno[(as.numeric(dfPheno$plot) %% input$nRep) == 1,]
  
  # get the haplotype of each individual of the initial collection
  f <- paste0(setup$truth.dir, "/", "p0.RData")
  dfInitColl <- data.frame(GAT1=p0$G.A[,1], GAT2=p0$G.A[,2])

  p <- plot_ly(type = 'scatter') %>%
    add_markers(data=dfInitColl,
                type = 'scatter',
                y = ~GAT2,
                x= ~GAT1,
                color = "Initial Collection",
                marker=list(color="gray" , size=5 , opacity=0.5),
                text="iniColl",
                inherit=FALSE) %>%
    add_markers(data=dfPheno,
                type = 'scatter',
                y = ~GAT2,
                x= ~GAT1,
                marker=list(size=8, opacity=1),
                color=~breeder,
                text=~ind,
                inherit=FALSE) %>%
    layout(title = "Genotypic values of traits 1 vs 2",
           xaxis = list(title = "GA 1"),
           yaxis = list(title = "GA 2"))

})


output$evalDebug <- renderPrint({
  print("---------")
  print(dfPhenoEval())


})

