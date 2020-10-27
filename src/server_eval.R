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


## Function
source("src/func_eval.R", local=TRUE, encoding="UTF-8")$value



###### server for "genotyping" ######

## Main UI ----
output$evalUI <- renderUI({
  if (breeder()!="No Identification" & breederStatus()!="player"){
    source("src/ui_eval_loggedIn.R", local=TRUE, encoding="UTF-8")$value
  }else{
    shinydashboard::box(width=12, title = "Content unavailable",
                                div(p("Sorry, you need the 'game-master' status or the 'tester' status to access this."))
    )
  }
})




## read uploaded file ----
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
  dfPheno$breeder <- sapply(as.character(dfPheno$ind), FUN = function(x){strsplit(x,split = "\\*")[[1]][1]})
  return(dfPheno)
})



## graph ----
evalGraphT1 <- reactive({
  dfPheno <- dfPhenoEval()
  breederOrder <- c(unique(as.character(dfPheno$ind[dfPheno$breeder=="control"])),
                    unique(as.character(dfPheno$ind[dfPheno$breeder!="control"])))

  target <- median(dfPheno$trait1[dfPheno$breeder=="control"])*constants$register.min.trait1


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
               color = ~breeder,
               colors = mycolors) %>%
    layout(title = "Phenotypic values of trait 1",
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = breederOrder
           ),
           yaxis = list(title = ""),
           autosize = T,
           margin = m) %>%
    add_lines(data=NULL,
              type='scatter',
              y=target,
              mode='lines',
              color = "Target")
  p
})
output$evalGraphT1 <- renderPlotly({
  evalGraphT1()
})

evalGraphT2 <- reactive({
  dfPheno <- dfPhenoEval()
  breederOrder <- c(unique(as.character(dfPheno$ind[dfPheno$breeder=="control"])),
                    unique(as.character(dfPheno$ind[dfPheno$breeder!="control"])))

  target <- constants$register.min.trait2


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
               color = ~breeder,
               colors = mycolors) %>%
    add_lines(data=NULL,
              type='scatter',
              y=target,
              mode='lines',
              color = "Target")%>%
    layout(title = "Phenotypic values of trait 2",
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = breederOrder),
           yaxis = list(title = ""),
           autosize = T,
           margin = m)
})
output$evalGraphT2 <- renderPlotly({
  evalGraphT2()
})



evalGraphT3 <- reactive({
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
               color = ~breeder,
               colors = mycolors) %>%
    layout(title = "Phenotypic values of trait 3",
           xaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = breederOrder),
           yaxis = list(title = ""),
           autosize = T,
           margin = m)
})
output$evalGraphT3 <- renderPlotly({
  evalGraphT3()
})



evalGraphT1vT2 <- reactive({
  dfPheno <- dfPhenoEval()
  dfPheno <- dfPheno[(as.numeric(dfPheno$plot) %% input$nRep) == 1,]

  # get the data for the initial collection
  f <- paste0(setup$truth.dir, "/", "p0.RData")
  load(f)
  dfInitColl <- data.frame(GAT1=p0$G.A[,1], GAT2=p0$G.A[,2], ind=names(p0$G.A[,2]))

  # linear regretion
  linMod <- lm(GAT2 ~ GAT1, data=dfInitColl)

  xLine <- c(min(dfInitColl$GAT1), max(dfInitColl$GAT1))
  yLine <- linMod$coefficients[1]+linMod$coefficients[2]*xLine


  p <- plot_ly(type = 'scatter',
               colors = mycolors) %>%
    add_markers(data=dfInitColl,
                type = 'scatter',
                y = ~GAT2,
                x= ~GAT1,
                color = "Initial Collection",
                marker=list(color="gray" , size=5 , opacity=0.5),
                text=~ind,
                inherit=FALSE) %>%
    add_lines(data=NULL,
              type='scatter',
              x=xLine,
              y=yLine,
              mode='lines',
              color = "Initial Collection",
              line=list(color="gray"),
              name="Linear regression",
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
output$evalGraphT1vT2 <- renderPlotly({
  evalGraphT1vT2()
})





output$evalUIAfsPlot <- renderUI({
  if (exists("dfPhenoEval")){
    breeders <- unique(dfPhenoEval()$breeder)
    breeders <- breeders[breeders!="control"]
    list(
      selectInput("afsBreeder","Breeder", choices=breeders),
      numericInput("propAFS", "Proportion of last individuals to take", 10, min = 1, max = 100),
      plotlyOutput("evalGraphAFsHist", height = "100%",width="100%") %>% withSpinner(),
      plotlyOutput("evalGraphAFsScatter", height = "100%",width="100%") %>% withSpinner())

  } else { p('no input')}

})

## AFs evaluation ----
afsEval <- reactive({


  # get parameters
  prop <- input$propAFS/100
  breeder <- input$afsBreeder
  f <- paste0(setup$truth.dir, "/afs0.RData")
  load(f) # afs0


  # get all individuals
  db <- dbConnect(SQLite(), dbname=setup$dbname)
  query <- paste0("SELECT * FROM plant_material_",breeder)
  res <- (dbGetQuery(conn=db, query))
  dbDisconnect(db)

  # select sample
  sampleSize <- round(nrow(res)*prop)
  selectedInd <- res[c((nrow(res)-sampleSize+1):nrow(res)), ]

  # calculate AFS
  progressAFS <- shiny::Progress$new(session, min=0, max=1)
  progressAFS$set(value = 0,
                  message = "Calculate AFs")
  afs1 <-  getAFs(selectedInd$child, breeder, progressAFS)

  dta <- data.frame(afs0=afs0, afs1=afs1)
  return(dta)
})


output$evalGraphAFsHist<- renderPlotly({


  # plot
  dta <- afsEval()
  dta$afs1[dta$afs1==1] <- 0.9995 # get better display
  dta$afs1[dta$afs1==0] <- 0.0005 # get better display


  p <- plot_ly(alpha=0.6, colors=c("gray", "#009933")) %>%
        add_histogram(data = dta,
                      x = ~afs0,
                      color = "1",
                      name = "initial AFs",
                      xbins = list("start"=0, "end"=1.05, "size"=0.05 ),
                      inherit = TRUE) %>%
        add_histogram(data = dta,
                      x = ~afs1,
                      color = "2",
                      name = "final AFs",#paste0("AFs ", input$afsBreeder),
                      xbins = list("start"=0, "end"=1.05, "size"=0.05 ),
                      inherit = TRUE) %>%
        layout(barmode = "overlay",
               title = input$afsBreeder,
               xaxis = list(title="allele frequencies"))

})

output$evalGraphAFsScatter<- renderPlotly({

  ids <- sample(row.names(afsEval()), 5000)
  dta <- afsEval()[ids,]
  p <- plot_ly(type = 'scatter') %>%
       add_markers(data=dta,
                type = 'scatter',
                y = ~afs1,
                x= ~afs0,
                marker=list(color="#009933" , size=5 , opacity=0.3),
                text="",
                inherit=FALSE) %>%
    layout(title = input$afsBreeder,
           xaxis = list(title="initial allele frequencies"),
           yaxis = list(title="final allele frequencies"))

})


## pedigree ----
output$evalUIpedigree <- renderUI({
  if (exists("dfPhenoEval")){
    breeders <- unique(dfPhenoEval()$breeder)
    breeders <- breeders[breeders!="control"]
    list(
      selectInput("pedigreeBreeder","Breeder", choices=breeders),
      div(
        plotOutput("evalPlotPedigree",width = "800px", height = "800px")
      )
      )

  } else { p('no input')}

})


genealogy <- reactive({
  breeders <- unique(dfPhenoEval()$breeder)
  breeders <- breeders[breeders!="control"]

  gene <- lapply(breeders, function(b) {
    # extract all individuals
    db <- dbConnect(SQLite(), dbname=setup$dbname)
    query <- paste0("SELECT * FROM plant_material_", b)
    allInds <- (dbGetQuery(conn=db, query))
    dbDisconnect(db)

    # get submitted individuals
    inds <- readQryEval()$ind[readQryEval()$breeder==b]
    subsetPedigree(allInds, inds)
  })
  names(gene) <- breeders
  gene
})


output$evalPlotPedigree <- renderPlot({
  plotPedigree(genealogy()[[input$pedigreeBreeder]]$child,
               genealogy()[[input$pedigreeBreeder]]$parent1,
               genealogy()[[input$pedigreeBreeder]]$parent2,
               genealogy()[[input$pedigreeBreeder]]$generation,
               edge.col.mother = "black",
               edge.col.father = "black",
               vertex.label.color = "darkgreen",
               vertex.size = 15,
               main = input$pedigreeBreeder)
})




## Additive relationships ----
output$evalUIaddRelation <- renderUI({
    if (exists("dfPhenoEval")){
        breeders <- unique(dfPhenoEval()$breeder)
        breeders <- breeders[breeders!="control"]
        list(
            selectInput("addRelBreeder","Breeder", choices=breeders),
            tableOutput("addRelTable")
        )

    } else { p('no input')}

})

output$addRelTable <- renderTable({
    calcAdditiveRelation(breeder=input$addRelBreeder,
                         query = readQryEval(),
                         setup=setup,
                         constants = constants)

}, rownames = TRUE, spacing ="s", digits = 3)






## requests history ----
output$evalUIrequestHistory <- renderUI({
    if (exists("dfPhenoEval")){
        breeders <- unique(dfPhenoEval()$breeder)
        breeders <- breeders[breeders!="control"]
        list(
            selectInput("historyBreeder","Breeder", choices=c(breeders, "--- All Breeders ---")),
            # dataTableOutput("historyTable")
            plotlyOutput("historyTable")
        )

    } else { p('no input')}

})

breederHistory <- reactive({
  breeders <- unique(dfPhenoEval()$breeder)
  breeders <- breeders[breeders!="control"]

  breedHist <- lapply(breeders, function(b){
    getBreederHistory(breeder=b,
                      setup=setup)
  })
  names(breedHist) <- breeders
  breedHist
})

breederHistoryTimeLines <- reactive({

  p <- list()

  for (breeder in c("--- All Breeders ---", names(breederHistory()))) {
    if (breeder != "--- All Breeders ---") {
      dta <- breederHistory()[[breeder]]
      optY <- FALSE
      lw <- 25
    } else {
      dta <- do.call(rbind, breederHistory())
      optY <- FALSE #TRUE
      lw <- 15
    }

    names(dta) <- c("group", "task", "quantity", "start")
    dta$content <- paste(dta$task, "quantity:", dta$quantity)
    dta$duration <- 0
    dta$color <- NA

    colorGenoHD <- "#2c82e6"
    colorGenoLD <- "#42cbf5"
    colorGenoSin <- "#42cbf5"
    colorAllof <- "#47db25"
    colorAutof <- "#47db25"
    colorHaplo <- "#47db25"
    colorPhenoF <- "#ed8b3b"
    colorPhenoP <- "#ed9e5c"

    dta$duration[dta$task == "geno-hd"] <- constants$duration.geno.hd
    dta$color[dta$task == "geno-hd"] <- colorGenoHD

    dta$duration[dta$task == "geno-ld"] <- constants$duration.geno.ld
    dta$color[dta$task == "geno-ld"] <- colorGenoLD

    dta$duration[dta$task == "geno-single-snp"] <- constants$duration.geno.single
    dta$color[dta$task == "geno-single-snp"] <- colorGenoSin

    dta$duration[dta$task == "allofecundation"] <- constants$duration.allof
    dta$color[dta$task == "allofecundation"] <- colorAllof

    dta$duration[dta$task == "autofecundation"] <- constants$duration.autof
    dta$color[dta$task == "autofecundation"] <- colorAutof

    dta$duration[dta$task == "haplodiploidization"] <- constants$duration.haplodiplo
    dta$color[dta$task == "haplodiploidization"] <- colorHaplo

    dta$duration[dta$task == "allofecundation"] <- constants$duration.allof
    dta$color[dta$task == "allofecundation"] <- colorAllof

    dta$duration[dta$task == "pheno-field"] <- constants$duration.pheno.field
    dta$color[dta$task == "pheno-field"] <- colorPhenoF

    dta$duration[dta$task == "pheno-patho"] <- constants$duration.pheno.patho
    dta$color[dta$task == "pheno-patho"] <- colorPhenoP

    dta$end <- dta$start + months(dta$duration)


    p[[breeder]] <- vistime(dta,
                            col.event = "task",
                            col.start = "start",
                            col.end = "end",
                            col.group = "group",
                            col.color = "color",
                            # col.fontcolor = "fontcolor",
                            col.tooltip = "content",
                            optimize_y = optY,
                            linewidth = lw
    )
  }

  p

})
output$historyTable <- renderPlotly({
  breederHistoryTimeLines()[[input$historyBreeder]]
})


## Download Report ----
output$elvalReport <- downloadHandler(

  filename = function(){
    paste0("PlantBreedGame_report_",  format(Sys.time(), "%F_%H-%M-%S"), ".html")
  },

  content = function(file) {
    if (is.null(readQryEval())) {
      alert("Please run an evaluation before downloading the report.s")
      return(NULL)
    }
    # start progresse bar:
    progBar <- shiny::Progress$new(session, min = 0, max = 20)
    progBar$set(value = 0,
                message = "Report initialization",
                detail = "")

    allBV <- calcGameProgress(progBar)

    # parameters to pass to Rmd document
    params <- list(progressBar = progBar,
                   evalT1 = evalGraphT1(),
                   evalT2 = evalGraphT2(),
                   evalT3 = evalGraphT3(),
                   evalT1T2 = evalGraphT1vT2(),
                   genealogy = genealogy(),
                   breederHistoryTimeLines = breederHistoryTimeLines(),
                   BVdta = allBV,
                   selInds = readQryEval())
    # execute the R-markdown with given parameters
    rmarkdown::render("./src/evalReport.Rmd",
                      output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv()),
                      encoding = "UTF-8"
    )

    progBar$close() # close progress bar


  }
)


## debug ----
output$evalDebug <- renderPrint({
  print("---------")
  print(dfPhenoEval())


})

