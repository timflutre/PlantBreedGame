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




# 4 top box specifying:
#  - breeder
#  - Date
#  - Budget
#  - server status
output$stat_UIbreederInfo <- renderUI({
  if ( breeder() != "No Identification" ) {
    list(infoBoxOutput("breederBoxGeno", width = 3),
         infoBoxOutput("dateBoxGeno", width = 3),
         infoBoxOutput("budgetBoxGeno", width = 3),
         infoBoxOutput("serverIndicGeno", width = 3))
  }
})



# phenotypic data extraction
gwasPhenoDta <- eventReactive(input$gwas_phenoFile, {
  dta <- read.table(input$gwas_phenoFile$datapath,
                    header = T,
                    sep = "\t",
                    skip = "#",
                    stringsAsFactors = F)
})
stat_checkPhenoDta <- reactive({
  dta <- gwasPhenoDta()
  msg <- ""

  if (!"ind" %in% colnames(dta)) {
    msg <- c('ERROR:\nphenotypic file must contain a "ind" colunm')
  }
  if (!any(c("trait1", "trait2", "trait3") %in% colnames(dta))) {
    msg <- paste0(msg, '\nphenotypic file must contain "trait1" or "trait2" or "trait3" colunms')
  }
  
  out <- list()
  out$msg <- msg
  if (identical(msg, "")) {
    out$v <- TRUE
  } else out$v <- FALSE
  
  out
  
})

# genotypic data extraction
gwasGenoDta <- eventReactive(input$gwas_genoFile, {
  dta <- read.table(input$gwas_genoFile$datapath,
                    header = T,
                    sep = "\t",
                    skip = "#",
                    stringsAsFactors = F)
  dta <- as.matrix(dta)
  dta
})
stat_checkGenoDta <- reactive({
  dta <- gwasGenoDta()
  msg <- ""

  if (!is.numeric(dta)) {
    msg <- c('ERROR:\ngenotypic marker data must be numeric values')
  }

  out <- list()
  out$msg <- msg
  if (identical(msg, "")) {
    out$v <- TRUE
  } else out$v <- FALSE
  
  out
  
})



# data summary
output$stat_sumPheno <- renderPrint({
  if (stat_checkPhenoDta()$v) {
    dta <- gwasPhenoDta()
    nInd <- length(unique(dta$ind))
    
    cat(paste0("Number of individuals: ", nInd, "\n",
               "Number of observations: ", nrow(dta), "\n"))
    
    cat("\n\nFile previsualisation:\n\n")
    head(dta)
    
  } else {
    cat(stat_checkPhenoDta()$msg)
  }
})

output$stat_sumGeno <- renderPrint({
  if (stat_checkGenoDta()$v) {
    dta <- gwasGenoDta()
    cat(paste0("Number of individuals: ", nrow(dta), "\n",
              "Number of markers: ", ncol(dta), "\n"))
    
    cat("\n\nFile previsualisation: (6 lines, 5 columns)\n\n")
    head(dta)[, 1:min(5, ncol(dta))]
  } else {
    cat(stat_checkGenoDta()$msg)
  }
})

output$stat_sumGlobal <- renderPrint({

  if (stat_checkGenoDta()$v & stat_checkPhenoDta()$v) {
    # browser()
    options(width = 149) # TODO improve display
    dtaPheno <- gwasPhenoDta()
    dtaGeno <- gwasGenoDta()
    
    phenoInds <- unique(dtaPheno$ind)
    genoInds <- unique(row.names(dtaGeno))
    nCom <- sum(phenoInds %in% genoInds)
    nPneoNotGeno <- sum(!phenoInds %in% genoInds)
    nGeoNotPheno <- sum(!genoInds %in% phenoInds)
    
    cat(paste0(nCom, " individuals are in both phenotypic data and genotypic data.\nOnly these individuals will be considered for the analysis.\n\n",
               nPneoNotGeno, " individuals are in phenotypic data but not in genotypic data:\n"))
    print(phenoInds[which(!phenoInds %in% genoInds)])
    
    cat("\n\n")
    cat(paste0(nGeoNotPheno, " individuals are in genotypic data but not in phenotypic data:\n"))
    print(genoInds[which(!genoInds %in% phenoInds)])
    
    options(width = 76)  # TODO improve display
    
  }
  
  
  
})




# select trait
output$stat_gwasTraitUI <- renderUI({
  if (stat_checkPhenoDta()$v) {
    # browser()
    dta <- gwasPhenoDta()
    cols <- which(colnames(dta) %in% c("trait1", "trait2", "trait3"))
    selectInput("stat_gwasTrait",
                "Trait",
                choices = colnames(dta)[cols],
                selected = 1)
  }
})


# Calculation button
output$stat_gwasCalcButtonUI <- renderUI({
  
  msg <- ""
  style <- "background-color:green; color: white;"
  disabled <- FALSE
  if (!stat_checkPhenoDta()$v) {
    msg <- paste0(msg, stat_checkPhenoDta()$msg)
    style <- "background-color:red; color: white;"
    disabled <- TRUE
  }
  if (!stat_checkGenoDta()$v) {
    msg <- paste0(msg, "\n", stat_checkGenoDta()$msg)
    style <- "background-color:red; color: white;"
    disabled <- TRUE
  }
  
  button <- actionButton("stat_runGwasButton", "Calculation",
                    icon("play-circle"),
                    style = style)
  # disable button
  if (disabled) {
    button$attribs$disabled <- ""
  }
  
  div(
    button,
    # p(msg)
  )
  

  
})



observeEvent(input$stat_runGwasButton, {
  alert("Run GWAS")
})
