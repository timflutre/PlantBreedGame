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



#### data extraction ####
# phenotypic data extraction
# gwasPhenoDta <- eventReactive(input$gwas_phenoFile, {
gwasPhenoDta <- reactive({
  if (is.null(input$gwas_phenoFile$datapath)) return(NULL)
  dta <- read.table(input$gwas_phenoFile$datapath,
                    header = T,
                    sep = "\t",
                    skip = "#",
                    stringsAsFactors = F)
})

stat_checkPhenoDta <- reactive({
  dta <- gwasPhenoDta()

  # quick return if no file are loaded
  if (is.null(dta)) {
    out <- list()
    out$msg <- 'No phenotypic data file imported'
    out$v <- FALSE
    return(out)
  }

  msg <- "ERROR: "

  if (!"ind" %in% colnames(dta)) {
      msg <- paste(msg, 'phenotypic file must contain a "ind" colunm.', sep = "\n")
  }
  if (!any(c("trait1", "trait2", "trait3") %in% colnames(dta))) {
    msg <- paste(msg, 'phenotypic file must contain "trait1" or "trait2" or "trait3" colunms.', sep = "\n")
  }
  if (nrow(dta) > constants$max.gwas.obs) {
    # to limit computation time on the server.
    msg <- paste(msg, paste('The file contains more than', constants$max.gwas.obs, 'observations.'), sep = "\n")
  }

  out <- list()
  out$msg <- msg
  if (identical(msg, "ERROR: ")) {
    out$v <- TRUE
  } else out$v <- FALSE

  out

})

# genotypic data extraction
# gwasGenoDta <- eventReactive(input$gwas_genoFile, {
gwasGenoDta <- reactive({
  if (is.null(input$gwas_genoFile$datapath)) return(NULL)
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

  # quick return if no file are loaded
  if (is.null(dta)) {
    out <- list()
    out$msg <- 'No genotypic data file imported'
    out$v <- FALSE
    return(out)
  }


  msg <- "ERROR: "
  if (!is.numeric(dta)) {
    msg <- paste(msg, 'genotypic marker data must be numeric values.', sep = "\n")
  } else if (!all(dta %in% c(0,1,2))) {
    msg <- paste(msg, 'genotypic marker data must be encoded in allel dose (0, 1, 2)', sep = "\n")
  }

  out <- list()
  out$msg <- msg
  if (identical(msg, "ERROR: ")) {
    out$v <- TRUE
  } else out$v <- FALSE

  out

})




#### data summary ####
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


#### NEXT button  ####
output$stat_gwasNextButtonUI <- renderUI({

  msg <- ""
  style <- "background-color:green; color: white;"
  disabled <- FALSE
  if (!stat_checkPhenoDta()$v) {
    # msg <- paste0(msg, stat_checkPhenoDta()$msg)
    style <- "background-color:red; color: white;"
    disabled <- TRUE
  }
  if (!stat_checkGenoDta()$v) {
    # msg <- paste0(msg, "\n", stat_checkGenoDta()$msg)
    style <- "background-color:red; color: white;"
    disabled <- TRUE
  }
  if (stat_checkGenoDta()$v & stat_checkPhenoDta()$v) {
    dtaPheno <- gwasPhenoDta()
    dtaGeno <- gwasGenoDta()

    phenoInds <- unique(dtaPheno$ind)
    genoInds <- unique(row.names(dtaGeno))
    nCom <- sum(phenoInds %in% genoInds)
    if (nCom == 0) {
      msg <- paste0(msg, "\n", "There is no common individuals between phenotype an genotype datasets.")
      style <- "background-color:red; color: white;"
      disabled <- TRUE
    }
  }

  button <- actionButton("stat_nextGwasButton", "Next",
                    icon("play-circle"),
                    style = style)
  # disable button
  if (disabled) {
    button$attribs$disabled <- ""
  }

  div(
    button,
    p(style = "color:red;", msg)
  )


})

observeEvent(input$stat_nextGwasButton, {
  updateTabsetPanel(session, "tabGWAS", selected = "2")
})



#### Model parametrization ####
output$stat_gwasVarUI <- renderUI({

  # test if pheno and geno is good
  if (stat_checkPhenoDta()$v & stat_checkGenoDta()$v) {

    # import pheno data
    dtaPheno <- gwasPhenoDta()

    selectInput("stat_gwasVar",
                label = "Variable of interest",
                choices = colnames(dtaPheno)[colnames(dtaPheno) != "ind"],
                selected = 1,
                multiple = FALSE)
  } else {
    p("please import data")
  }
})

output$stat_gwasFixedUI <- renderUI({

  # test if pheno and geno is good
  if (stat_checkPhenoDta()$v & stat_checkGenoDta()$v) {
    # import pheno data
    dtaPheno <- gwasPhenoDta()
    selectInput("stat_gwasFixed",
                label = "Fixed effects",
                choices = colnames(dtaPheno)[colnames(dtaPheno) != "ind"],
                multiple = TRUE)
  }
})

#### Calc button  ####
output$stat_gwasCalcButtonUI <- renderUI({

  msg <- ""
  style <- "background-color:green; color: white;"
  disabled <- FALSE
  if (!stat_checkPhenoDta()$v) {
    msg <- paste(msg, stat_checkPhenoDta()$msg, sep = "<br>")
    style <- "background-color:red; color: white;"
    disabled <- TRUE
  }
  if (!stat_checkGenoDta()$v) {
    msg <- paste(msg, stat_checkGenoDta()$msg, sep = "<br>")
    style <- "background-color:red; color: white;"
    disabled <- TRUE
  }
  # browser()
  if (stat_checkGenoDta()$v & stat_checkPhenoDta()$v) {
    dtaPheno <- gwasPhenoDta()
    dtaGeno <- gwasGenoDta()

    phenoInds <- unique(dtaPheno$ind)
    genoInds <- unique(row.names(dtaGeno))
    nCom <- sum(phenoInds %in% genoInds)
    if (nCom == 0) {
      msg <- paste(msg, "There is no common individuals between phenotype an genotype datasets.", sep = "<br>")
      style <- "background-color:red; color: white;"
      disabled <- TRUE
    }
  }
  if (breeder() == "No Identification") {
    msg <- paste(msg, "You need to identify yourself in order to fit the model.", sep = "<br>")
    style <- "background-color:red; color: white;"
    disabled <- TRUE
  }

  msg <- gsub("\n", "<br>", msg)

  button <- actionButton("stat_calcGwasButton", "Calculation",
                         icon("play-circle"),
                         style = style)

  # disable button
  if (disabled) {
    button$attribs$disabled <- ""
  }

  div(
    button,
    p(style = "color:red;", HTML(msg))

  )


})

#### Model fitting ####
modelGWAS <- eventReactive(input$stat_calcGwasButton, {
  # browser()
  # load data
  dtaPheno <- gwasPhenoDta()
  dtaGeno <- gwasGenoDta()

  ## check "stat_gwasVar" and "stat_gwasFixed"

  ### prepare data for analysis

  # filter pheno
  # dtaPheno <- dtaPheno[dtaPheno$pathogen, ]
  dtaPheno <- dtaPheno[, c("ind", input$stat_gwasVar, input$stat_gwasFixed)]
  # dtaPheno <- dtaPheno[, c("ind", "trait1", "trait2", "trait3", )]#, input$stat_gwasFixed)]
  # dtaPheno <- dtaPheno[, c("ind", "trait1", "pathogen", "year")]
  # keep only common individuals
  phenoInds <- unique(dtaPheno$ind)
  genoInds <- unique(row.names(dtaGeno))
  comInds <- phenoInds[phenoInds %in% genoInds]

  dtaPheno <- dtaPheno[dtaPheno$ind %in% comInds, ]
  dtaGeno <- dtaGeno[comInds, ]


  # prepare geno
  # genotype encoded as -1,0,1
  # dtaGeno <- dtaGeno - 1
  # keep phenotyped individuals
  dtaGeno <- dtaGeno[dtaPheno$ind,]

  # load SNP coordinates
  snpCoordHD <- read.table("data/shared/initial_data/snp_coords_hd.txt.gz",
                           stringsAsFactors = FALSE)
  snpCoordHD$marker <- rownames(snpCoordHD)
  snpCoordHD <- snpCoordHD[,c("marker", "chr", "pos")] # reorder columns
  snpCoordHD <- snpCoordHD[snpCoordHD$marker %in% colnames(dtaGeno),]
  snpCoordHD <- snpCoordHD[colnames(dtaGeno),] # reorder lines

  # merge snpCoord and geno
  dtaGeno <- cbind(snpCoordHD, t(dtaGeno)) # merge snpCoord and geno


  GWAS(pheno = dtaPheno,
       geno = dtaGeno,
       fixed = input$stat_gwasFixed,
       plot = TRUE)

  # GWAS(pheno = dtaPheno,
  #      geno = dtaGeno,
  #      # fixed = "year",
  #      plot = TRUE)



})



#### Manhathan plot ####
output$gwas_plot <- renderPlotly({
  mod <- modelGWAS()
  # browser()
  colnames(mod)[4] <- "LogP"
  mod$plotPos <- mod$pos
  offset <- 0

  # change "chr1" to "chr01"
  mod$chr <- paste0("chr",
                    sprintf("%02i", as.numeric(gsub("chr", "", mod$chr))))
  for (chr in sort(unique(mod$chr))) {
    mod[mod$chr == chr,]$plotPos <- mod[mod$chr == chr,]$plotPos + offset
    offset <- offset + max(mod[mod$chr == chr,"pos"])
  }

  alpha <- max(input$stat_gwasAlpha, 1e-12)

  # browser()

  plot_ly(data =  group_by(mod, chr),
          type = 'scatter',
          mode = 'markers',
          x = ~plotPos,
          y = ~LogP,
          # y =~-log10(p.adjust(exp(-LogP), method = "bonferroni")),
          yaxis = ~LogP,
          color = ~chr,
          groups = ~chr,
          hoverinfo = 'text',
          text = ~paste0("Marker: ", marker, "\n",
                         "Chr: ", chr, "\n",
                         "Position: ", pos, "\n",
                         "-log<sub>10</sub>(p-value): ", round(LogP, 2))
  )   %>%
    layout(
      title = "Manhattan Plot",
      xaxis = list(title = "Position",
                   range = c(0 - max(mod$plotPos)*0.02, max(mod$plotPos)*1.02),
                   zeroline = FALSE,
                   showticklabels = FALSE),
      yaxis = list(title = "-log<sub>10</sub>(p-value)"),
      margin = list(
        l = 50,
        r = 50,
        b = 50,
        t = 100,
        pad = 10
      ),
      shapes = list(
        type = "line",
        line = list(color = "red"),
        x0 = min(mod$plotPos),
        x1 = max(mod$plotPos),
        y0 = -log10(alpha/nrow(mod)),
        y1 = -log10(alpha/nrow(mod)))
    ) %>%
    add_annotations(x = 1,
                    y = -log10(alpha/nrow(mod)),
                    text = paste("Bonferroni\n\U03B1 =", alpha),
                    xref = "paper",
                    yref = "y",
                    xanchor = "left",
                    yanchor = "center",
                    yanchor = "center",
                    showarrow = FALSE,
                    arrowhead = 0,
                    arrowsize = 0,
                    # Styling annotations' text:
                    font = list(color = 'red',
                                size = 14))
})
