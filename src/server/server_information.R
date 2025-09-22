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


## server for information part



output$dwnlIniData <- downloadHandler(
  filename = function() input$iniDataFile, # lambda function
  content = function(file) {
    filePath <- file.path(DATA_INITIAL_DATA, input$iniDataFile)
    file.copy(filePath, file)
  }
)

output$costSummaryTable <- renderUI({
  c <- constantsReactive()

  costAllof <- format(c$cost.allof * c$cost.pheno.field, digits = 2)
  costAutof <- format(c$cost.autof * c$cost.pheno.field, digits = 2)
  costHaplo <- format(c$cost.haplodiplo * c$cost.pheno.field, digits = 2)

  costGenoHD <- format(c$cost.geno.hd * c$cost.pheno.field, digits = 2)
  costGenoLD <- format(c$cost.geno.ld * c$cost.pheno.field, digits = 2)
  costGenoSNP <- format(c$cost.geno.single * c$cost.pheno.field, digits = 2)

  costPhenoField <- format(c$cost.pheno.field, digits = 2)
  costPhenoPatho <- format(c$cost.pheno.patho * c$cost.pheno.field, digits = 2)

  costRegistration <- format(c$cost.register * c$cost.pheno.field, digits = 2)

  style_pltmat <- "background-color: #ffcccb;"
  style_pheno <- "background-color: #c2f0c2;"
  style_geno <- "background-color: #ffedcc;"
  style_registration <- "background-color: #e6f7ff;"

  div(
    style = "text-align: center;",
    tags$table(
      border = 2, style = "width:100%; text-align:center; max-width: 75em",
      tags$tr(
        tags$th("Request type", style = "text-align:center"),
        tags$th("Request detail", style = "text-align:center"),
        tags$th("Cost (Mendels)", style = "text-align:center"),
        tags$th("Duration (months)", style = "text-align:center"),
        tags$tr(
          style = style_pltmat,
          tags$td(rowspan = 3, "Plant Material"),
          tags$td("Allofecundation"), tags$td(costAllof), tags$td(c$duration.allof)
        ),
        tags$tr(
          style = style_pltmat,
          tags$td("Autofecundation"), tags$td(costAutof), tags$td(c$duration.autof)
        ),
        tags$tr(
          style = style_pltmat,
          tags$td("Haplodiploidisations"), tags$td(costHaplo), tags$td(c$duration.haplodiplo)
        ),
        tags$tr(
          style = style_pheno,
          tags$td(rowspan = 2, "Phenotyping"),
          tags$td("Field"), tags$td(costPhenoField), tags$td(c$duration.pheno.field)
        ),
        tags$tr(
          style = style_pheno,
          tags$td("Greenhouse"), tags$td(costPhenoPatho), tags$td(c$duration.pheno.patho)
        ),
        tags$tr(
          style = style_geno,
          tags$td(rowspan = 3, "Genotyping"),
          tags$td("High density"), tags$td(costGenoHD), tags$td(c$duration.geno.hd)
        ),
        tags$tr(
          style = style_geno,
          tags$td("Low density"), tags$td(costGenoLD), tags$td(c$duration.geno.ld)
        ),
        tags$tr(
          style = style_geno,
          tags$td("Single SNP"), tags$td(costGenoSNP), tags$td(c$duration.geno.single)
        ),
        tags$tr(
          style = style_registration,
          tags$td("Registration"), tags$td(""), tags$td(costRegistration), tags$td("Instant")
        )
      )
    )
  )
})




## Debug
output$infoDebug <- renderPrint({
  print("----")
  print(input$iniDataFile)
  print(class(input$iniDataFile))
})
