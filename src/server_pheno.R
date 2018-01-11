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
source("src/func_pheno.R", local=TRUE, encoding = "UTF-8")$value


## server for "phenotyping"

# read uploaded file
readQryPheno <- reactive({
  if(is.null(input$file.pheno))
    return(NULL)
  test <-  try(df <- readCheckBreedDataFileJD(input$file.pheno$datapath, subset.snps=subset.snps, breeder=breeder()))
  if (is.data.frame(test)){
    df <- df[df$task == "pheno",]
    return(df)
  }else {return("error")}
})



# check
output$PhenoUploaded <- renderPrint({
  if (is.data.frame(readQryPheno())){
    print("GOOD")
  } else if (is.null(readQryPheno())){
      print("No file uploaded")
  } else print("Not good")
  
})



# summary
output$PhenoSmy <- renderPrint({
  if (is.data.frame(readQryPheno())){
    summary(as.data.frame(apply(readQryPheno(), MARGIN = 2, FUN = as.factor)))
  }
})
output$PhenoStr <- renderPrint({
  if (is.data.frame(readQryPheno())){
    print(str(readQryPheno()))
  }
})


# data
output$qryPheno <- renderTable({
  if (is.data.frame(readQryPheno())){
    readQryPheno()
  }
})



# output
pheno_data <- reactive({
  if (is.data.frame(readQryPheno())){
    phenotype(breeder(), readQryPheno(), year)
  }
})



output$dwnlPheno <- downloadHandler(
  filename=pheno_data()$filename,
  content=function(file){
    write.table(pheno_data()$df,
                file=gzfile(file), quote=FALSE,
                sep="\t", row.names=FALSE, col.names=TRUE)
  }
)



output$dwnlUIPheno <- renderUI({
  if (is.data.frame(readQryPheno())){
    downloadButton("dwnlPheno", "Télécharger les résultats")
  } else p("Vérifiez votre fichier")
  
})




## DEBUG
# 
output$PhenoDebug <- renderPrint({
  print("--")

})

