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
source("src/func_geno.R", local=TRUE, encoding = "UTF-8")$value


## server for "genotyping"

# read uploaded file
readQryGeno <- reactive({
  if(is.null(input$file.geno))
    return(NULL)
  test <-  try(df <- readCheckBreedDataFileJD(input$file.geno$datapath, subset.snps=subset.snps, breeder=breeder()))
  if (is.data.frame(test)){
    df <- df[df$task == "geno",]
    return(df)
  }else {return("error")}
})



# check
output$GenoUploaded <- renderPrint({
  if (is.data.frame(readQryGeno())){
    print("GOOD")
  } else if (is.null(readQryGeno())){
    print("No file uploaded")
  } else print("Not good")

})



# summary
output$GenoSmy <- renderPrint({
  if (is.data.frame(readQryGeno())){
    summary(as.data.frame(apply(readQryGeno(), MARGIN = 2, FUN = as.factor)))
  }
})
output$GenoStr <- renderPrint({
  if (is.data.frame(readQryGeno())){
    print(str(readQryGeno()))
  }
})


# data
output$qryGeno <- renderTable({
  if (is.data.frame(readQryGeno())){
    readQryGeno()
  }
})



# output
geno_data <- reactive({
  if (is.data.frame(readQryGeno())){
    genotype(breeder(), readQryGeno(), year)
  }
})




output$dwnlGenold <- downloadHandler(
  filename=geno_data()$filename.ld,
  content=function(file){
    write.table(geno_data()$df.ld,
                file=gzfile(file), quote=FALSE,
                sep="\t", row.names=TRUE, col.names=TRUE)
  }
)

output$dwnlUIGenold <- renderUI({
  if (is.data.frame(geno_data()$df.ld)){
    downloadButton("dwnlGenold", "Download the output file")
  } else p("No request for low-density genotyping.")

})


output$dwnlGenohd <- downloadHandler(
  filename=geno_data()$filename.hd,
  content=function(file){
    write.table(geno_data()$df.hd,
                file=gzfile(file), quote=FALSE,
                sep="\t", row.names=TRUE, col.names=TRUE)
  }
)

output$dwnlUIGenohd <- renderUI({
  if (is.data.frame(geno_data()$df.hd)){
    downloadButton("dwnlGenohd", "Download the output file")
  } else p("No request for high-density genotyping.")

})



output$dwnlGenosnp <- downloadHandler(
  filename=geno_data()$filename.snp,
  content=function(file){
    write.table(geno_data()$df.snp,
                file=gzfile(file), quote=FALSE,
                sep="\t", row.names=TRUE, col.names=TRUE)
  }
)

output$dwnlUIGenosnp <- renderUI({
  if (is.data.frame(geno_data()$df.snp)){
    downloadButton("dwnlGenosnp", "Download the output file")
  } else p("No request for single-SNP genotyping.")

})




# DEBUG

output$GenoDebug <- renderPrint({
  print(readCheckBreedDataFileJD(input$file.geno$datapath, subset.snps=subset.snps, breeder=breeder()))



})

