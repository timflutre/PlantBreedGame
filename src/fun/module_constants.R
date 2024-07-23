# Showing DB values in the UI is not so straitforward, as
# each values needs its own id and "outputs".
# This define a "shiny modules" that helps for that.


constants_ui <- function(id) {
  ns <- NS(id)
  textOutput(ns("value"), inline = TRUE)
}

constants_server <- function(const, constantsReactive) {
  function(id) {
    moduleServer(id, function(input, output, session) {
      output$value <- renderText({
        default <- "NA"
        tryCatch(
          {
            constants <- constantsReactive()
            if (const == "generations.per.year") {
              return(12 / constants$duration.allof)
            }
            if (const == "register.min.trait1.percent") {
              return(100 * constants$register.min.trait1)
            }
            if (const == "cost.register.mendels") {
              return(format(constants$cost.register * constants$cost.pheno.field, digits = 2))
            }
            if (const == "initial.budget") {
              return(format(constants$initialBudget, digits = 2, scientific = F))
            }
            if (const == "cost.geno.single.mendels") {
              return(format(constants$cost.geno.single * constants$cost.pheno.field, digits = 2))
            }
            if (const == "cost.geno.ld.mendels") {
              return(format(constants$cost.geno.ld * constants$cost.pheno.field, digits = 2))
            }
            if (const == "cost.geno.hd.mendels") {
              return(format(constants$cost.geno.hd * constants$cost.pheno.field, digits = 2))
            }
            if (const == "cost.haplodiplo.mendels") {
              return(format(constants$cost.haplodiplo * constants$cost.pheno.field, digits = 2))
            }

            if (const == "cost.autof.mendels") {
              return(format(constants$cost.autof * constants$cost.pheno.field, digits = 2))
            }
            if (const == "cost.allof.mendels") {
              return(format(constants$cost.allof * constants$cost.pheno.field, digits = 2))
            }
            if (const == "max.upload.pheno.field") {
              return(format(as.Date(constants$max.upload.pheno.field, format = "%m-%d"), "%B %d"))
            }
            if (const == "chr.length.Mb") {
              return(format(constants$chr.length / 10^6, digits = 2))
            }

            if (const == "duration.allof") {
              return(format(12 / constants$duration.allof,
                digits = 2
              ))
            }

            if (const == "pheno.data.availability.date") {
              max.upload.pheno.field <- constants$max.upload.pheno.field
              duration.pheno.field <- constants$duration.pheno.field
              return(format(seq.Date(as.Date(max.upload.pheno.field, format = "%m-%d"),
                length = 2,
                by = paste0(duration.pheno.field, " months")
              )[2], "%B %d"))
            }
            if (const == "cost.pheno.patho.mendels") {
              return(round(constants$cost.pheno.patho * constants$cost.pheno.field, 2))
            }
            if (const == "max.upload.pheno.field") {
              return(format(as.Date(constants$max.upload.pheno.field, format = "%m-%d"), "%B %d"))
            }
            if (const %in% names(constants)) {
              return(constants[[const]])
            }
            return(default)
          },
          error = function(err) {
            return(default)
          }
        )
      })
    })
  }
}
