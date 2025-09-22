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


## functions for the "id part"

availToDwnld <- function(fileName, gameTime) {
  # function to check if files are available to download
  # fileName (char) name of the file
  # gameTime ("POSIXlt") (given by getGameTime function)

  stopifnot(
    is.character(fileName),
    fileName != ""
  )

  # quick Return
  initFiles <- list.files(DATA_INITIAL_DATA)
  if (fileName %in% initFiles) {
    res <- list()
    res$isAvailable <- TRUE
    res$availDate <- NULL
    return(res)
  }


  # get the date when the file was requested
  m <- regexpr("[0-9]{4}[-][0-9]{2}[-][0-9]{2}", fileName)
  requestDate <- strptime(regmatches(fileName, m), format = "%Y-%m-%d")


  constants <- getBreedingGameConstants()

  # calculate the available date
  if (grepl("pheno-field", fileName)) {
    maxDate <- strptime(paste0(data.table::year(requestDate), "-", constants$max.upload.pheno.field), format = "%Y-%m-%d")
    availDate <- seq(from = maxDate, by = paste0(constants$duration.pheno.field, " month"), length.out = 2)[2]
    if (requestDate > maxDate) {
      availDate <- seq(from = availDate, by = "1 year", length.out = 2)[2]
    }
  } else if (grepl("pheno-patho", fileName)) {
    availDate <- seq(from = requestDate, by = paste0(constants$duration.pheno.patho, " month"), length.out = 2)[2]
  } else if (grepl("genos-single-snps", fileName)) {
    availDate <- seq(from = requestDate, by = paste0(constants$duration.geno.single, " month"), length.out = 2)[2]
  } else if (grepl("genos-hd", fileName)) {
    availDate <- seq(from = requestDate, by = paste0(constants$duration.geno.hd, " month"), length.out = 2)[2]
  } else if (grepl("genos-ld", fileName)) {
    availDate <- seq(from = requestDate, by = paste0(constants$duration.geno.ld, " month"), length.out = 2)[2]
  } else {
    (stop())
  }

  # results
  res <- list()
  res$isAvailable <- availDate <= gameTime
  res$availDate <- availDate

  return(res)
}





#' Convert txt genotype to vcf genotype
#'
#' @param txt_file path of the `.txt(.gz)` genotype data file
#' @param file path of the new `.vcf.gz` genotype data file
#'
#' @return NULL
txt2Vcf <- function(txt_file, vcf_file, prog = NULL) {
  # read genotype data
  if (!is.null(prog)) {
    prog$set(
      value = 0,
      detail = "Get genotype data..."
    )
  }
  geno <- read.table(txt_file,
    header = TRUE,
    sep = "\t"
  )
  geno <- data.frame(lapply(geno, as.character),
    row.names = row.names(geno)
  )

  if (identical(colnames(geno), c("ind", "snp", "geno"))) {
    # single snp genotyping results
    geno <- tidyr::spread(geno, key = "snp", value = "geno")
    row.names(geno) <- geno$ind
    geno <- subset(geno, select = -1)
  }

  # read snp coordinates
  if (!is.null(prog)) {
    prog$set(
      value = 1,
      detail = "Get SNP coordinates..."
    )
  }
  snpCoord <- read.table(file.path(DATA_INITIAL_DATA, "snp_coords_hd.txt.gz"))
  snpCoord <- snpCoord[row.names(snpCoord) %in% colnames(geno), ]

  # Create the VCF "Fixed region"
  if (!is.null(prog)) {
    prog$set(
      value = 2,
      detail = "Create VCF fixed region..."
    )
  }
  fixedColNames <- c(
    "#CHROM",
    "POS",
    "ID",
    "REF",
    "ALT",
    "QUAL",
    "FILTER",
    "INFO",
    "FORMAT"
  )
  data <- as.data.frame(matrix(NA,
    nrow = ncol(geno),
    ncol = nrow(geno) + length(fixedColNames)
  ))
  colnames(data)[1:length(fixedColNames)] <- fixedColNames
  colnames(data)[seq(length(fixedColNames) + 1, ncol(data))] <- row.names(geno)
  data$`#CHROM` <- snpCoord$chr
  data$POS <- snpCoord$pos
  data$ID <- row.names(snpCoord)
  data$REF <- "A"
  data$ALT <- "T"
  data[, c("QUAL", "INFO")] <- "."
  data$FILTER <- "PASS"
  data$FORMAT <- "GT"

  data <- data[order(data$POS), ]
  data <- data[order(data$`#CHROM`), ]



  # Genotype region
  if (!is.null(prog)) {
    prog$set(
      value = 3,
      detail = "Create VCF genotype region..."
    )
  }
  gt <- matrix(NA, nrow = nrow(geno), ncol = ncol(geno))
  gt[geno == "0"] <- "0/0"
  gt[geno == "1"] <- "1/0"
  gt[geno == "2"] <- "1/1"
  gt[is.na(geno)] <- "./."
  colnames(gt) <- colnames(geno)
  row.names(gt) <- row.names(geno)
  gt <- t(gt)
  data[, colnames(gt)] <- gt[data$ID, ]


  # Meta region
  meta <- paste("##fileformat=VCFv4.3",
    '##source="PlantBreedGame", data in this file are simulated.',
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    sep = "\n"
  )

  # write file
  if (!is.null(prog)) {
    prog$set(
      value = 4,
      detail = "Write VCF File..."
    )
  }
  if (file.exists(vcf_file)) {
    file.remove(vcf_file)
  }

  f <- gzfile(vcf_file, "w")
  writeLines(text = meta, con = f)
  close(f)
  data.table::fwrite(
    x = data,
    file = vcf_file,
    append = TRUE,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )
}
