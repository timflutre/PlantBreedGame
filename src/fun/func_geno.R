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


## Contain functions used in "genotyping" section.

process_geno_request <- function(request_id, progressGeno = NULL) {
  # function which genotype the requested individuals (see game_master_pheno-geno.R)
  # create a result file in shared folder of the breeder

  # breeder (character) name of the breeder
  # inds.todo (data frame) output of "readCheckBreedDataFile"
  # gameTime ("POSIXlt") of the request (given by getGameTime function)


  request <- db_get_game_requests(id = request_id)
  stopifnot(request$type == "geno")

  breeder <- request$breeder
  request_time <- request$game_date
  geno_request_dta <- db_get_game_requests_data(id = request_id)

  constants <- getBreedingGameConstants()

  ## 0. load required data
  flush.console()
  f <- paste0(DATA_TRUTH, "/p0.RData")
  load(f)
  subset.snps <- getSNPsubset()


  ## 2. check that the requested individuals already exist
  genotyped_inds_ids <- unique(geno_request_dta$ind_id)
  genotyped_inds <- db_get_individual(ind_id = genotyped_inds_ids)
  geno_request_dta <- merge(geno_request_dta,
    genotyped_inds[, c("id", "name")],
    by.x = "ind_id", by.y = "id"
  )
  colnames(geno_request_dta)[colnames(geno_request_dta) == "name"] <- "ind_name"

  genotypes <- load_genotypes(inds_ids = genotyped_inds_ids, UIprogress = progressGeno)


  file_types <- c("ld", "hd", "singleSnp")
  geno_file_names <- as.list(
    paste0(
      DATA_SHARED, "/", breeder, "/",
      "Result_genos-", file_types, "_",
      request$name,
      ".txt.gz"
    ) # file name is unique as request$name is unique per breeder
  )
  names(geno_file_names) <- file_types


  ## 5. handle the 'geno' tasks for the requested individuals
  for (genotyping_density in c("ld", "hd")) {
    if (!is.null(progressGeno)) {
      progressGeno$set(
        value = 2,
        detail = paste0("Process ", genotyping_density)
      )
    }

    geno_request_dta_filtered <- geno_request_dta[geno_request_dta$type == genotyping_density, ]

    if (nrow(geno_request_dta_filtered) == 0) {
      next
    }

    filtered_genotype <- genotypes[geno_request_dta_filtered$ind_name,
      subset.snps[[genotyping_density]],
      drop = FALSE
    ]

    write.table(
      x = filtered_genotype,
      file = gzfile(geno_file_names[[genotyping_density]]), quote = FALSE,
      sep = "\t", row.names = TRUE, col.names = TRUE
    )
  }

  ## 6. handle the 'snp' tasks for the requested individuals
  geno_request_dta_single_snp <- geno_request_dta[!geno_request_dta$type %in% c("ld", "hd"), ]
  if (nrow(geno_request_dta_single_snp) > 0) {
    single_snp_genotypes <- data.frame(
      ind = geno_request_dta_single_snp$ind_name,
      snp = geno_request_dta_single_snp$type,
      geno = NA,
      stringsAsFactors = FALSE
    )

    single_snp_genotypes$geno <- mapply(
      function(i, s) genotypes[i, s],
      single_snp_genotypes$ind,
      single_snp_genotypes$snp
    )

    write.table(
      x = single_snp_genotypes,
      file = gzfile(geno_file_names$singleSnp), quote = FALSE,
      sep = "\t", row.names = FALSE, col.names = TRUE
    )
  }

  db_add_geno_data(
    geno_req_id = request_id,
    genotype_data_files = geno_file_names
  )
  db_update_request(id = request_id, processed = 1)
  # output
  return("done")
}



createInvoiceGeno <- function(request.df) {
  # function which create the corresponding invoice of a request
  # request.df (data.frame) of the request


  request.df$details[grep("*snp*", request.df$details)] <- "single-snp"

  # aggregate by geno-task
  invoice.geno <- aggregate(rep(1, nrow(request.df)) ~ details, data = request.df, sum)
  names(invoice.geno) <- c("Task", "Quantity")

  invoice.geno$Task <- paste0("geno-", invoice.geno$Task)


  # get prices
  constants <- getBreedingGameConstants()
  prices <- list(
    "geno-hd" = constants$cost.geno.hd * constants$cost.pheno.field,
    "geno-ld" = round(constants$cost.geno.ld * constants$cost.pheno.field, 2),
    "geno-single-snp" = constants$cost.geno.single * constants$cost.pheno.field
  )
  invoice.geno$Unitary_Price <- as.vector(as.numeric(prices[invoice.geno$Task]))
  invoice.geno$Total <- invoice.geno$Unitary_Price * invoice.geno$Quantity



  ## create invoice:

  invoice <- rbind(
    invoice.geno,
    data.frame(
      Task = "Total",
      Quantity = "",
      Unitary_Price = "",
      Total = sum(invoice.geno$Total)
    )
  )
  invoice <- invoice[c("Task", "Unitary_Price", "Quantity", "Total")]

  return(invoice)
}



load_genotypes <- function(inds_ids, UIprogress = NULL, add_breeder_to_inds_names = FALSE) {
  # return the genotypes of the given individuals as a matrix:
  # one row per individuals
  # one column per SNP
  # row names are the individuals names
  inds <- db_get_individual(ind_id = inds_ids)
  n_inds <- nrow(inds)

  genotypes <- matrix(
    nrow = n_inds,
    ncol = getBreedingGameConstants()$nb.snps
  )
  inds_names <- inds$name
  if (add_breeder_to_inds_names) {
    inds_names <- paste(inds$breeder, inds$name, sep = "*")
  }
  rownames(genotypes) <- inds_names

  for (i in 1:n_inds) {
    ind_name <- inds$name[i]
    if (add_breeder_to_inds_names) {
      ind_name <- paste(inds$breeder[i], inds$name[i], sep = "*")
    }

    if (!is.null(UIprogress)) {
      UIprogress$set(
        value = 1, # TODO: to be calculated using the current "value" +1
        detail = paste0("Load haplotypes: ", paste0(i, "/", n_inds, " ", ind_name))
      )
    }
    haplotype_file <- inds$haplotype_file[i]
    if (!file.exists(haplotype_file)) {
      stop(paste0("Haplotype file of ", ind_name, " doesn't exist"))
    }
    load(haplotype_file) # load the `ind` variable
    ind$genos <- segSites2allDoses(seg.sites = ind$haplos, ind.ids = ind_name)
    genotypes[ind_name, ] <- ind$genos
  }
  colnames(genotypes) <- colnames(ind$genos)
  return(genotypes)
}
