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


## Contain functions used in "plant material" section.

process_plantmat_request <- function(request_id, progressPltMat = NULL) {
  # function which create the new generations (see game_master_plant-material.R)

  db_update_request(request_id,
    progress = 0.0001,
    inc_retry = TRUE
  )
  progress <- 0
  n_step <- 7


  request <- db_get_game_requests(id = request_id)
  stopifnot(request$type == "pltmat")

  breeder <- request$breeder
  pltmat_request_dta <- db_get_game_requests_data(id = request_id)


  ## check the presence of new individuals in the set of existing individuals
  existing_childs <- db_get_individual(breeder = breeder, name = pltmat_request_dta$child_name)
  stopifnot(nrow(existing_childs) == 0)


  ## get parent's informations
  progress <- progress + (1 / (n_step + 1))
  db_update_request(request_id, progress = progress)

  requested_parents <- db_get_individual(ind_id = unique(c(
    pltmat_request_dta$parent1_id,
    pltmat_request_dta$parent2_id
  )))
  pltmat_request_dta <- merge(pltmat_request_dta,
    requested_parents[, c("id", "name")],
    by.x = "parent1_id", by.y = "id"
  )
  colnames(pltmat_request_dta)[colnames(pltmat_request_dta) == "name"] <- "parent1" # new name respecting `drawLocCrossovers` convention
  pltmat_request_dta <- merge(pltmat_request_dta,
    requested_parents[, c("id", "name")],
    by.x = "parent2_id", by.y = "id"
  )
  colnames(pltmat_request_dta)[colnames(pltmat_request_dta) == "name"] <- "parent2" # new name respecting `drawLocCrossovers` convention
  colnames(pltmat_request_dta)[colnames(pltmat_request_dta) == "child_name"] <- "child" # new name respecting `drawLocCrossovers` convention


  # initialise parent haplotypes
  progress <- progress + (1 / (n_step + 1))
  db_update_request(request_id, progress = progress)

  load(requested_parents$haplotype_file[1]) # load the haplotype of one parent to get the general structure of the haplotypes
  parents_haplotypes <- list()
  for (chr.id in names(ind$haplos)) {
    ploidy <- nrow(ind$haplos[[chr.id]])
    parents_haplotypes[[chr.id]] <- matrix(
      data = NA,
      ncol = ncol(ind$haplos[[chr.id]]),
      nrow = nrow(requested_parents) * ploidy,
      dimnames = list(
        paste0(
          rep(requested_parents$name, each = ploidy),
          rep(paste0("_h", 1:ploidy), length(requested_parents$name))
        ),
        colnames(ind$haplos[[chr.id]])
      )
    )
  }


  progress <- progress + (1 / (n_step + 1))
  db_update_request(request_id, progress = progress)

  if (!is.null(progressPltMat)) {
    progressPltMat$inc(
      amount = 1,
    )
  }
  for (i in 1:nrow(requested_parents)) {
    parent_name <- requested_parents$name[i]
    if (!is.null(progressPltMat)) {
      progressPltMat$inc(
        amount = 1 / nrow(requested_parents),
        detail = paste0(
          "Load haplotypes: ",
          i, "/", nrow(requested_parents), ": ",
          parent_name
        )
      )
    }
    if ("ind" %in% ls()) {
      rm(ind)
    }
    f <- requested_parents$haplotype_file[i]
    if (!file.exists(f)) {
      stop(paste0("Haplotype file of ", parent_name, " doesn't exist"))
    }
    load(f)
    for (chr.id in names(parents_haplotypes)) {
      parents_haplotypes[[chr.id]][row.names(ind$haplos[[chr.id]]), ] <- ind$haplos[[chr.id]]
    }
  }

  ## perform the requested crosses
  progress <- progress + (1 / (n_step + 1))
  db_update_request(request_id, progress = progress)

  if (!is.null(progressPltMat)) {
    progressPltMat$inc(
      amount = 1,
      detail = "perform crosses..."
    )
  }

  new_inds_haplo <- list()
  loc.crossovers <- drawLocCrossovers(
    crosses = pltmat_request_dta[, c("parent1", "parent2", "child")],
    nb.snps = sapply(parents_haplotypes, ncol)
  )
  new_inds_haplo <- makeCrosses(
    haplos = parents_haplotypes,
    crosses = pltmat_request_dta[, c("parent1", "parent2", "child")],
    loc.crossovers = loc.crossovers, verbose = 0
  )
  rm(parents_haplotypes)


  ## save the haplotypes of the new individuals
  progress <- progress + (1 / (n_step + 1))
  db_update_request(request_id, progress = progress)

  haplotype_file_data <- data.frame(
    haplotype_file = rep(NA, nrow(pltmat_request_dta)),
    row.names = pltmat_request_dta$child
  )

  genotypes <- matrix(
    nrow = nrow(pltmat_request_dta),
    ncol = getBreedingGameConstants()$nb.snps
  )
  rownames(genotypes) <- pltmat_request_dta$child

  if (!is.null(progressPltMat)) {
    progressPltMat$inc(
      amount = 1,
    )
  }
  for (new.ind.id in getIndNamesFromHaplos(new_inds_haplo)) {
    if (!is.null(progressPltMat)) {
      progressPltMat$inc(
        amount = 1 / nrow(pltmat_request_dta),
        detail = paste0("Save haplotypes: ", new.ind.id)
      )
    }
    # message(new.ind.id)
    ind <- list(haplos = getHaplosInd(new_inds_haplo, new.ind.id))
    f <- paste0(DATA_TRUTH, "/", breeder, "/", new.ind.id, "_haplos.RData")
    save(ind, file = f)
    haplotype_file_data[new.ind.id, "haplotype_file"] <- f

    # genotype
    ind$genos <- segSites2allDoses(seg.sites = ind$haplos, ind.ids = new.ind.id)
    genotypes[new.ind.id, ] <- ind$genos
  }
  colnames(genotypes) <- colnames(ind$genos)

  progress <- progress + (1 / (n_step + 1))
  db_update_request(request_id, progress = progress)

  if (!is.null(progressPltMat)) {
    progressPltMat$inc(
      amount = 1,
      detail = "Finalisation..."
    )
  }

  db_add_pltmat(req_id = request_id)
  haplotype_file_data$id <- db_get_individuals_ids(
    breeder = breeder,
    names = row.names(haplotype_file_data)
  )
  db_update_pltmat(haplotype_file_data)

  progress <- progress + (1 / (n_step + 1))
  db_update_request(request_id, progress = progress)

  GV <- calculate_genetic_values(genotypes)
  GV$id <- db_get_individuals_ids(
    breeder = breeder,
    names = row.names(GV)
  )
  db_update_pltmat(GV)

  db_update_request(id = request_id, progress = 1)
}






createInvoicePltmat <- function(request.df) {
  # function which create the corresponding invoice of a request
  # request.df (data.frame) of the request


  # aggregate by explanations
  invoice.pltmat <- aggregate(rep(1, nrow(request.df)) ~ explanations, data = request.df, sum)
  names(invoice.pltmat) <- c("Task", "Quantity")


  # get prices
  constants <- getBreedingGameConstants()
  prices <- list(
    "allofecundation" = constants$cost.allof * constants$cost.pheno.field,
    "autofecundation" = constants$cost.autof * constants$cost.pheno.field,
    "haplodiploidization" = constants$cost.haplodiplo * constants$cost.pheno.field
  )

  invoice.pltmat$Unitary_Price <- as.vector(as.numeric(prices[invoice.pltmat$Task]))
  invoice.pltmat$Total <- invoice.pltmat$Unitary_Price * invoice.pltmat$Quantity



  ## create invoice:
  invoice <- rbind(
    invoice.pltmat,
    data.frame(
      Task = "Total",
      Quantity = "",
      Unitary_Price = "",
      Total = sum(invoice.pltmat$Total)
    )
  )
  invoice <- invoice[c("Task", "Unitary_Price", "Quantity", "Total")]

  return(invoice)
}



indExist <- function(indList, breeder) {
  # function to check if any individuals in indList already exist in the DB
  # indList (character verctor), list of individuals to check
  # breeder (charracter) breeder name

  # get requested individuals information
  all_breeder_inds <- db_get_individual(breeder = breeder)

  return(any(indList %in% all_breeder_inds$name))
}
