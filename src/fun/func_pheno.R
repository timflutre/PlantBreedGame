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



## Contain functions used in "phenotyping" section.

process_pheno_request <- function(request_id, progressPheno = NULL) {
  # function which phenotype the requested individuals
  # request_id the request id in the DB


  request <- db_get_game_requests(id = request_id)
  stopifnot(request$type == "pheno")

  breeder <- request$breeder
  request_time <- request$game_date
  pheno_request_dta <- db_get_game_requests_data(id = request_id)

  constants <- getBreedingGameConstants()


  ## Calculate the year of the phenotyping
  year <- get_phenotyping_year(request_time)


  ## 0. load required data
  f <- paste0(DATA_TRUTH, "/p0.RData")
  load(f)
  f <- paste0(DATA_TRUTH, "/afs0.RData")
  load(f)
  subset.snps <- getSNPsubset()

  ## 1. Calculate year effect
  # get the seed from database:
  yearEffectSeed <- constants$seed.year.effect

  # set seed
  saved_seed <- .GlobalEnv$.Random.seed
  set.seed(yearEffectSeed + year) # seed depend of the year
  # calculate year effect
  alphas <- c(
    stats::rnorm(n = 1, mean = 0, sd = sqrt(p0$sigma.alpha2[1])),
    stats::rnorm(n = 1, mean = 0, sd = sqrt(p0$sigma.alpha2[2]))
  )
  Alpha <- matrix(alphas,
    nrow = 1, ncol = 2,
    dimnames = list(year, c("trait1", "trait2"))
  )
  .GlobalEnv$.Random.seed <- saved_seed

  ## 2. Get the phenotyped individuals
  phenotyped_inds_ids <- unique(pheno_request_dta$ind_id)
  phenotyped_inds <- db_get_individual(ind_id = phenotyped_inds_ids)
  pheno_request_dta <- merge(pheno_request_dta,
                             phenotyped_inds[, c("id", "name")],
                             by.x = "ind_id", by.y = "id")
  colnames(pheno_request_dta)[colnames(pheno_request_dta) == "name"] <- "ind_name"

  X <- load_genotypes(inds_ids = phenotyped_inds_ids, UIprogress = progressPheno)


  ## 4.1 handle the 'pheno-field' tasks for the requested individuals
  pheno_field_request_dta <- pheno_request_dta[pheno_request_dta$type == "pheno-field",]
  if (nrow(pheno_field_request_dta) > 0) {

    if (!is.null(progressPheno)) {
      progressPheno$set(
        value = 2,
        detail = "pheno simulation (field)..."
      )
    }

    phenosField.df <- makeDfPhenos(
      ind.ids = pheno_field_request_dta$ind_name,
      nb.plots.per.ind = pheno_field_request_dta$n_pheno,
      year = year,
      pathogen = ifelse((year - 2005) %% 3 == 0,
        TRUE, FALSE
      )
    )

    phenosField <- simulTraits12(
      dat = phenosField.df,
      mu = p0$mu,
      sigma.alpha2 = p0$sigma.alpha2,
      Alpha = Alpha,
      X = X[levels(phenosField.df$ind), , drop = FALSE],
      Beta = p0$Beta,
      sigma2 = p0$sigma2,
      afs = afs0
    )

    phenosField$trait3 <- simulTrait3(
      dat = phenosField.df,
      X = X[levels(phenosField.df$ind), , drop = FALSE],
      qtn.id = p0$trait3$qtn.id,
      resist.genos = p0$trait3$resist.genos,
      prob.resist.no.qtl = p0$trait3$prob.resist.no.qtl
    )

    phenosField.df$trait1.raw <- phenosField$Y[, 1]
    phenosField.df$trait2 <- phenosField$Y[, 2]
    phenosField.df$trait3 <- phenosField$trait3$y
    phenosField.df$trait1 <- phenosField.df$trait1.raw
    inds_with_t3 <- (phenosField.df$pathogen & as.logical(phenosField.df$trait3))
    phenosField.df$trait1[inds_with_t3] <- (1 - p0$prop.yield.loss) * phenosField.df$trait1[inds_with_t3]
  }

  ## 4.2 handle the 'pheno-patho' tasks for the requested individuals
  pheno_patho_request_dta <- pheno_request_dta[pheno_request_dta$type == "pheno-patho",]
  if (nrow(pheno_patho_request_dta) > 0) {

    if (!is.null(progressPheno)) {
      progressPheno$set(
        value = 3,
        detail = "pheno simulation (patho)..."
      )
    }

    phenosPatho.df <- makeDfPhenos(
      ind.ids = pheno_patho_request_dta$ind_name,
      nb.plots.per.ind = pheno_patho_request_dta$n_pheno,
      year = year,
      pathogen = TRUE
    )
    phenosPatho.df$plot <- paste0("lab-", phenosPatho.df$plot)

    phenosPatho <- list()
    phenosPatho$trait3 <- simulTrait3(
      dat = phenosPatho.df,
      X = X[levels(phenosPatho.df$ind), , drop = FALSE],
      qtn.id = p0$trait3$qtn.id,
      resist.genos = p0$trait3$resist.genos,
      prob.resist.no.qtl = 0
    )

    phenosPatho.df$trait1.raw <- NA
    phenosPatho.df$trait2 <- NA
    phenosPatho.df$trait3 <- phenosPatho$trait3$y
    phenosPatho.df$trait1 <- NA
  }

  db_add_pheno_data(phenosField.df, request_id)
  db_add_pheno_data(phenosPatho.df, request_id)
  db_update_request(id = request_id, processed = 1)

  # output
  return("done")
}




createInvoicePheno <- function(request.df) {
  # function which create the corresponding invoice of a request
  # request.df (data.frame) of the request

  # extract pheno-task
  # request.df.pheno <- request.df[request.df$task=="pheno-field"
  #                                |request.df$task=="pheno-patho",]
  request.df$details <- as.numeric(request.df$details)


  # aggregate by task
  invoice.pheno <- aggregate(details ~ task, data = request.df, sum)
  names(invoice.pheno) <- c("Task", "Quantity")

  constants <- getBreedingGameConstants()
  prices <- list(
    "pheno-field" = constants$cost.pheno.field,
    "pheno-patho" = constants$cost.pheno.patho * constants$cost.pheno.field
  )

  # get prices
  invoice.pheno$Unitary_Price <- as.vector(as.numeric(prices[invoice.pheno$Task]))
  invoice.pheno$Total <- invoice.pheno$Unitary_Price * invoice.pheno$Quantity



  ## create invoice:
  invoice <- rbind(
    invoice.pheno,
    data.frame(
      Task = "Total",
      Quantity = "",
      Unitary_Price = "",
      Total = sum(invoice.pheno$Total)
    )
  )
  invoice <- invoice[c("Task", "Unitary_Price", "Quantity", "Total")]

  return(invoice)
}



get_phenotyping_year <- function(request_dates) {
  # from a vector of request date return the corresponding phenotyping years
  if (length(request_dates) == 0) {
    return(request_dates)
  }
  request_year <- data.table::year(request_dates)
  limitDate <- as.Date(paste(request_year,
                             getBreedingGameConstants()$max.upload.pheno.field,
                             sep = "-"))

  pheno_year <- ifelse(request_dates > limitDate, request_year + 1, request_year)
  return(pheno_year)
}

get_remaining_pheno_plot <- function(breeder, request_date) {

  historyPheno <- db_get_game_requests_history(breeder = breeder,
                                               type = "pheno",
                                               detail = "pheno-field")
  historyPheno$pheno_year <- get_phenotyping_year(historyPheno$game_date)
  historyPheno <- historyPheno[historyPheno$pheno_year == get_phenotyping_year(request_date),]

  used_plots <- sum(historyPheno$quantity)
  remaining_plots <- max(0, getBreedingGameConstants()$nb.plots - used_plots)
  return(remaining_plots)
}


plotAvailable <- function(breeder, inds.todo, gameTime) {
  # function which check if all plots are available for phenotyping
  # breeder (character) name of the breeder
  # inds.todo (data frame) output of "readCheckBreedDataFile"
  # gameTime ("POSIXlt") of the request (given by getGameTime function)

  breeder_info <- db_get_breeder(breeder.name = breeder)
  if (breeder_info$status %in% c("game master", "tester")) {
    return(TRUE)
  }

  remaining_plots <- get_remaining_pheno_plot(breeder, gameTime)
  requested_plots <- sum(as.numeric(inds.todo$details[inds.todo$task == "pheno-field"]))

  return(remaining_plots >= requested_plots)
}
