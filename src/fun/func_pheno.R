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

phenotype <- function(breeder, inds.todo, gameTime, progressPheno = NULL, fileName = NULL) {
  # function which phenotype the requested individuals (see game_master_pheno-geno.R)
  # create a result file in shared folder of the breeder

  # breeder (character) name of the breeder
  # inds.todo (data frame) output of "readCheckBreedDataFile"
  # gameTime ("POSIXlt") of the request (given by getGameTime function)

  constants <- getBreedingGameConstants()

  ## Initialisations
  breederList <- getBreederList()
  stopifnot(breeder %in% breederList)

  data.types <- countRequestedBreedTypes(inds.todo)


  ## calculate output file names:
  fout <- list("pheno-field" = NULL, "pheno-patho" = NULL)
  for (dty in c("pheno-field", "pheno-patho")) {
    if (is.null(fileName) | grepl("[0-9]{4}[-][0-9]{2}[-][0-9]{2}", fileName)) { # fileName must not contain a date
      fout[dty] <- paste0(
        DATA_SHARED, "/", breeder, "/", "Result_", dty, "_",
        strftime(gameTime, format = "%Y-%m-%d"), ".txt.gz"
      )
      n <- 0
      while (file.exists(fout[[dty]])) {
        n <- n + 1
        fout[dty] <- paste0(
          DATA_SHARED, "/", breeder, "/", "Result_", dty, "_",
          strftime(gameTime, format = "%Y-%m-%d"), "_", n, ".txt.gz"
        )
      }
    } else {
      fileName <- strsplit(fileName, split = "[.]")[[1]][1] # delete extention
      fout[dty] <- paste0(
        DATA_SHARED, "/", breeder, "/", "Result_", dty, "_", fileName, "_",
        strftime(gameTime, format = "%Y-%m-%d"), ".txt.gz"
      )
      n <- 0
      while (file.exists(fout[[dty]])) {
        n <- n + 1
        fout[dty] <- paste0(
          DATA_SHARED, "/", breeder, "/", "Result_", dty, "_", fileName, "_",
          strftime(gameTime, format = "%Y-%m-%d"),
          "_", n, ".txt.gz"
        )
      }
    }
  }


  ## Calculate the year of the phenotyping
  maxDate <- strptime(
    paste0(
      data.table::year(gameTime), "-",
      constants$max.upload.pheno.field
    ),
    format = "%Y-%m-%d"
  )
  if (gameTime > maxDate) {
    year <- data.table::year(gameTime) + 1
  } else {
    year <- data.table::year(gameTime)
  }



  ## 0. load required data
  flush.console()
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

  ## 2. check that the requested individuals already exist
  flush.console()
  all_breeder_inds <- getBreedersIndividuals(breeder)
  stopifnot(all(inds.todo$ind %in% all_breeder_inds$child))


  ## 3. load the haplotypes and convert to genotypes
  flush.console()

  X <- matrix(
    nrow = length(unique(inds.todo$ind)),
    ncol = constants$nb.snps
  )


  for (i in 1:length(unique(inds.todo$ind))) {
    ind.id <- unique(inds.todo$ind)[i]

    if (!is.null(progressPheno)) {
      progressPheno$set(
        value = 1,
        detail = paste0("Load haplotypes: ", paste0(i, "/", nrow(inds.todo), " ", ind.id))
      )
    }

    # message(paste0(i, "/", nrow(inds.todo), " ", ind.id))

    f <- paste0(DATA_TRUTH, "/", breeder, "/", ind.id, "_haplos.RData")
    if (!file.exists(f)) {
      stop(paste0(f, " doesn't exist"))
    }
    load(f)

    ind$genos <- segSites2allDoses(seg.sites = ind$haplos, ind.ids = ind.id)
    X[i, ] <- ind$genos
  }
  rownames(X) <- unique(inds.todo$ind)
  colnames(X) <- colnames(ind$genos)

  ## 4.1 handle the 'pheno-field' tasks for the requested individuals
  flush.console()
  idx <- which(inds.todo$task == "pheno-field")

  if (!is.null(progressPheno)) {
    progressPheno$set(
      value = 2,
      detail = "pheno simulation (field)..."
    )
  }


  if (length(idx) > 0) {
    phenosField.df <- makeDfPhenos(
      ind.ids = inds.todo$ind[idx],
      nb.plots = as.numeric(inds.todo$details[idx]),
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
    tmp <- (phenosField.df$pathogen & as.logical(phenosField.df$trait3))
    if (any(tmp)) {
      phenosField.df$trait1[tmp] <- (1 - p0$prop.yield.loss) * phenosField.df$trait1[tmp]
    }

    ## write the phenotypes (all inds into the same file)

    write.table(
      x = phenosField.df[, -grep("raw", colnames(phenosField.df))],
      file = gzfile(fout[["pheno-field"]]), quote = FALSE,
      sep = "\t", row.names = FALSE, col.names = TRUE
    )
  }




  ## 4.2 handle the 'pheno-patho' tasks for the requested individuals
  flush.console()
  idx <- which(inds.todo$task == "pheno-patho")

  if (!is.null(progressPheno)) {
    progressPheno$set(
      value = 3,
      detail = "pheno simulation (patho)..."
    )
  }

  if (length(idx) > 0) {
    phenosPatho.df <- makeDfPhenos(
      ind.ids = inds.todo$ind[idx],
      nb.plots = as.numeric(inds.todo$details[idx]),
      year = year,
      pathogen = TRUE
    )

    phenosPatho <- list()

    phenosPatho$trait3 <- simulTrait3(
      dat = phenosPatho.df,
      X = X[levels(phenosPatho.df$ind), , drop = FALSE],
      qtn.id = p0$trait3$qtn.id,
      resist.genos = p0$trait3$resist.genos,
      prob.resist.no.qtl = 0
    )

    phenosPatho.df$trait1.raw <- "--"
    phenosPatho.df$trait2 <- "--"
    phenosPatho.df$trait3 <- phenosPatho$trait3$y
    phenosPatho.df$trait1 <- "--"

    ## write the phenotypes (all inds into the same file)
    write.table(
      x = phenosPatho.df[, -grep("raw", colnames(phenosPatho.df))],
      file = gzfile(fout[["pheno-patho"]]), quote = FALSE,
      sep = "\t", row.names = FALSE, col.names = TRUE
    )
  }




  ## 7. log
  flush.console()
  for (type in names(data.types)) {
    if ((type == "pheno-field" | type == "pheno-patho") && data.types[type] > 0) {
      query <- paste0(
        "INSERT INTO log(breeder,request_date,task,quantity)",
        " VALUES ('", breeder,
        "', '", strftime(gameTime, format = "%Y-%m-%d %H:%M:%S"),
        "', '", type, "', '",
        data.types[type], "')"
      )
      res <- db_execute_request(query)
    }
  }


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



plotAvailable <- function(breeder, inds.todo, gameTime) {
  # function which check if all plot are available for phenotyping

  # breeder (character) name of the breeder
  # inds.todo (data frame) output of "readCheckBreedDataFile"
  # gameTime ("POSIXlt") of the request (given by getGameTime function)


  ## Initialisations
  breederList <- getBreederList()
  stopifnot(breeder %in% breederList)


  ## get the historic of pheno requests
  query <- paste0("SELECT * FROM log WHERE breeder='", breeder, "' AND task='pheno-field' ")
  historyPheno <- db_get_request(query)

  ## get game constants
  constants <- getBreedingGameConstants()

  ## Calculate the start date of the current pheno session:
  limitDate <- strptime(
    paste0(
      data.table::year(gameTime), "-",
      constants$max.upload.pheno.field
    ),
    format = "%Y-%m-%d"
  )
  if (gameTime > limitDate) {
    yearRequest <- data.table::year(gameTime) + 1
  } else {
    yearRequest <- data.table::year(gameTime)
  }

  limitDate <- strptime(
    paste0(
      yearRequest - 1, "-",
      constants$max.upload.pheno.field
    ),
    format = "%Y-%m-%d"
  )


  ## Calculate the number of plot already used:
  historyPheno$request_date <- strptime(historyPheno$request_date, format = "%Y-%m-%d")
  usedPlot <- sum(historyPheno$quantity[historyPheno$request_date >= limitDate])

  ## compare with the request:
  requestPlot <- sum(as.numeric(inds.todo$details[inds.todo$task == "pheno-field"]))

  if (constants$nb.plots - usedPlot - requestPlot < 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
