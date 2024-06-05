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

create_plant_material <- function(breeder, crosses.todo, gameTime, progressPltMat = NULL, fileName = NULL) {
  # function which create the new generations (see game_master_plant-material.R)

  # breeder (character) name of the breeder
  # crosses.todo (data frame) output of "readCheckBreedPlantFile"
  # gameTime ("POSIXlt") of the request (given by getGameTime function)




  ## Initialisation
  breederList <- getBreederList()
  stopifnot(breeder %in% breederList)

  stopifnot(!is.null(crosses.todo))
  cross.types <- countRequestedBreedTypes(crosses.todo)

  year <- data.table::year(gameTime)

  ## file name
  if (is.null(fileName)) {
    fout <- paste0(
      DATA_SHARED, "/", breeder, "/", "IndList_",
      strftime(gameTime, format = "%Y-%m-%d"), ".txt"
    )
    n <- 0
    while (file.exists(fout)) {
      n <- n + 1
      fout <- paste0(
        DATA_SHARED, "/", breeder, "/", "IndList_",
        strftime(gameTime, format = "%Y-%m-%d"), "_", n, ".txt"
      )
    }
  } else {
    fileName <- strsplit(fileName, split = "[.]")[[1]][1] # delete extention
    fout <- paste0(
      DATA_SHARED, "/", breeder, "/", "IndList_", fileName, "_",
      strftime(gameTime, format = "%Y-%m-%d"), ".txt"
    )
    n <- 0
    while (file.exists(fout)) {
      n <- n + 1
      fout <- paste0(
        DATA_SHARED, "/", breeder, "/", "IndList_", fileName, "_",
        strftime(gameTime, format = "%Y-%m-%d"), "_", n, ".txt"
      )
    }
  }



  ## check the presence of new individuals in the set of existing individuals
  flush.console()
  parent.ids <- unique(c(crosses.todo$parent1, crosses.todo$parent2))
  parent.ids <- parent.ids[!is.na(parent.ids)]
  child.ids <- crosses.todo$child

  all_breeder_inds <- getBreedersIndividuals(breeder)

  stopifnot(all(parent.ids %in% all_breeder_inds$child))
  stopifnot(all(!child.ids %in% all_breeder_inds$child))


  ## load the haplotypes of all parents
  flush.console()

  # initialise parent haplotypes
  parents <- list(haplos = list())
  f <- list.files(
    path = DATA_TRUTH,
    pattern = "*_haplos.RData",
    full.names = T
  )[1]
  if (!file.exists(f)) {
    stop(paste0("No '*_haplos.RData' file found in /data folder"))
  }
  load(f)
  for (chr.id in names(ind$haplos)) {
    parents$haplos[[chr.id]] <- matrix(
      data = NA,
      ncol = ncol(ind$haplos[[chr.id]]),
      nrow = length(parent.ids) * nrow(ind$haplos[[chr.id]]),
      dimnames = list(seq(length(parent.ids) * nrow(ind$haplos[[chr.id]])), colnames(ind$haplos[[chr.id]]))
    )
    colnames(parents$haplos[[chr.id]]) <- colnames(ind$haplos[[chr.id]])
  }

  lines <- seq(nrow(ind$haplos[[chr.id]]))
  i <- 1
  for (parent.id in parent.ids) {
    if (!is.null(progressPltMat)) {
      progressPltMat$set(
        value = 1,
        detail = paste0(
          "Load haplotypes: ",
          i, "/", length(parent.ids), ": ",
          parent.id
        )
      )
      i <- i + 1
    }
    if ("ind" %in% ls()) {
      rm(ind)
    }
    f <- paste0(DATA_TRUTH, "/", breeder, "/", parent.id, "_haplos.RData")
    if (!file.exists(f)) {
      stop(paste0(f, " doesn't exist"))
    }
    load(f)
    for (chr.id in names(parents$haplos)) {
      parents$haplos[[chr.id]][lines, ] <- ind$haplos[[chr.id]]
      row.names(parents$haplos[[chr.id]])[lines] <- row.names(ind$haplos[[chr.id]])
    }
    lines <- lines + nrow(ind$haplos[[chr.id]])
  }
  stopifnot(sapply(parents$haplos, nrow) / 2 == length(parent.ids))


  ## perform the requested crosses
  if (!is.null(progressPltMat)) {
    progressPltMat$set(
      value = 2,
      detail = "perform crosses..."
    )
  }


  flush.console()
  new.inds <- list()
  loc.crossovers <- drawLocCrossovers(
    crosses = crosses.todo,
    nb.snps = sapply(parents$haplos, ncol)
  )
  new.inds$haplos <- makeCrosses(
    haplos = parents$haplos,
    crosses = crosses.todo,
    loc.crossovers = loc.crossovers, verbose = 0
  )


  ## save the haplotypes of the new individuals
  flush.console()
  for (new.ind.id in getIndNamesFromHaplos(new.inds$haplos)) {
    if (!is.null(progressPltMat)) {
      progressPltMat$set(
        value = 3,
        detail = paste0("Save haplotypes: ", new.ind.id)
      )
    }
    # message(new.ind.id)
    ind <- list(haplos = getHaplosInd(new.inds$haplos, new.ind.id))
    f <- paste0(DATA_TRUTH, "/", breeder, "/", new.ind.id, "_haplos.RData")
    save(ind, file = f)
  }



  ## insert the requested crosses into their table
  flush.console()

  constants <- getBreedingGameConstants()
  getAvailDate <- function(type) {
    if (type == "allofecundation") {
      availableDate <- seq(from = gameTime, by = paste0(constants$duration.allof, " month"), length.out = 2)[2]
    } else if (type == "autofecundation") {
      availableDate <- seq(from = gameTime, by = paste0(constants$duration.autof, " month"), length.out = 2)[2]
    } else if (type == "haplodiploidization") {
      availableDate <- seq(from = gameTime, by = paste0(constants$duration.haplodiplo, " month"), length.out = 2)[2]
    }
    return(strftime(availableDate, format = "%Y-%m-%d %H:%M:%S"))
  }
  crosses.todo$availableDate <- sapply(crosses.todo$explanations, FUN = getAvailDate)
  crosses.todo


  query <- paste((crosses.todo$parent1),
    (crosses.todo$parent2),
    (crosses.todo$child),
    (crosses.todo$availableDate),
    sep = "','", collapse = "'),('"
  )
  tbl <- paste0("plant_material_", breeder)
  query <- paste0("INSERT INTO ", tbl, " (parent1, parent2, child, avail_from) VALUES ('", query, "')")
  db_execute_request(query)

  ## write table
  write.table(
    x = crosses.todo[, -4], file = fout, quote = FALSE,
    sep = "\t", row.names = FALSE, col.names = TRUE
  )


  ## log
  flush.console()
  for (type in names(cross.types)) {
    if (cross.types[type] > 0) {
      query <- paste0(
        "INSERT INTO log(breeder,request_date,task,quantity)",
        " VALUES ('", breeder,
        "', '", strftime(gameTime, format = "%Y-%m-%d %H:%M:%S"),
        "', '", type,
        "', '", cross.types[type], "')"
      )
      db_execute_request(query)
    }
  }



  return("done")
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
  all_breeder_inds <- getBreedersIndividuals(breeder)

  return(any(indList %in% all_breeder_inds$child))
}
