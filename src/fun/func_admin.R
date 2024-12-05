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





deleteBreeder <- function(breederName) {
  if (breederName == "admin") {
    stop("admin can't be deleted.")
  }

  ## delete truth and shared folders
  sharedDir <- paste0(DATA_SHARED, "/", breederName)
  truthDir <- paste0(DATA_TRUTH, "/", breederName)

  unlink(sharedDir, recursive = TRUE)
  unlink(truthDir, recursive = TRUE)


  ## clean dataBase
  # delete plant_material_oldBreeder
  tbl_pltMat <- paste0("plant_material_", breederName)
  db_execute_request(paste0("DROP TABLE ", tbl_pltMat))
  db_execute_request(paste0(
    "DELETE FROM breeders ",
    "WHERE name = '", breederName, "'"
  ))

  # delete entry in Evaluation file:


  evalDta <- read.table(file.path(DATA_SHARED, "Evaluation.txt"),
    header = T, sep = "\t"
  )
  evalDta <- evalDta[evalDta$breeder != breederName, ]
  write.table(evalDta,
    file = file.path(DATA_SHARED, "Evaluation.txt"),
    append = FALSE,
    quote = FALSE, sep = "\t",
    row.names = FALSE, col.names = TRUE
  )
}




calcBV <- function(breeder, inds, savedBV = NULL, progress = NULL) {
  if (!is.null(progress)) {
    n <- length(inds)
    i <- 1
  }

  # load SNP effects
  f <- paste0(DATA_TRUTH, "/p0.RData")
  load(f)
  BV <- t(sapply(inds, function(ind.id) {
    if (!is.null(progress)) {
      progress$set(detail = paste0(
        "BV calculation for breeder: ", breeder,
        " - ", i, "/", n
      ))
      i <<- i + 1
    }
    indName <- paste0(c(breeder, ind.id), collapse = "_")
    f <- paste0(DATA_TRUTH, "/", breeder, "/", ind.id, "_haplos.RData")
    if (!file.exists(f)) {
      stop(paste0(f, " doesn't exist"))
    }
    load(f)

    ind$genos <- segSites2allDoses(seg.sites = ind$haplos, ind.ids = ind.id)
    rownames(ind$genos) <- indName

    ind$genos %*% p0$Beta
  }))

  BV
}

calcGameProgress <- function(progBar = NULL) {
  if (!is.null(progBar)) {
    if (is.null(progBar$getValue())) {
      progBar$set(value = 0)
    }
    progBar$set(
      value = progBar$getValue() + 1,
      message = "Game progress calculation:",
      detail = "Initialisation..."
    )
  } else {
    progBar <- list()
    progBar$set <- function(...) {
      invisible(NULL)
    }
    progBar$getValue <- function(...) {
      invisible(NULL)
    }
  }


  # load breeding values data:
  f <- paste0(DATA_TRUTH, "/allBV.RData")
  if (file.exists(f)) {
    progBar$set(detail = "Load BV...")
    load(f) # load `breedValuesDta` variable
  } else {
    ### GET BV of the initial individuals:
    progBar$set(detail = "BV calculation for initial collection...")
    # load initial collection genotypes
    f <- paste0(DATA_TRUTH, "/coll.RData")
    load(f) # load `coll` variable
    # load SNP effects
    f <- paste0(DATA_TRUTH, "/p0.RData")
    load(f) # load `p0` variable

    # initialisation of the breeding values data with the initial collection
    BVcoll <- data.frame(
      trait1 = coll$geno %*% p0$Beta[, 1],
      trait2 = coll$geno %*% p0$Beta[, 2]
    )
    breedValuesDta <- data.frame(
      breeder = "Initial collection",
      parent1 = NA,
      parent2 = NA,
      ind = rownames(BVcoll),
      gen = 1,
      BVcoll
    )
    rm(list = c("coll", "BVcoll")) # Free memory
  }

  ### GET BV of the breeders's individuals:
  progBar$set(value = progBar$getValue() + 1, detail = "BV calculation for new individuals...")
  # get the list of the breeders (without "admin" and "test")
  breeders <- getBreederList()
  breeders <- breeders[breeders != "admin" & breeders != "test"]


  ### Remove deleted breeders from breedValuesDta
  breedValuesDta <- breedValuesDta[breedValuesDta$breeder %in% c(breeders, "Initial collection"), ]

  ### Get all database tables (to avoid query to missing tables).
  allTbls <- db_list_tables()

  ### calculation
  # get list of all individuals with generation and BV
  breedValuesDta <- rbind(
    breedValuesDta,
    do.call(
      rbind,
      sapply(breeders, simplify = F, function(breeder) {
        progBar$set(detail = paste0(
          "BV calculation for breeder: ",
          breeder
        ))

        # get list of individuals
        tbl_pltMat <- paste0("plant_material_", breeder)
        if (tbl_pltMat %in% allTbls) {
          query <- paste0("SELECT * FROM ", tbl_pltMat)
          allInds <- db_get_request(query)
        } else {
          return()
        }

        # get the new individuals
        tmpBVdta <- breedValuesDta[breedValuesDta$breeder %in% c(breeder, "Initial collection"), ]
        inds <- allInds$child[!allInds$child %in% tmpBVdta$ind]

        if (length(inds) == 0) {
          return()
        }

        # calc breeding values of new individuals
        BV <- calcBV(breeder, inds, progress = progBar)
        colnames(BV) <- c("trait1", "trait2")

        # calc generation
        generation <- calcGeneration(
          allInds,
          inds
        )

        cbind(
          breeder = breeder,
          generation,
          BV
        )
      })
    )
  )


  # save breeding values:
  progBar$set(value = progBar$getValue() + 1, detail = "Save breeding values...")
  save(breedValuesDta,
    file = paste0(DATA_TRUTH, "/allBV.RData")
  )


  # return centered BV:
  # center breeding values
  iniMeanT1 <- mean(breedValuesDta[breedValuesDta$breeder == "Initial collection", "trait1"])
  iniMeanT2 <- mean(breedValuesDta[breedValuesDta$breeder == "Initial collection", "trait2"])
  breedValuesDta$trait1 <- breedValuesDta$trait1 - iniMeanT1
  breedValuesDta$trait2 <- breedValuesDta$trait2 - iniMeanT2

  # add intercept
  load(file.path(DATA_TRUTH, "p0.RData"))
  breedValuesDta$trait1 <- breedValuesDta$trait1 + p0$mu["trait1"]
  breedValuesDta$trait2 <- breedValuesDta$trait2 + p0$mu["trait2"]
  breedValuesDta$t1t2 <- breedValuesDta$trait1 * breedValuesDta$trait2

  progBar$set(value = progBar$getValue() + 1, detail = "Done !")
  breedValuesDta
}



calcGeneration <- function(ped, inds) {
  # checks
  stopifnot(
    is.data.frame(ped),
    all(c(
      "child", "parent1",
      "parent2"
    ) %in% colnames(ped)),
    all(!is.na(ped$parent1)),
    all(!is.na(ped$child)),
    all(!duplicated(ped$child)),
    is.character(inds),
    all(inds %in% ped$child)
  )

  # keep only columns "parent1", "parent2" and "child"
  ped <- ped[, c("parent1", "parent2", "child")]


  # get the id of the initial individuals of the population
  initInds <- unique(c(ped$parent1, ped$parent2))
  initInds <- initInds[!initInds %in% ped$child]

  # initialize dataframe individual/generation
  generation <- data.frame(
    parent1 = NA,
    parent2 = NA,
    ind = c(initInds, ped$child),
    gen = NA
  )
  generation$gen[generation$ind %in% initInds] <- 0

  parents <- initInds[initInds != "NA"]
  while (any(is.na(generation$gen))) {
    # get the list of all the child
    child <- ped$child[ped$parent1 %in% parents | ped$parent2 %in% parents]

    # get generation of the parents
    par1 <- ped$parent1[ped$child %in% child]
    genP1 <- sapply(par1, function(par) {
      generation$gen[generation$ind == par]
    })
    par2 <- ped$parent2[ped$child %in% child]
    genP2 <- sapply(par2, function(par) {
      generation$gen[generation$ind == par]
    })

    generation$gen[generation$ind %in% child] <- pmax(genP1, genP2) + 1
    generation$parent1[generation$ind %in% child] <- par1
    generation$parent2[generation$ind %in% child] <- par2

    # preparation for next generation
    parents <- child
  }

  generation <- generation[generation$ind != "NA", ]


  generation[generation$ind %in% inds, ]
}
