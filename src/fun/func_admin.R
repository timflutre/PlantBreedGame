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






calcGameProgress <- function() {
  all_inds <- db_get_individual()
  all_inds <- all_inds[, c(
    "id",
    "breeder",
    "name",
    "parent1_id",
    "parent2_id",
    "parent1_name",
    "parent2_name",
    "GV_trait1",
    "GV_trait2"
  )]
  colnames(all_inds) <- c(
    "id",
    "breeder",
    "name",
    "parent1_id",
    "parent2_id",
    "parent1_name",
    "parent2_name",
    "trait1",
    "trait2"
  )
  load(file.path(DATA_TRUTH, "p0.RData"))
  all_inds$trait1 <- all_inds$trait1 + p0$mu["trait1"]
  all_inds$trait2 <- all_inds$trait2 + p0$mu["trait2"]
  all_inds$t1t2 <- all_inds$trait1 * all_inds$trait2

  all_inds$gen <- calcGeneration(all_inds, all_inds$id)

  all_inds$breeder[all_inds$breeder == "@ALL"] <- "Initial collection"
  return(all_inds)
}

calcGeneration <- function(ped, ids = ped$id) {
  force(ids)
  ped <- ped[, c("id", "parent1_id", "parent2_id")]
  ped$id <- as.character(ped$id)
  ped$parent1_id <- as.character(ped$parent1_id)
  ped$parent2_id <- as.character(ped$parent2_id)
  row.names(ped) <- as.character(ped$id)

  # get the id of the founders of the population
  # ie. inds with NA for parent1 or which are not in the pedigree data
  founders_ids <- ped$id[is.na(ped$parent1_id)]
  all_parents <- na.omit(unique(c(ped$parent1_id, ped$parent2_id)))
  founders_ids <- unique(c(founders_ids, all_parents[!all_parents %in% ped$id]))

  ped[founders_ids, "gen"] <- -1 # Founder at -1 so "Initial collection" is 0

  parents <- founders_ids
  n_loop <- 0
  max_loop <- 1000 # ugly but works as long as we don't have ~1000 generations
  while (any(is.na(ped$gen)) & n_loop < max_loop) {
    n_loop <- n_loop + 1

    # get the list of all the child ids
    child_ids <- ped$id[ped$parent1_id %in% parents | ped$parent2_id %in% parents]

    # get generation of the parents
    parent_1_ids <- ped[child_ids, "parent1_id"]
    generation_p1 <- ped[parent_1_ids, "gen"]

    parent_2_ids <- ped[child_ids, "parent2_id"]
    generation_p2 <- rep(-Inf, length(parent_2_ids))
    generation_p2[!is.na(parent_2_ids)] <- ped[parent_2_ids[!is.na(parent_2_ids)], "gen"]

    ped[child_ids, "gen"] <- pmax(generation_p1, generation_p2) + 1

    # preparation for next generation
    parents <- child_ids
  }
  if (any(is.na(ped$gen))) {
    stop("individuals generation couldn't be calculated")
  }
  return(ped[as.character(ids), "gen"])
}
