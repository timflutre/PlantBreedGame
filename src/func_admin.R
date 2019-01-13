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



addNewBreeder <- function(breederName, status, psw, progressNewBreeder=NULL){
  ## this function create a new breeder
  ## breederName (char) name of the new breeder

  if(grepl("[ ]", breederName)){
    stop("Breeder's name must not contain spaces")
  }
  if(status != "tester" & psw == ""){
    stop(paste0("a breeder with another status than tester",
                " can't have the empty string as password"))
  }

  #### initialisation:
  initIndsHaplo <- list.files(setup$truth.dir)
  initIndsHaplo <- initIndsHaplo[grep("Coll",initIndsHaplo)]
  db <- dbConnect(SQLite(), dbname=setup$dbname)


  #### test if new breeder already exist
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 1,
                           detail = "Check breeder existance")
  }

  tbl <- paste0("plant_material_", breederName)
  if(tbl %in% dbListTables(db)){
    stop("breeder already exist")
  }



  #### add breeder in the "breeders" table of database:
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 2,
                           detail = "Add breeder in \"breeders\" table")
  }
  hashed.psw <- digest(psw, "md5", serialize=FALSE)

  tbl <- "breeders"
  query <- paste0("INSERT INTO ", tbl, " VALUES",
                  " ('", breederName,"','", status,"','", hashed.psw, "')")
  res <- dbExecute(conn=db, query)




  #### create "plant_material_newBreeder"
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 3,
                           detail = "create \"plant_material\" table")
  }
  tbl <- paste0("plant_material_", breederName)
  query <- paste0("CREATE TABLE ", tbl,
                  " (parent1 TEXT",
                  ", parent2 TEXT",
                  ", child TEXT PRIMARY KEY",
                  ", avail_from TEXT)")
  res <- dbExecute(conn=db, query)



  #### fill "plant_material_newBreeder"
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 4,
                           detail = "fill \"plant_material\" table")
  }
  coll.ids <- gsub("_haplos.RData","",initIndsHaplo)
  query <- paste0("INSERT INTO ", tbl,
                  " (parent1, parent2, child, avail_from)",
                  " VALUES",
                  " ('", paste(gsub("Coll","ind", coll.ids),
                               rep(NA, length(coll.ids)),
                               coll.ids,
                               rep(paste0(constants$first.year, "-01-01 00:00:00"),
                                   length(coll.ids)),
                               sep="','",collapse = "'),('")
                  , "')")
  res <- dbExecute(conn=db, query)
  dbDisconnect(db)


  #### create folders of the new breeder:
  if (!is.null(progressNewBreeder)){
    progressNewBreeder$set(value = 5,
                           detail = "create truth and shared folders")
  }
  newTruthDir <- paste0(setup$truth.dir,"/",breederName)
  newSharedDir <- paste0(setup$shared.dir,"/",breederName)
  dir.create(newTruthDir)
  dir.create(newSharedDir)



  funApply <- function(fileName){
    if (!is.null(progressNewBreeder)){
      progressNewBreeder$set(value = 6,
                             detail = paste0("Create haplo symlink:",fileName))
    }

    fromFile <- paste0("../",fileName)
    toFile <- paste0(newTruthDir,"/",fileName)
    file.symlink(fromFile,toFile)
    return()
  }

  if (Sys.info()["sysname"]!="Windows"){
    sapply(initIndsHaplo, FUN = funApply)
  }else {
    # try with one file:
    funApply(initIndsHaplo[1])
    if (file.exists(paste0(newTruthDir,"/",initIndsHaplo[1]))){ #if it worked
      sapply(initIndsHaplo[-1], FUN = funApply)


    }else{
      # copy files
      funApply <- function(fileName){
        if (!is.null(progressNewBreeder)){
          progressNewBreeder$set(value = 6,
                                 detail = paste0("Create haplo copy:",fileName))
        }
        fromFile <- paste0(setup$truth.dir,"/",fileName)
        toFile <- paste0(newTruthDir,"/",fileName)
        file.copy(fromFile,toFile) # copy
        return()
      }
      sapply(initIndsHaplo, FUN = funApply)
    }
  }

}




deleteBreeder <- function(breederName){

  if(breederName=="admin"){
    stop("admin can't be deleted.")
  }

  ## delete truth and shared folders
  sharedDir <- paste0(setup$shared.dir,"/",breederName)
  truthDir <- paste0(setup$truth.dir,"/",breederName)

  unlink(sharedDir, recursive = TRUE)
  unlink(truthDir, recursive = TRUE)


  ## clean dataBase
  db <- dbConnect(SQLite(), dbname=setup$dbname)

  # delete plant_material_oldBreeder
  tbl <- paste0("plant_material_", breederName)
  query <- paste0("DROP TABLE ", tbl)
  res <- dbExecute(conn=db, query)

  # delete entry in breeders' table
  tbl <- "breeders"
  query <- paste0("DELETE FROM ", tbl,
                  " WHERE name = '",breederName,"'")
  res <- dbExecute(conn=db, query)

  # delete entry in log table


  dbDisconnect(db)



}
