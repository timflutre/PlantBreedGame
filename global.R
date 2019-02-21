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


source("src/dependencies.R", local=TRUE, encoding="UTF-8")$value

source("src/functions.R", local=TRUE, encoding="UTF-8")$value
source("src/func_time.R", local=TRUE, encoding="UTF-8")$value
source("src/func_id.R", local=TRUE, encoding="UTF-8")$value

## -------------------------------------------------------------------
## parameters

options(warn=-1) # don't show warnings
debugDisplay <- FALSE # display debug

mycolors <- c("#00aedb", "#a200ff", "#f47835", "#d41243", "#8ec127")

if(Sys.info()["sysname"] == "Windows"){
  Sys.setlocale("LC_TIME", "English")
} else
  Sys.setlocale("LC_TIME", "en_US.UTF-8")

## -------------------------------------------------------------------
## variables

root.dir <- "data"
setup <- getBreedingGameSetup(root.dir)
checkDbFile(setup$dbname)
constants <- getBreedingGameConstants(setup$dbname)

prices <- list("allofecundation"=constants$cost.allof*constants$cost.pheno.field,
               "autofecundation"=constants$cost.autof*constants$cost.pheno.field,
               "haplodiploidization"=constants$cost.haplodiplo*constants$cost.pheno.field,
               "pheno-field"=constants$cost.pheno.field,
               "pheno-patho"=constants$cost.pheno.patho*constants$cost.pheno.field,
               "geno-hd"=constants$cost.geno.hd*constants$cost.pheno.field,
               "geno-ld"=round(constants$cost.geno.ld*constants$cost.pheno.field,2),
               "geno-single-snp"=constants$cost.geno.single*constants$cost.pheno.field)

subset.snps <- list()
f <- paste0(setup$init.dir, "/snp_coords_hd.txt.gz")
subset.snps[["hd"]] <- rownames(read.table(f))
f <- paste0(setup$init.dir, "/snp_coords_ld.txt.gz")
subset.snps[["ld"]] <- rownames(read.table(f))

url.repo <- "https://github.com/timflutre/PlantBreedGame"
code.version <- getCodeVersion(url.repo)

stopifnot(all(c("admin", "test") %in% getBreederList(setup$dbname)))
stopifnot(all("game master" == getBreederStatus(setup$dbname, "admin"),
              "tester" == getBreederStatus(setup$dbname, "test")))
