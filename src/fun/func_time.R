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


## Contain functions used for time managment.



getGameTime <- function(setup) {
  ## function to convert real time into game time

  # get current time
  now <- Sys.time()


  ## get sessions informations
  query <- paste0("SELECT * FROM sessions")
  res <- db_get_request(query)

  res$start <- strptime(res$start, format = "%Y-%m-%d %H:%M")
  res$end <- strptime(res$end, format = "%Y-%m-%d %H:%M")
  res <- res[order(res$start), ]



  ## get the current session
  currentSesion <- which(now >= res$start & now < res$end)
  if (length(currentSesion) == 0) {
    ## out of game session
    previousSession <- which(now >= res$start)
    currentSesion <- max(previousSession)
    if (length(previousSession) != 0) {
      now <- res$end[max(previousSession)] # end date of the laste session
    } else {
      return(strptime("2015-01-01", format = "%Y-%m-%d"))
    }
  }


  ## calculation
  elapsTime <- 0
  for (i in 1:currentSesion) {
    if (i != currentSesion) {
      elapsTime <- as.double(elapsTime + difftime(res$end[i], res$start[i], units = "mins") / res$year_time[i])
    } else {
      elapsTime <- as.double(elapsTime + difftime(now, res$start[i], units = "mins") / res$year_time[i])
    }
  }
  elapsTime <- as.difftime(elapsTime * 365.25, units = "days")

  # result
  gameTime <- strptime("2015-01-01", format = "%Y-%m-%d") + elapsTime
  return(gameTime)
}
