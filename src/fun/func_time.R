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



getGameTime <- function(time_irl = Sys.time(),
                        gameSessions = db_get_game_sessions(),
                        first_year = getBreedingGameConstants()["first.year"]) {

  first_game_day <- strptime(paste0(first_year, "-01-01"), format = "%Y-%m-%d")


  if (nrow(gameSessions) == 0) {
    return(first_game_day)
  }

  # get current time
  gameSessions$start <- apply(gameSessions, 1, function(line){
    lubridate::parse_date_time(line["start"], orders = "%Y-%m-%d %H:%M:%S", tz = line["time_zone"])
  }, simplify = TRUE)
  gameSessions$end <- apply(gameSessions, 1, function(line){
    lubridate::parse_date_time(line["end"], orders = "%Y-%m-%d %H:%M:%S", tz = line["time_zone"])
  }, simplify = TRUE)
  gameSessions$order <- order(gameSessions$start)

  # sort gameSession in ascending order
  gameSessions <- gameSessions[gameSessions$order, ]

  ## get the current session

  currentSesion <- gameSessions[which(time_irl >= gameSessions$start & time_irl < gameSessions$end), ]
  if (nrow(currentSesion) == 0) {
    # we are between 2 sessions
    previousSessions <- gameSessions[which(time_irl >= gameSessions$start),]

    if (nrow(previousSessions) == 0) {
      # no previous session means the game didn't start yet
      return(first_game_day)
    }

    # Then we set the "current time" at the end of the previous session so that
    # the game time do not pass.
    previousSession <- previousSessions[max(previousSessions$order),]
    time_irl <- previousSession$end
    currentSesion <- previousSession
  }

  ## calculation
  elapsTime <- 0
  for (i in seq(1:currentSesion$order)) {
    if (i != currentSesion$order) {
      elapsTime <- as.double(elapsTime + difftime(gameSessions$end[i], gameSessions$start[i], units = "mins") / gameSessions$year_time[i])
    } else {
      elapsTime <- as.double(elapsTime + difftime(time_irl, gameSessions$start[i], units = "mins") / gameSessions$year_time[i])
    }
  }
  elapsTime <- as.difftime(elapsTime * 365.2425, units = "days")

  # result
  gameTime <- first_game_day + elapsTime
  return(gameTime)
}

