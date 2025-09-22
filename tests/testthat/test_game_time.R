library(testthat)
source("../../src/fun/func_time.R", local = TRUE, encoding = "UTF-8")


firstYear <- 2000
game_first_day <- strptime(paste0(firstYear, "-01-01 00:00"), format = "%Y-%m-%d %H:%M")

gameSessions_UTC <- data.frame(
  id = c(1, 2),
  start = c("2000-01-03 09:00:00", "2000-01-02 09:00:00"),
  end = c("2000-01-03 17:00:00", "2000-01-02 17:00:00"),
  year_time = 365.2425, # 1 min = 1 day
  time_zone = "UTC"
)
# NOTE:
# sessions are not ordered
#
# id |               start |                 end | year_time | time_zone
#  2 | 2000-01-02 09:00:00 | 2000-01-02 17:00:00 |  365.2425 |       UTC
#  1 | 2000-01-03 09:00:00 | 2000-01-03 17:00:00 |  365.2425 |       UTC

gameSessions_timezone <- data.frame(
  id = c(1, 2),
  start = c("2000-01-03 10:00:00", "2000-01-02 18:00:00"),
  end = c("2000-01-03 18:00:00", "2000-01-03 02:00:00"),
  year_time = 365.2425, # 1 min = 1 day
  time_zone = c("Europe/Paris", "Asia/Tokyo")
)
# This is the same sessions but with some specified timezones

gameSessions <- list(
  UTC = gameSessions_UTC,
  time_zone = gameSessions_timezone
)

# 2 sessions from 9h to 17h -> 8h * 60 = 480 mins
game_last_day <- game_first_day + as.difftime(480 * 2, units = "days")

test_that("getGameTime_before_sessions_start", {
  expected <- game_first_day

  for (gameSessions_name in names(gameSessions)) {
    # time_irl is before the start time of the fist session
    time_irl_list <- list(
      UTC = strptime("2000-01-01 00:00", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Asia = strptime("2000-01-01 00:00", format = "%Y-%m-%d %H:%M", tz = "Asia/Hong_Kong")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "before start time, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }

    # time_irl is at the same time of the start time of the fist session
    time_irl_list <- c(
      UTC = strptime("2000-01-02 09:00", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Europe = strptime("2000-01-02 10:00", format = "%Y-%m-%d %H:%M", tz = "Europe/Paris"),
      Asia = strptime("2000-01-02 18:00", format = "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "exactly at start time, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }
  }
})


test_that("getGameTime_after_sessions_ends", {
  expected <- game_last_day

  for (gameSessions_name in names(gameSessions)) {
    # time_irl is after end time of the last session
    time_irl_list <- c(
      UTC = strptime("2000-01-04 12:15", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Asia = strptime("2000-01-04 18:00", format = "%Y-%m-%d %H:%M", tz = "Asia/Hong_Kong")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "after the end time, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }


    # time_irl is at the same time of the end time of the last session
    time_irl_list <- c(
      UTC = strptime("2000-01-03 17:00", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Europe = strptime("2000-01-03 18:00", format = "%Y-%m-%d %H:%M", tz = "Europe/Paris"),
      Asia = strptime("2000-01-04 02:00", format = "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "exactly at the end time, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }
  }
})


test_that("getGameTime_between_2_sessions", {
  # 1st session is 8h
  expected <- game_first_day + as.difftime(8 * 60, units = "days")

  for (gameSessions_name in names(gameSessions)) {
    # between 2 session
    time_irl_list <- c(
      UTC = strptime("2000-01-02 20:15", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Europe = strptime("2000-01-02 22:22", format = "%Y-%m-%d %H:%M", tz = "Europe/Paris"),
      Asia = strptime("2000-01-03 03:12", format = "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "between 2 sessions, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }

    # just at the end of 1st session
    time_irl_list <- c(
      UTC = strptime("2000-01-02 17:00", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Europe = strptime("2000-01-02 18:00", format = "%Y-%m-%d %H:%M", tz = "Europe/Paris"),
      Asia = strptime("2000-01-03 02:00", format = "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "exactly at the end of the 1st sessions, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }

    # just at the start of 2nd session
    time_irl_list <- c(
      UTC = strptime("2000-01-03 09:00", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Europe = strptime("2000-01-03 10:00", format = "%Y-%m-%d %H:%M", tz = "Europe/Paris"),
      Asia = strptime("2000-01-03 18:00", format = "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "exactly at the start of the 2nd sessions, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }
  }
})


test_that("getGameTime_during_session", {
  for (gameSessions_name in names(gameSessions)) {
    # time_irl is during the 1st session
    # 3h15 min after 1st session start:
    expected <- game_first_day + as.difftime(3 * 60 + 15, units = "days")
    time_irl_list <- c(
      UTC = strptime("2000-01-02 12:15", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Europe = strptime("2000-01-02 13:15", format = "%Y-%m-%d %H:%M", tz = "Europe/Paris"),
      Asia = strptime("2000-01-02 21:15", format = "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "During 1st session, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }

    # time_irl is during the 2nd session
    time_irl <- strptime("2000-01-03 16:20", format = "%Y-%m-%d %H:%M", tz = "UTC")
    # 1st session is 8h
    # 7h20 min after 2nd session start
    expected <- game_first_day + as.difftime(8 * 60 + 7 * 60 + 20, units = "days")
    time_irl_list <- c(
      UTC = strptime("2000-01-03 16:20", format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Europe = strptime("2000-01-03 17:20", format = "%Y-%m-%d %H:%M", tz = "Europe/Paris"),
      Asia = strptime("2000-01-04 01:20", format = "%Y-%m-%d %H:%M", tz = "Asia/Tokyo")
    )
    for (time_irl_name in names(time_irl_list)) {
      expect_equal(
        getGameTime(
          time_irl = time_irl_list[[time_irl_name]],
          gameSessions = gameSessions[[gameSessions_name]],
          first_year = firstYear
        ),
        expected,
        label = paste(
          "During 2nd session, for time_irl", time_irl_name,
          ", gameSessions", gameSessions_name
        )
      )
    }
  }
})


test_that("getGameTime_empty_game_sessions", {
  empty_gameSessions <- structure(
    list(
      id = numeric(0),
      start = numeric(0),
      end = numeric(0),
      year_time = numeric(0),
      time_zone = character(0)
    ),
    class = "data.frame"
  )

  expected <- game_first_day

  time_irl <- strptime("2000-01-03 16:20", format = "%Y-%m-%d %H:%M", tz = "UTC")
  expect_equal(
    getGameTime(
      time_irl = time_irl,
      gameSessions = empty_gameSessions,
      first_year = firstYear
    ),
    expected
  )
})
