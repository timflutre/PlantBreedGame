library(logger)
log_threshold(DEBUG) # TODO: update according to env variable

log_info("Starting request worker")

print(getwd())

suppressPackageStartupMessages({
  source("src/dependencies.R", local = TRUE, encoding = "UTF-8")
})
source("./src/fun/func_dbRequests.R", local = TRUE, encoding = "UTF-8")
source("./src/fun/func_geno.R", local = TRUE, encoding = "UTF-8")
source("./src/fun/func_pheno.R", local = TRUE, encoding = "UTF-8")
source("./src/fun/func_plant_material.R", local = TRUE, encoding = "UTF-8")



DATA_ROOT <- Sys.getenv("PLANTBREEDGAME_DATA_ROOT")
if (identical(DATA_ROOT, "")) {
  DATA_ROOT <- "./data"
}

if (!dir.exists(DATA_ROOT)) {
  stop(paste(
    "Specified game data folder:", DATA_ROOT, "does not exist.",
    "Make sure the `PLANTBREEDGAME_DATA_ROOT` environment variable is correctly set."
  ))
}


process_request <- function(request) {
  if (request$type == "geno") {
    process_geno_request(request$id)
  }
  if (request$type == "pheno") {
    process_pheno_request(request$id)
  }
  if (request$type == "pltmat") {
    process_plantmat_request(request$id)
  }
}

MAX_RETRY <- 3
SLEEP_TIME <- 3


log_info("Initialisation done, starting loop")
last_session <- ""
missing_data_folder <- FALSE

while (TRUE) {
  tryCatch(
    {
      DATA_IN_USE_FILE <- file.path(DATA_ROOT, "data_folder_in_use.txt")
      if (file.exists(DATA_IN_USE_FILE)) {
        CURRENT_SESSION_ID <- readLines(DATA_IN_USE_FILE)
        DATA_SESSION <- normalizePath(file.path(DATA_ROOT, CURRENT_SESSION_ID), mustWork = TRUE)
        if (DATA_SESSION != last_session) {
          log_info("Using session: {CURRENT_SESSION_ID}")
          last_session <- DATA_SESSION
        }
        missing_data_folder <- FALSE
      } else {
        if (!missing_data_folder) {
          log_warn(
            "No game session found, retrying in every ",
            "{SLEEP_TIME} s until it is found"
          )
        }
        missing_data_folder <- TRUE
        Sys.sleep(SLEEP_TIME)
        next
      }

      DATA_TRUTH <- file.path(DATA_SESSION, "truth")
      DATA_SHARED <- file.path(DATA_SESSION, "shared")
      DATA_INITIAL_DATA <- file.path(DATA_SHARED, "initial_data")
      DATA_DB <- file.path(DATA_SESSION, "breeding-game.sqlite")
      options(DATA_DB = DATA_DB)

      requests <- db_get_game_requests(progress = 0)
      if (nrow(requests) == 0) {
        Sys.sleep(SLEEP_TIME)
        next
      }

      log_info("Found {nrow(requests)} pending request(s)")

      # TODO: better priority management for the request to process
      # (eg. oldest one, earlier "available date"...)
      request_to_process <- requests[1, ]

      log_info(
        "Processing request [ID: {request_to_process$id}] from breeder '{request_to_process$breeder}': {request_to_process$name}",
        "(attempt {request_to_process$n_retry + 1}/{MAX_RETRY})"
      )


      tryCatch(
        {
          start_time <- Sys.time()
          process_request(request_to_process)
          elapsed_time <- round(as.numeric(difftime(
            Sys.time(),
            start_time,
            units = "secs"
          )), 2)
          log_success("Successfully processed request [ID: {request_to_process$id}] in {elapsed_time}s")
        },
        error = function(err) {
          log_error("Failed to process request [ID: {request_to_process$id}]: {conditionMessage(err)}")
          log_debug("Error details: {capture.output(print(err))}")
          db_update_request(
            id = request_to_process$id,
            process_info = conditionMessage(err)
          )

          if (request_to_process$n_retry < MAX_RETRY) {
            db_update_request(
              id = request_to_process$id,
              progress = 0
            )
            log_warn(paste(
              "Request [ID: {request_to_process$id}] will be retried"
            ))
          } else {
            db_update_request(
              id = request_to_process$id,
              progress = -1
            )
            log_error("Request [ID: {request_to_process$id}] marked as failed after {request_to_process$n_retry} retries (max: {MAX_RETRY})")
          }
        }
      )
    },
    error = function(err) {
      log_error("REQUEST WORKER FAILURE: {capture.output(print(err))}")
      Sys.sleep(SLEEP_TIME)
    }
  )
}
