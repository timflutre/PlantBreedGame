# functions used to validate game initialisation parameters
# these functions are defined here because they are used at 2 different
# places:
#  - The application, trhough the modules of each parameters
#    with an `InputValidator` object (to highlight wrong inputs in the UI)
#  - The Game Initialisation script (to stop it in case the inputs are wrong)
#
# Technically, the error messages should be slightly different if we raise an
# error from the initialisation script or in the application. But since it
# would complexify the code for few benefit the error messages to show on the
# UI are implemented here (as it is the intended way to setup the game).
#
# These functions can either `stop` or `return` an error message.
# The stop behaviour is intended to be used in the initialisation script
# and the return behaviour is intended to be used with `InputValidator$add_rule`


valid_rng_seed <- function(seed, accept_null = TRUE, raise_error = FALSE) {

  error <- return
  if (raise_error) {
    error <- stop
  }

  if (is.null(seed)) {
    if (accept_null) {
      return(NULL)
    }
    error("Must not be NULL")
  }

  if (is.na(seed)) {
    error("Mandatory and should be a positive integer")
  }

  if (!is.numeric(seed)) {
    error("Should be a positive integer")
  }

  if (seed %% 1 != 0 || seed < 0) {
    error("Should be a positive integer")
  }

  return(NULL)
}

valid_positive_number <- function(x, accept_null = TRUE, raise_error = FALSE) {

  error <- return
  if (raise_error) {
    error <- stop
  }

  if (is.null(x)) {
    if (accept_null) {
      return(NULL)
    }
    error("Must not be NULL")
  }

  if (is.na(x)) {
    error("Mandatory and should be a positive number")
  }

  if (!is.numeric(x)) {
    error("Should be a positive number")
  }

  if (x < 0) {
    error("Should be a positive number")
  }

  return(NULL)
}


