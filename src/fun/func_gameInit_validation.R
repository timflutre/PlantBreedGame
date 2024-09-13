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


valid_positive_integer <- function(n, strict = FALSE, accept_null = TRUE, raise_error = FALSE) {

  error <- return
  if (raise_error) {
    error <- stop
  }

  strictly <- ""
  if (strict) {
    strictly <- " strictly "
  }

  if (is.null(n)) {
    if (accept_null) {
      return(NULL)
    }
    error("Must not be NULL")
  }

  if (is.na(n)) {
    error(paste0("Mandatory and should be a", strictly, "positive integer"))
  }

  if (!is.numeric(n)) {
    error(paste0("Should be a", strictly, "positive integer"))
  }

  if (n %% 1 != 0 || n < 0) {
    error(paste0("Should be a", strictly, "positive integer"))
  }

  if (strict && n == 0) {
    error(paste0("Should be a", strictly, "positive integer"))
  }

  return(NULL)
}

valid_positive_number <- function(x, strict = FALSE, accept_null = TRUE, raise_error = FALSE) {

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

  strictly <- ""
  if (strict) {
    strictly <- "strictly "
  }

  if (is.na(x)) {
    error(paste0("Mandatory and should be a ", strictly, "positive number"))
  }

  if (!is.numeric(x)) {
    error(paste0("Should be a ", strictly, "positive number"))
  }

  if (x < 0) {
    error(paste0("Should be a ", strictly, "positive number"))
  }

  if (strict && x == 0) {
    error(paste0("Should be a ", strictly, "positive number"))
  }

  return(NULL)
}

valid_number <- function(x, accept_null = TRUE, raise_error = FALSE) {

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
    error("Mandatory and should be a number")
  }

  if (!is.numeric(x)) {
    error("Mandatory and should be a number")
  }

  return(NULL)
}

valid_range <- function(x, min, max, incl_min = TRUE, incl_max = TRUE, accept_null = TRUE, raise_error = FALSE) {

  error <- return
  error <- return
  if (raise_error) {
    error <- stop
  }
  error <- return
  if (raise_error) {
    error <- stop
  }
  error <- return
  if (raise_error) {
    error <- stop
  }
  error <- return
  if (raise_error) {
    error <- stop
  }
  error <- return
  if (raise_error) {
    error <- stop
  }
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
    error("Mandatory and should be a number")
  }

  if (!is.numeric(x)) {
    error("Mandatory and should be a number")
  }

  min_msg <- paste(min, "(excluded)")
  if (incl_min) {
    min_msg <- paste(min, "(included)")
  }
  max_msg <- paste(max, "(excluded)")
  if (incl_max) {
    max_msg <- paste(max, "(included)")
  }

  err_msg <- paste("Should be between", min_msg, "and", max_msg)
  if (x < min) {
    error(err_msg)
  }
  if (x > max) {
    error(err_msg)
  }

  if (!incl_min && x == min) {
    error(err_msg)
  }

  if (!incl_max && x == max) {
    error(err_msg)
  }

  return(NULL)

}

valid_mu <- function(x, accept_null = TRUE, raise_error = FALSE) {
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
    error("Mandatory and should be a number")
  }

  if (!is.numeric(x)) {
    error("Mandatory and should be a number")
  }

  if (x == 0) {
    error("μ cannot be null")
  }

  return(NULL)
}

valid_Tmin <- function(x, mu, accept_null = TRUE, raise_error = FALSE) {
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
    error("Mandatory and should be a number")
  }

  if (!is.numeric(x)) {
    error("Mandatory and should be a number")
  }

  if (is.na(mu)) {
    return(NULL)
  }

  if (x >= mu) {
    error(paste("Must be strictly lower than μ =", mu))
  }

  return(NULL)
}

valid_cv_g <- function(x, accept_null = TRUE, raise_error = FALSE) {
  valid_positive_number(x, strict = TRUE, accept_null, raise_error)
}

valid_h2 <- function(x, accept_null = TRUE, raise_error = FALSE) {
  valid_range(x, 0, 1, incl_min = FALSE, incl_max = FALSE, accept_null = accept_null, raise_error = raise_error)
}


calc_sigma_p2 <- function(mu, min) {((mu - min)/3)^2}
calc_sigma_a2 <- function(cv_g, mu) {(cv_g * mu)^2}
calc_sigma2 <- function(h2, sigma_a2) {((1 - h2) / h2) * sigma_a2}
calc_sigma_y2 <- function(sigma_p2, sigma_a2, sigma2) {sigma_p2 - sigma_a2 - sigma2}

valid_variance <- function(x, name = NULL, accept_na = FALSE, accept_null = TRUE, raise_error = FALSE) {
  error <- return
  if (raise_error) {
    error <- stop
  }

  if (length(x) == 0) {
    if (accept_null) {
      return(NULL)
    }
    error("No variance provided")
  }

  if (is.null(x)) {
    if (accept_null) {
      return(NULL)
    }
    error("Must not be NULL")
  }

  var_name = ""
  if (!is.null(name)) {
    var_name = paste(" for", name)
  }

  if (is.na(x)) {
    if (accept_na) {
      return(NULL)
    }
    error(paste0("variance is NA", var_name))
  }

  if (!is.numeric(x)) {
    error(paste0("variance is not a number", var_name))
  }

  if (x <= 0) {
    error(paste0("This value leads to negative variance", var_name))
  }
  if (x == Inf) {
    error(paste0("This value leads to an infinite variance", var_name))
  }
  return(NULL)
}

valid_prop_pleio <- function(x, accept_null = TRUE, raise_error = FALSE) {
  valid_range(x, 0, 1, incl_min = TRUE, incl_max = TRUE, accept_null = accept_null, raise_error = raise_error)
}
valid_cor_pleio <-  function(x, accept_null = TRUE, raise_error = FALSE) {
  valid_range(x, -1, 1, incl_min = TRUE, incl_max = TRUE, accept_null = accept_null, raise_error = raise_error)
}

