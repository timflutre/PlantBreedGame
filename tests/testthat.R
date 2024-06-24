library(testthat)

# load functions
invisible(
  sapply(FUN = source,
         X = list.files("src/fun", pattern = ".R$", full.names = T))
)

# run tests
# test_file("tests/testthat/test_0_dependencies.R",
#           stop_on_failure = TRUE,
#           stop_on_warning = FALSE)
# test_file("tests/testthat/test_game_time.R",
#           stop_on_failure = TRUE,
#           stop_on_warning = FALSE)

test_dir("tests/testthat",
         stop_on_failure = TRUE,
         stop_on_warning = FALSE)
