#' Calculate Resistance Summary Statistics
#'
#' Calculates resistance summary statistics for individuals based on phenotypic
#' data. The function determines raw resistance status from pathogen presence
#' (ie. if the individual resist to the pathogen for the given observation,
#' pathogen=TRUE and trait3=false), then aggregates observations by individual
#' to compute resistance metrics (ie. proportion of resistance).
#'
#' @param pheno_data A data frame of the phenotypes
#'
#' @return A data frame with the following columns:
#'  - n_patho_obs Number of observations where pathogen was present
#'  - n_observed_resistance Number of observations where resistance was
#'       observed (pathogen and !trait3)
#'  - resistance_prop Proportion of resistance events among pathogen
#'       observations (n_observed_resistance / n_patho_obs)
#'
calc_resistant_summary <- function(pheno_data) {
  pheno_data$raw_resistance <- ifelse(
    pheno_data$pathogen,
    !pheno_data$trait3,
    NA
  )

  resistance_summary <- as.data.frame(
    pheno_data %>%
      dplyr::group_by(ind) %>%
      dplyr::summarise(
        n_patho_obs = sum(pathogen, na.rm = TRUE),
        n_observed_resistance = sum(raw_resistance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        resistance_prop = n_observed_resistance / n_patho_obs,
      ))

  return(resistance_summary)
}



#' Detect Resistant Individuals Using Naive Thresholding
#'
#' Classifies individuals as resistant or non-resistant based on their
#' resistance proportion using configurable thresholds and a minimum number of
#' observations. Individuals with intermediate resistance proportions are
#' classified as NA (unknown).
#'
#' The logic is:
#'  - if number of pathogen observations < `min_patho_obs` ->`NA` (ie. unknown)
#'  - if resistance proportion > thresh_resistance -> `TRUE` (resistant)
#'  - if resistance < thresh_non_resistance -> `FALSE` (resistant)
#'  - else `NA` (ie. unknown)
#'
#' @param resistance_summary A data frame containing resistance summary
#'   statistics, cf. `calc_resistant_summary` function
#'
#' @param thresh_resistance Numeric threshold (0-1) for classifying an
#'   individual as resistant. Individuals with resistance_prop >=
#'   thresh_resistance are classified as resistant.
#'
#' @param thresh_non_resistance Numeric threshold (0-1) for classifying an
#'   individual as non-resistant. Individuals with resistance_prop <=
#'   thresh_non_resistance are classified as non-resistant.
#'
#' @param min_patho_obs Minimum number of pathogen observations required to
#'   make a classification. Individuals with fewer observations are classified
#'   as NA. Default is 1.
#'
#' @return The input data frame with an additional `resistant` column (TRUE/FLASE/NA)
#'
detect_resistant_naive <- function(resistance_summary,
                                   thresh_resistance,
                                   thresh_non_resistance,
                                   min_patho_obs = 1) {
  resistance_summary <- resistance_summary %>%
    dplyr::mutate(
      resistant = dplyr::case_when(
        n_patho_obs < min_patho_obs ~ NA,
        resistance_prop > thresh_resistance ~ TRUE,
        resistance_prop < thresh_non_resistance ~ FALSE,
        .default = NA
      )
    )

  return(resistance_summary)
}



