PERMISSIONS_LIST <- list(
  # permission_name = list(
  #   name = "name of the premission saved in the DB (only alphanumeric characters, hyphens, and underscore)",
  #   label_name = "Human readable name to show in the UI",
  #   desc = 'Description of the permission to show in the UI'
  #   no_ui = TRUE/FALSE # do not create a UI input for setting this permission
  # )
  admin = list(
    name = "admin",
    label_name = "Admin",
    desc = 'When enabled, player can access the "Admin" page to reinitialise the game, add/delete players, get access to the progess of each players'
  ),
  no_time_constraint = list(
    name = "no_time_constraint",
    label_name = "No time constraint",
    desc = "When enabled, player bypasses all time delays for accessing game's data. They receive results immediately after their corresponging request have been processed instead of waiting the normal delays (e.g., 4 months for phenotyping field plots, 1 months for genotyping, 12 months for haplodiploidisation...)."
  ),
  no_request_size_constraint = list(
    name = "no_request_size_constraint",
    label_name = "No request size constraints",
    desc = "When enabled, player bypasses all request size constraints. (e.g. maximum 300 plots for phenotyping requests)"
  ),
  evaluation = list(
    name = "evaluation",
    label_name = "Evaluation",
    desc = 'When enabled, player can access the "Evaluation" page to run the final trial comparing the submitted individuals of each players.'
  ),
  data_viz = list(
    name = "data_viz",
    label_name = "Data visualisation",
    desc = 'When enabled, player can access the "Data Visualisation" page providing tools for automatic vizualisation of their phenotype data.'
  )
)

PERMISSIONS_PRESETS <- list(
  admin = list(
    label_name = "Admin",
    permissions = unname(sapply(PERMISSIONS_LIST, function(p) {
      p$name
    })),
    desc = "Enables all permissions."
  ),
  test = list(
    label_name = "Tester",
    permissions = unname(sapply(
      Filter(function(p) p$name != "admin", PERMISSIONS_LIST),
      function(p) {
        if (p$name != "admin") {
          return(p$name)
        }
        return(NULL)
      }
    )),
    desc = "Enables all permissions except for game administration. Useful for letting a user test the game (eg. no timeor request size restriction) without the posibility of modifying the current game session."
  ),
  easy = list(
    label_name = "Easy",
    permissions = c("data_viz"),
    desc = "Allow access to game's facilitating tools."
  ),
  hard = list(
    label_name = "Hard",
    permissions = c(),
    desc = "Restrict access to game's facilitating tools."
  )
)


# check permission list
check_permissions_list <- function() {
  mandatory_fields <- c("name", "label_name", "desc")

  db_names <- rep(NA, length(PERMISSIONS_LIST))
  names(db_names) <- names(PERMISSIONS_LIST)
  for (permission_name in names(PERMISSIONS_LIST)) {
    permission <- PERMISSIONS_LIST[[permission_name]]

    permission_fields <- names(permission)
    if (any(!mandatory_fields %in% permission_fields)) {
      missing_fields <- mandatory_fields[!mandatory_fields %in% permission_fields]
      stop(paste0(
        "Permission `", permission_name, "` is missing mandatory fields: ",
        paste(missing_fields, collapse = ", ")
      ))
    }

    # Check that name contains only alphanumeric characters, hyphens, and underscores
    if (!grepl("^[A-Za-z0-9_-]+$", permission$name)) {
      stop(paste0(
        "Permission `", permission_name, "` has invalid `name`: '",
        permission$name, "'. Only alphanumeric characters, hyphens (-), and underscores (_) are allowed."
      ))
    }
    db_names[permission_name] <- permission$name
  }

  if (any(duplicated(db_names))) {
    stop(paste0(
      "Duplicated permission's `db_names`: ",
      paste(db_names[duplicated(db_names)], collapse = ", ")
    ))
  }
}
check_permissions_list() # check when this file is sourced
