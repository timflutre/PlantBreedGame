library(testthat)
library(RSQLite)

source("../../src/fun/func_dbRequests.R", local = TRUE, encoding = "UTF-8")
source("../../src/fun/functions.R", local = TRUE, encoding = "UTF-8")


db_get_all <- function(table) {
  query <- paste0("SELECT * FROM ", table)
  db_get(query)
}

db_delete_from <- function(tbl, condition) {
  query <- paste0("DELETE FROM ", tbl, " WHERE ", condition)
  db_execute(query)
}

charSeq <- function(to, prefix = "", from = 1, suffix = "") {
  .seq <- seq(from, to)
  sprintf(
    paste0(
      prefix,
      "%0", floor(log10(to)) + 1, "i",
      suffix
    ),
    .seq
  )
}


tmpdir <- "../tmp"
unlink(tmpdir, recursive = TRUE)
dir.create(tmpdir)
options(DATA_DB = file.path(tmpdir, "test_db.sqlite"))

# create db file
dbDisconnect(dbConnect(SQLite(), dbname = getOption("DATA_DB")))



n_init_inds <- 10

initial_collection_parents_names <- charSeq(n_init_inds, "Ind_")
initial_collection_names <- charSeq(n_init_inds, "Coll_")

mock_pheno_field <- function(request_data, year) {
  pheno <- do.call(rbind, apply(request_data, 1, function(x) {
    data.frame(
      ind = as.character(x["ind"]),
      trait1 = rnorm(x["details"]),
      trait2 = rnorm(x["details"]),
      trait3 = sample(c(0, 1), size = x["details"], replace = TRUE)
    )
  }))
  pheno$plot <- seq(1:nrow(pheno))
  pheno$year <- year
  pheno$pathogen <- sample(c(T, F), size = 1)
  pheno <- pheno[, c("ind", "year", "plot", "pathogen", "trait1", "trait2", "trait3")]

  return(pheno)
}

mock_pheno_patho <- function(request_data, year) {
  pheno <- mock_pheno_field(request_data, year)
  pheno$pathogen <- TRUE
  pheno$trait1 <- NA
  pheno$trait2 <- NA
  return(pheno)
}

set.seed(1234)

test_that("db initialisation", {
  db_schema <- read_sql_file("../../src/db-schema.sql")


  db_execute(query = db_schema)

  db <- dbConnect(SQLite(), dbname = getOption("DATA_DB"))
  tables <- dbListTables(db)
  dbDisconnect(db)

  expect_true(
    setequal(
      tables,
      c(
        "breeders",
        "constants",
        "genotypes",
        "geno_requests",
        "pheno_requests",
        "phenotypes",
        "plant_material",
        "pltmat_requests",
        "evaluation_requests",
        "requests",
        "sessions",
        "sqlite_sequence",
        "v_plant_material",
        "v_phenotypes",
        "v_genotypes",
        "v_request_history",
        "v_evaluation_requests_summary",
        "v_remaining_budget"
      )
    )
  )
})


test_that("read/write constants", {
  expect_identical(
    getBreedingGameConstants(),
    structure(list(), names = character(0))
  )

  constants_list <- list(
    toto = 1,
    tata = "a",
    titi = "2000/01/01",
    first.year = 1900,
    max.upload.pheno.field = "05-31",
    duration.pheno.field = 4,
    duration.pheno.patho = 3,
    duration.allof = 4,
    duration.autof = 5,
    duration.haplodiplo = 12,
    duration.geno.hd = 3,
    duration.geno.ld = 2,
    duration.geno.single = 1,
    cost.pheno.field = 100,
    cost.pheno.patho = 0.1,
    cost.allof = 0.1,
    cost.autof = 0.25,
    cost.haplodiplo = 1,
    cost.geno.hd = 1,
    cost.geno.ld = 0.5,
    cost.geno.single = 0.02,
    cost.register = 4,
    initialBudget = 195000
  )

  db_add_constants(constants_list)
  expect_identical(
    getBreedingGameConstants(),
    constants_list
  )

  update_constants_list <- list(
    toto = 2,
    tata = "b"
  )
  db_update_constants(update_constants_list)
  updated_constants_list <- constants_list
  updated_constants_list$toto <- 2
  updated_constants_list$tata <- "b"
  expect_identical(
    getBreedingGameConstants(),
    updated_constants_list
  )
})

test_that("db utility functions", {
  expect_equal(
    condition("AND", "id", "IN", c(1, 2, 3)),
    "AND id IN ( 1, 2, 3 )"
  )

  expect_equal(
    condition("AND", "name", "IN", c("A", "B")),
    "AND name IN ( 'A', 'B' )"
  )

  expect_equal(
    condition("OR", "t", "BETWEEN", c(1, 100)),
    "OR t BETWEEN 1 AND 100"
  )

  expect_equal(
    condition("OR", "t", "=", "' OR '1'='1"),
    "OR t = ''' OR ''1''=''1'"
  )

  value <- NULL
  expect_equal(
    condition("OR", "t", "=", value),
    ""
  )

  expect_error(condition("OR", "t", "TOTO", 1))

  base_query <- "SELECT * FROM constants WHERE 1=1"

  key <- "tata"
  value <- "b"
  query <- paste(
    base_query,
    condition("AND", "key", "=", key),
    condition("AND", "value", "=", value)
  )
  expect_equal(
    db_get(query),
    data.frame(key = key, value = value)
  )

  # SQL injection
  key <- "tata' OR 1=1 --"
  value <- "b"
  safe_query <- paste(
    base_query,
    condition("AND", "key", "=", key),
    condition("AND", "value", "=", value)
  )
  malicious_query <- paste(
    base_query,
    paste("AND", "key", "= '", key, "'"),
    paste("AND", "value", "= '", value, "'")
  )
  expect_equal(
    db_get(malicious_query),
    db_get_all("constants")
  )
  expect_equal(
    db_get(safe_query),
    data.frame(key = character(), value = character())
  )
})


test_that("read/write game sessions", {
  expect_equal(
    db_get_game_sessions(),
    data.frame(
      id = numeric(),
      start = character(),
      end = character(),
      year_time = numeric(),
      time_zone = character()
    )
  )

  db_add_game_session(1, "2018-02-15 09:00:00", "2018-02-15 12:00:00", 60, "UTC")
  expect_equal(
    db_get_game_sessions(),
    data.frame(
      id = 1,
      start = "2018-02-15 09:00:00",
      end = "2018-02-15 12:00:00",
      year_time = 60,
      time_zone = "UTC"
    )
  )


  db_add_game_session(NA, "2018-02-15 14:00:00", "2018-02-15 17:00:00", 60, "UTC")
  db_add_game_session(NA, "2018-02-16 09:00:00", "2018-02-16 11:40:00", 40, "UTC")
  expect_equal(
    db_get_game_sessions(),
    data.frame(
      id = c(1, 2, 3),
      start = c("2018-02-15 09:00:00", "2018-02-15 14:00:00", "2018-02-16 09:00:00"),
      end = c("2018-02-15 12:00:00", "2018-02-15 17:00:00", "2018-02-16 11:40:00"),
      year_time = c(60, 60, 40),
      time_zone = rep("UTC", 3)
    )
  )

  db_delete_game_session(1)
  db_delete_game_session(3)
  db_delete_game_session(2)
  expect_equal(
    db_get_game_sessions(),
    data.frame(
      id = numeric(),
      start = character(),
      end = character(),
      year_time = numeric(),
      time_zone = character()
    )
  )


  db_add_game_session(NA, "2018-02-15 14:00:00", "2018-02-15 17:00:00", 60, "UTC")
  expect_equal(
    db_get_game_sessions(),
    data.frame(
      id = 4,
      start = "2018-02-15 14:00:00",
      end = "2018-02-15 17:00:00",
      year_time = 60,
      time_zone = "UTC"
    )
  )
})



test_that("read/write breeders", {
  expect_equal(
    db_get_all("breeders"),
    data.frame(
      name = character(),
      status = character(),
      h_psw = character()
    )
  )

  db_add_breeder("test-db 1", "admin", "1234")
  db_add_breeder("test-db 2", "player", "abcd")

  expect_equal(
    db_get_all("breeders"),
    data.frame(
      name = c("test-db 1", "test-db 2"),
      status = c("admin", "player"),
      h_psw = c("1234", "abcd")
    )
  )

  expect_equal(
    getBreederList(),
    c("test-db 1", "test-db 2")
  )

  expect_equal(
    db_get_breeder("test-db 1"),
    list(
      name = "test-db 1",
      status = "admin",
      h_psw = "1234"
    )
  )

  db_delete_breeder("test-db 1")
  expect_equal(
    db_get_all("breeders"),
    data.frame(
      name = "test-db 2",
      status = "player",
      h_psw = "abcd"
    )
  )
  expect_no_error({
    db_update_breeder(
      breeder = "test-db 2",
      new_h_psw = "1234"
    )
  })
  expect_equal(
    db_get_all("breeders"),
    data.frame(
      name = "test-db 2",
      status = "player",
      h_psw = "1234"
    )
  )
  expect_no_error({
    db_update_breeder(
      breeder = "test-db 2",
      new_status = "tester"
    )
  })
  expect_equal(
    db_get_all("breeders"),
    data.frame(
      name = "test-db 2",
      status = "tester",
      h_psw = "1234"
    )
  )
  db_delete_breeder("test-db 2")

  db_add_breeder("@ALL", NA, NA)
  db_add_breeder("A", "player", "1234")
  db_add_breeder("B", "player", "1234")
})


test_that("read/write requests", {
  expect_equal(
    db_get_all("requests"),
    data.frame(
      id = numeric(),
      breeder = character(),
      name = character(),
      type = character(),
      game_date = character(),
      time = numeric(),
      processed = numeric()
    )
  )

  db_add_request(
    id = NA,
    breeder = "A",
    name = "test_A_1",
    type = "pltmat",
    game_date = "2000-01-01"
  )
  requests_dta <- db_get_game_requests()
  requests_dta <- subset(requests_dta, select = -time)
  expect_equal(
    requests_dta,
    data.frame(
      id = 1,
      breeder = "A",
      name = "test_A_1",
      type = "pltmat",
      game_date = "2000-01-01",
      processed = 0
    )
  )

  expect_error({
    # requests names should be unique for each breeders
    db_add_request(
      id = NA,
      breeder = "A",
      name = "test_A_1",
      type = "pltmat",
      game_date = "2000-01-02"
    )
  })

  db_delete_from("requests", "id=1")



  expect_equal(
    requests_dta,
    data.frame(
      id = 1,
      breeder = "A",
      name = "test_A_1",
      type = "pltmat",
      game_date = "2000-01-01",
      processed = 0
    )
  )


  # breeder's requests
  for (breeder in c("A", "B")) {
    for (i in seq(3)) {
      for (type in c("pltmat", "pheno", "geno")) {
        db_add_request(
          id = NA,
          breeder = breeder,
          name = paste0("test_", type, "_", i),
          type = type,
          game_date = paste0("2000-0", i, "-01")
        )
      }
    }
  }
  expect_equal(nrow(db_get_game_requests()), 18)
  expect_equal(nrow(db_get_game_requests(breeder = "A")), 9)
  expect_equal(nrow(db_get_game_requests(breeder = "A", name = "test_pltmat_1")), 1)
  expect_equal(nrow(db_get_game_requests(breeder = "A", type = "pheno")), 3)
  expect_equal(nrow(db_get_game_requests(id = 2)), 1)

  expect_equal(nrow(db_get_game_requests(id = c(2, 3))), 2)
  expect_equal(
    nrow(db_get_game_requests(
      breeder = "A",
      name = c("test_pltmat_1", "test_pltmat_2")
    )),
    2
  )

  # cascade delete
  db_add_breeder("test_req", "player", "1234")
  db_add_request(
    id = NA,
    breeder = "test_req",
    name = "test_to_delete",
    type = "pltmat",
    game_date = "2000-01-01"
  )
  expect_equal(nrow(db_get_game_requests(breeder = "test_req")), 1)
  db_delete_breeder("test_req")
  expect_equal(nrow(db_get_game_requests(breeder = "test_req")), 0)

  # initial requests
  expect_no_error({
    db_add_request(
      id = NA,
      breeder = "@ALL",
      name = "-- Initial Plant Material --",
      type = "pltmat",
      game_date = paste0(getBreedingGameConstants()$first.year - 1, "-01-01")
    )
  })
  expect_error({
    db_add_request(
      id = NA,
      breeder = "@ALL",
      name = "-- Initial Plant Material --",
      type = "pltmat",
      game_date = paste0(getBreedingGameConstants()$first.year - 1, "-01-01")
    )
  })

  expect_no_error({
    db_add_request(
      id = NA,
      breeder = "@ALL",
      name = "-- Initial Genotypes --",
      type = "geno",
      game_date = paste0(getBreedingGameConstants()$first.year - 1, "-01-01")
    )
  })
  expect_error({
    db_add_request(
      id = NA,
      breeder = "@ALL",
      name = "-- Initial Genotypes --",
      type = "geno",
      game_date = paste0(getBreedingGameConstants()$first.year - 1, "-01-01")
    )
  })

  # cf. phenotype data section for initial phenotype requests

  expect_equal(nrow(db_get_game_requests(breeder = "A")), 11)
  expect_equal(nrow(db_get_game_requests(breeder = "B", type = "pheno")), 3)
  expect_equal(nrow(db_get_game_requests(type = "geno")), 7)


  request_id <- db_get_game_requests(breeder = "A")[1, "id"]
  expect_equal(db_get_game_requests(id = request_id)$processed, 0)
  expect_no_error({
    db_update_request(id = request_id, processed = 1)
  })
  expect_equal(db_get_game_requests(id = request_id)$processed, 1)
})


test_that("plant material data", {
  expect_equal(
    db_get_all("pltmat_requests"),
    data.frame(
      id = numeric(),
      req_id = numeric(),
      parent1_id = numeric(),
      parent2_id = numeric(),
      parent1_request_name = character(),
      parent2_request_name = character(),
      child_name = character(),
      cross_type = character()
      # processed = numeric()
    )
  )

  expect_equal(
    db_get_all("plant_material"),
    data.frame(
      id = numeric(),
      # breeder = character(),
      name = character(),
      parent1_id = numeric(),
      parent2_id = numeric(),
      pltmat_request_id = numeric(),
      haplotype_file = character(),
      control = numeric(),
      GV_trait1 = numeric(),
      GV_trait2 = numeric(),
      resist_trait3 = numeric()
    )
  )

  # add initial individuals
  mock_init_request <- data.frame(
    parent1 = charSeq(n_init_inds, "Ind_"),
    parent2 = NA,
    child = charSeq(n_init_inds, "Coll_"),
    explanations = "haplodiploidization"
  )
  initial_plant_mat_request_id <- db_get_game_requests(name = "-- Initial Plant Material --")$id
  expect_no_error({
    new_data_init <- db_add_init_pltmat_req_data(initial_plant_mat_request_id, mock_init_request)
  })

  expect_equal(
    new_data_init,
    data.frame(
      id = 1:n_init_inds,
      req_id = 21,
      parent1_id = 1:n_init_inds,
      parent2_id = NA_integer_,
      parent1_request_name = initial_collection_parents_names,
      parent2_request_name = NA_character_,
      child_name = initial_collection_names,
      cross_type = "haplodiploidization"
    )
  )

  # add initial collection
  expect_equal(
    length(db_add_pltmat(req_id = initial_plant_mat_request_id)),
    n_init_inds
  )

  initial_plantMaterial_raw <- db_get_individual_raw()
  expect_equal(
    initial_plantMaterial_raw,
    data.frame(
      id = c(1:(2 * n_init_inds)),
      name = c(
        initial_collection_parents_names,
        initial_collection_names
      ),
      parent1_id = c(rep(NA, n_init_inds), c(1:n_init_inds)),
      parent2_id = NA_integer_,
      pltmat_request_id = c(
        rep(NA, n_init_inds),
        c(1:n_init_inds)
      ),
      haplotype_file = NA_character_,
      control = 0,
      GV_trait1 = NA_integer_,
      GV_trait2 = NA_integer_,
      resist_trait3 = NA_integer_
    )
  )
  initial_plantMaterial <- db_get_individual()
  expect_equal(nrow(initial_plantMaterial), n_init_inds)
  expect_equal(unique(initial_plantMaterial$breeder), "@ALL")



  # add new generations

  # test we can not add plant materials before their request
  initial_plantMaterial <- db_get_individual()
  breeder_pltmat_req_id <- db_get_game_requests(
    breeder = "A",
    name = "test_pltmat_1"
  )[1, 1]
  expect_equal(
    length(db_add_pltmat(req_id = breeder_pltmat_req_id)),
    0
  )
  expect_equal(
    db_get_individual(),
    initial_plantMaterial
  )

  # test we can not add pltmat_request without their individuals in the db
  # (this is ensure by the db schema for parent1 but not for parent2)
  mock_request <- data.frame(
    parent1 = c(initial_collection_names[1], "XXX"),
    parent2 = c(initial_collection_names[2], initial_collection_names[2]),
    child = c("A", "B"),
    explanations = rep("allofecundation", 2)
  )
  expect_error({
    db_add_pltmat_req_data(breeder_pltmat_req_id, mock_request)
  })

  mock_request <- data.frame(
    parent1 = c(initial_collection_names[1], initial_collection_names[1]),
    parent2 = c(initial_collection_names[2], "XXX"),
    child = c("A", "B"),
    explanations = rep("allofecundation", 2)
  )
  expect_error({
    db_add_pltmat_req_data(breeder_pltmat_req_id, mock_request)
  })


  i <- 0
  current_plantMaterial <- initial_plantMaterial
  new_inds <- initial_collection_names
  for (generation in c(1, 2)) {
    parent1 <- c(
      new_inds,
      rep(new_inds[1], 2)
    )
    parent2 <- c(
      rev(new_inds),
      NA, new_inds[1]
    )
    new_inds <- charSeq(n_init_inds, paste0("G", generation, "_"))

    mock_request <- data.frame(
      parent1 = parent1,
      parent2 = parent2,
      child = c(
        new_inds,
        paste0("G", generation, "_", c("haplo", "autof"))
      ),
      explanations = c(
        rep("allofecundation", length(parent1) - 2),
        "haplodiploidization",
        "autofecundation"
      )
    )

    for (breeder in c("A", "B")) {
      breeder_pltmat_req_id <- db_get_game_requests(
        breeder = breeder,
        name = paste0(
          "test_pltmat_",
          generation
        )
      )[1, 1]

      # add plant_material request
      expect_no_error({
        new_data <- db_add_pltmat_req_data(breeder_pltmat_req_id, mock_request)
      })
      expected_ids <- n_init_inds + 1 + i * (n_init_inds + 2)
      expect_equal(
        new_data,
        data.frame(
          id = expected_ids:(expected_ids + n_init_inds + 2 - 1),
          req_id = breeder_pltmat_req_id,
          parent1_id = db_get_individuals_ids(breeder, mock_request$parent1),
          parent2_id = db_get_individuals_ids(breeder, mock_request$parent2),
          parent1_request_name = mock_request$parent1,
          parent2_request_name = mock_request$parent2,
          child_name = mock_request$child,
          cross_type = mock_request$explanations
        )
      )

      # add plant_material
      expect_equal(
        length(db_add_pltmat(req_id = breeder_pltmat_req_id)),
        nrow(mock_request)
      )

      expect_equal(
        nrow(db_get_individual()),
        nrow(current_plantMaterial) + nrow(mock_request)
      )

      pltmat_req_data <- db_get_game_requests_data(
        id = breeder_pltmat_req_id,
        type = "pltmat"
      )
      individuals_data <- db_get_individual(
        breeder = breeder,
        request_name = paste0(
          "test_pltmat_",
          generation
        )
      )
      pltmat_req_data <- pltmat_req_data[order(pltmat_req_data$id), ]
      individuals_data <- individuals_data[order(individuals_data$pltmat_request_id), ]

      expect_equal(individuals_data$name, pltmat_req_data$child_name)
      expect_equal(individuals_data$parent1_name, pltmat_req_data$parent1_request_name)
      expect_equal(individuals_data$parent2_name, pltmat_req_data$parent2_request_name)
      expect_equal(individuals_data$cross_type, pltmat_req_data$cross_type)
      expect_equal(individuals_data$request_date, pltmat_req_data$request_game_date)

      i <- i + 1
      current_plantMaterial <- db_get_individual()
    }
  }


  # db_get_game_requests_data function test
  expect_equal(
    nrow(db_get_game_requests_data(id = db_get_game_requests(breeder = "A", name = "-- Initial Plant Material --")[1, 1])),
    n_init_inds
  )
  expect_equal(
    nrow(db_get_game_requests_data(id = db_get_game_requests(breeder = "B", name = "test_pltmat_1")[1, 1])),
    n_init_inds + 2
  )

  expect_equal(nrow(db_get_game_requests_data(breeder = "A", type = "pltmat")), n_init_inds + 2 * (n_init_inds + 2))
  expect_equal(nrow(db_get_game_requests_data(type = "pltmat")), n_init_inds + 2 * 2 * (n_init_inds + 2))
  expect_equal(
    colnames(db_get_game_requests_data(type = "pltmat")),
    c(
      "id",
      "req_id",
      "parent1_id",
      "parent2_id",
      "parent1_request_name",
      "parent2_request_name",
      "child_name",
      "cross_type",
      "request_breeder",
      "request_name",
      "request_type",
      "request_game_date",
      "request_time",
      "request_processed"
    )
  )

  # several requests type
  expect_error(nrow(db_get_game_requests_data(breeder = "A")))

  # request id is required
  expect_error({
    new_data <- db_add_pltmat_req_data(NA, mock_request)
  })

  controls_names <- initial_collection_names[c(1, length(initial_collection_names))]
  expect_no_error({
    db_mark_as_control(controls_names)
  })
  expect_no_error({
    controls <- db_get_individual(control = TRUE)
  })
  expect_equal(controls$name, controls_names)


  # request without initial collection
  not_init_coll <- db_get_individual(exclude_initial_coll = TRUE)
  expect_true(!any(not_init_coll$breeder == "@ALL"))
})




test_that("phenotype data", {
  expect_equal(
    db_get_all("pheno_requests"),
    data.frame(
      id = numeric(),
      req_id = numeric(),
      ind_id = numeric(),
      ind_request_name = character(),
      type = character(),
      n_pheno = numeric()
    )
  )
  expect_equal(
    db_get_all("phenotypes"),
    data.frame(
      id = integer(0),
      pheno_req_id = integer(0),
      year = integer(0),
      plot = character(0),
      pathogen = integer(0),
      trait1 = numeric(0),
      trait2 = numeric(0),
      trait3 = integer(0)
    )
  )


  # Add initial pheno data
  initial_pheno_list <- list()
  n_rep <- 4
  for (i in seq(n_init_inds - 1)) {
    pheno_req <- data.frame(
      ind = initial_collection_names[i:(i + 1)],
      task = "pheno-field",
      details = n_rep
    )
    initial_pheno_list[[i]] <- mock_pheno_field(pheno_req, 1950 + i)
  }
  init_pheno_data <- do.call(rbind, initial_pheno_list)
  expect_no_error({
    db_add_initial_pheno_data(init_pheno_data)
  })
  expect_no_error({
    initial_pheno <- db_get_phenotypes(initial_data_only = TRUE)
  })
  expect_equal(
    nrow(initial_pheno),
    2 * n_rep * (n_init_inds - 1)
  )

  expect_identical(
    db_get_phenotypes(breeder = "A"),
    initial_pheno
  )
  expect_identical(
    db_get_phenotypes(breeder = "A"),
    db_get_phenotypes(breeder = "B")
  )


  for (generation in c(0, 1, 2)) {
    for (breeder in c("A", "B")) {
      if (generation == 0) {
        inds <- initial_collection_names[c(1, n_init_inds)]
      } else {
        inds <- db_get_individual(
          breeder = breeder,
          request_name = paste0("test_pltmat_", generation)
        )$name
      }
      mock_request <- data.frame(
        ind = rep(inds, 2),
        task = rep(c("pheno-field", "pheno-patho"), each = length(inds)),
        details = c(
          rep("2", length(inds)),
          rep("1", length(inds))
        )
      )

      pheno_req_id <- db_get_game_requests(
        breeder = breeder,
        name = paste0(
          "test_pheno_",
          generation + 1
        )
      )[1, 1]

      # add pheno request
      expect_no_error({
        new_pheno_req_data <- add_pheno_req_data(pheno_req_id, mock_request)
      })

      # mock phenotype
      pheno_field <- mock_pheno_field(mock_request[mock_request$task == "pheno-field", ], 2000)
      pheno_patho <- mock_pheno_patho(mock_request[mock_request$task == "pheno-patho", ], 2000)
      pheno_data <- rbind(pheno_field, pheno_patho)

      # add phenotype data
      expect_no_error({
        db_add_pheno_data(pheno_data, pheno_req_id)
      })
    }
  }


  # test with pheno data that are only field data
  breeder <- "A"
  req_name <- "test_field_only"
  db_add_request(
    id = NA,
    breeder = breeder,
    name = req_name,
    type = "pheno",
    game_date = "2000-01-01"
  )
  pheno_req_id <- db_get_game_requests(breeder = breeder, name = req_name)[1, 1]

  # add pheno request
  mock_request <- data.frame(
    ind = initial_collection_names,
    task = "pheno-field",
    details = 1
  )
  expect_no_error({
    new_pheno_req_data <- add_pheno_req_data(pheno_req_id, mock_request)
  })

  # add phenotype data
  pheno_data <- mock_pheno_field(mock_request, 2000)
  expect_no_error({
    db_add_pheno_data(pheno_data, pheno_req_id)
  })

  # test with pheno data that are only patho data
  breeder <- "A"
  req_name <- "test_patho_only"
  db_add_request(
    id = NA,
    breeder = breeder,
    name = req_name,
    type = "pheno",
    game_date = "2000-01-01"
  )
  pheno_req_id <- db_get_game_requests(breeder = breeder, name = req_name)[1, 1]

  # add pheno request
  mock_request <- data.frame(
    ind = initial_collection_names,
    task = "pheno-patho",
    details = 1
  )
  expect_no_error({
    new_pheno_req_data <- add_pheno_req_data(pheno_req_id, mock_request)
  })

  # add phenotype data
  pheno_data <- mock_pheno_patho(mock_request, 2000)
  expect_no_error({
    db_add_pheno_data(pheno_data, pheno_req_id)
  })



  # test access to phenotypes data
  expect_no_error({
    db_get_phenotypes()
  })
  expect_no_error({
    db_get_phenotypes(
      breeder = "A",
      ind_name = initial_collection_names[1]
    )
  })
  expect_true(
    (nrow(db_get_phenotypes(breeder = "A", ind_name = initial_collection_names[1])) !=
      nrow(db_get_phenotypes(breeder = "B", ind_name = initial_collection_names[1])))
  )
  expect_no_error({
    db_get_phenotypes(id = 1)
  })
  expect_no_error({
    db_get_phenotypes(ind_id = db_get_individuals_ids(breeder = "@ALL", initial_collection_names[2]))
  })
  expect_no_error({
    db_get_phenotypes(type = "pheno-field")
    db_get_phenotypes(type = "pheno-patho")
  })
  expect_no_error({
    db_get_phenotypes(request_name = "test_patho_only")
  })
  expect_no_error({
    db_get_phenotypes(plot = c(1, 99))
  })
  expect_no_error({
    positive_t1 <- db_get_phenotypes(min_t1 = 0)
  })
  expect_true({
    all(positive_t1$trait1 >= 0)
  })
  expect_no_error({
    positive_t2 <- db_get_phenotypes(min_t2 = 0)
  })
  expect_true({
    all(positive_t2$trait2 >= 0)
  })
  expect_no_error({
    negative_t1 <- db_get_phenotypes(max_t1 = 0)
  })
  expect_true({
    all(negative_t1$trait1 <= 0)
  })
  expect_no_error({
    negative_t2 <- db_get_phenotypes(max_t2 = 0)
  })
  expect_true({
    all(negative_t2$trait2 <= 0)
  })
  expect_no_error({
    db_get_phenotypes(t3 = 1)
  })
  expect_no_error({
    db_get_phenotypes(t3 = 0)
  })
  expect_no_error({
    db_get_phenotypes(
      pathogen = 1,
      t3 = 0
    )
  })
  expect_no_error({
    db_get_phenotypes(pathogen = 0)
  })
  expect_no_error({
    db_get_phenotypes(year = c(1950:2000))
  })


  # available date calculation
  pheno <- db_get_phenotypes()
  pheno_field <- pheno[pheno$type == "pheno-field", ]

  constants <- getBreedingGameConstants()
  expected_available_dates <- as.character(
    lubridate::add_with_rollback(
      as.Date(paste0(pheno_field$year, "-", constants$max.upload.pheno.field)),
      months(constants$duration.pheno.field),
      roll_to_first = TRUE
    )
  )
  expect_equal(pheno_field$avail_from, expected_available_dates)

  pheno_patho <- pheno[pheno$type == "pheno-patho", ]
  expected_available_dates <- as.character(
    lubridate::add_with_rollback(
      as.Date(pheno_patho$request_game_date),
      months(constants$duration.pheno.patho),
      roll_to_first = TRUE
    )
  )
  expect_equal(pheno_patho$avail_from, expected_available_dates)
})


test_that("genotype data", {
  expect_equal(
    db_get_all("geno_requests"),
    data.frame(
      id = numeric(),
      req_id = numeric(),
      ind_id = numeric(),
      ind_request_name = character(),
      type = character()
    )
  )
  expect_equal(
    db_get_all("genotypes"),
    data.frame(
      id = integer(0),
      geno_req_id = integer(0),
      type = character(),
      result_file = character()
    )
  )


  # Add initial geno data
  init_geno_req <- data.frame(
    ind = initial_collection_names[c(-1, -length(initial_collection_names))],
    task = "geno",
    details = "hd"
  )
  init_geno_req_id <- db_get_game_requests(name = "-- Initial Genotypes --")$id
  expect_no_error({
    new_data_init <- db_add_geno_req_data(init_geno_req_id, init_geno_req)
  })

  genotype_data_files <- list(hd = "data/whatever/Result_genos_subset-initialColl-hd.txt.gz")
  expect_no_error({
    db_add_geno_data(init_geno_req_id, genotype_data_files)
  })
  expect_error({
    db_add_geno_data(init_geno_req_id, genotype_data_files)
  })

  expect_no_error({
    initial_geno <- db_get_genotypes()
  })
  expect_equal(
    nrow(initial_geno),
    n_init_inds - 2
  )

  expect_identical(
    db_get_genotypes(breeder = "A"),
    initial_geno
  )
  expect_identical(
    db_get_genotypes(breeder = "A"),
    db_get_genotypes(breeder = "B")
  )


  for (generation in c(0, 1, 2)) {
    for (breeder in c("A", "B")) {
      if (generation == 0) {
        inds <- initial_collection_names[c(1, n_init_inds)]
      } else {
        inds <- db_get_individual(
          breeder = breeder,
          request_name = paste0("test_pltmat_", generation)
        )$name
      }
      mock_request <- data.frame(
        ind = c(inds, inds[1], inds[1], inds[2]),
        task = "geno",
        details = "ld"
      )
      mock_request$details[1:floor((nrow(mock_request) - 2) / 2)] <- "hd"
      mock_request$details[(nrow(mock_request) - 1):nrow(mock_request)] <- paste0("snp00", c(1, 2) + generation)

      request_name <- paste0("test_geno_", generation + 1)
      geno_req_id <- db_get_game_requests(
        breeder = breeder,
        name = request_name
      )[1, 1]

      # add geno request
      expect_no_error({
        new_geno_req_data <- db_add_geno_req_data(geno_req_id, mock_request)
      })

      # mock genotype
      geno_files <- list(
        hd = paste0("result_geno_", request_name, "-hd"),
        ld = paste0("result_geno_", request_name, "-ld"),
        singleSnp = paste0("result_geno_", request_name, "-snp")
      )

      # add phenotype data
      expect_no_error({
        db_add_geno_data(geno_req_id, geno_files)
      })
    }
  }


  # test access to genotypes data
  expect_no_error({
    db_get_genotypes()
  })
  expect_no_error({
    db_get_genotypes(
      breeder = "A",
      ind_name = initial_collection_names[1]
    )
  })
  expect_no_error({
    db_get_genotypes(id = 1)
  })
  expect_no_error({
    db_get_genotypes(ind_id = db_get_individuals_ids(breeder = "@ALL", initial_collection_names[2]))
  })
  expect_no_error({
    db_get_genotypes(type = "hd")
    db_get_genotypes(type = "ld")
  })
  expect_no_error({
    db_get_genotypes(breeder = "A", type = "snp002")
  })
  expect_no_error({
    db_get_genotypes(breeder = "A", type = "snp002")
  })
  expect_no_error({
    db_get_genotypes(breeder = "A", result_file = "result_geno_test_geno_2-hd")
  })

  expect_no_error({
    geno_data_list <- db_get_genotypes_data_list(breeder = "A")
  })
  expect_true(length(geno_data_list) != 0)
})


test_that("evaluation requests", {
  A_inds <- db_get_individual(breeder = "A", request_name = "test_pltmat_1")

  selected_inds_A <- A_inds[1:3, ]
  expect_no_error({
    db_add_evaluation_inds(
      breeder = "A",
      ind_ids = selected_inds_A$id,
      game_date = "2000-02-01"
    )
  })
  expect_no_error({
    eval_req <- db_get_evaluation_requests()
  })
  expect_equal(nrow(eval_req), nrow(selected_inds_A))
  expect_true(setequal(eval_req$ind_id, selected_inds_A$id))

  inds <- db_get_individual()
  expect_true(all(inds$selected_for_evaluation[inds$id %in% selected_inds_A$id] == 1))
  expect_true(all(inds$selected_for_evaluation[!inds$id %in% selected_inds_A$id] == 0))


  # remove 1 selected ind
  ind_to_remove <- selected_inds_A$id[1]
  removed_inds <- ind_to_remove
  expect_no_error({
    db_remove_evaluation_inds(breeder = "A", ind_ids = ind_to_remove, game_date = "2000-02-02")
  })
  eval_req <- db_get_evaluation_requests()
  expect_equal(nrow(eval_req), nrow(selected_inds_A) + length(removed_inds))
  expect_equal(nrow(eval_req[eval_req$action == "remove", ]), length(removed_inds))
  expect_true(setequal(eval_req$ind_id[eval_req$action == "remove"], ind_to_remove))
  inds <- db_get_individual()
  expect_true(all(inds$selected_for_evaluation[inds$id %in% setdiff(selected_inds_A$id, removed_inds)] == 1))
  expect_true(all(inds$selected_for_evaluation[inds$id %in% ind_to_remove] == 0))

  # remove 2 selected ind
  ind_to_remove <- selected_inds_A$id[2:3]
  removed_inds <- c(removed_inds, ind_to_remove)
  expect_no_error({
    db_remove_evaluation_inds(breeder = "A", ind_ids = ind_to_remove, game_date = "2000-02-03")
  })
  eval_req <- db_get_evaluation_requests()
  expect_equal(nrow(eval_req), nrow(selected_inds_A) + length(removed_inds))
  expect_equal(nrow(eval_req[eval_req$action == "remove", ]), length(removed_inds))
  expect_true(setequal(eval_req$ind_id[eval_req$action == "remove"], removed_inds))
  inds <- db_get_individual()
  expect_true(all(inds$selected_for_evaluation[inds$id %in% setdiff(selected_inds_A$id, removed_inds)] == 1))
  expect_true(all(inds$selected_for_evaluation[inds$id %in% ind_to_remove] == 0))

  # add back a removed ind
  added_back_ind <- selected_inds_A$id[1]
  db_add_evaluation_inds(
    breeder = "A",
    ind_ids = added_back_ind,
    game_date = "2000-02-01"
  )
  inds <- db_get_individual(ind_id = added_back_ind)
  expect_equal(inds$selected_for_evaluation, 1)
})




test_that("request history", {
  expect_no_error({
    db_get_all("v_request_history")
  })
  expect_no_error({
    db_get_game_requests_history()
  })
  expect_no_error({
    db_get_game_requests_history(breeder = "A")
  })
})


unlink(tmpdir, recursive = TRUE)
