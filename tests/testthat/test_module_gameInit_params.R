
library(testthat)
source("../../src/fun/module_gameInit_params.R", local = TRUE, encoding = "UTF-8")
source("../../src/fun/func_gameInit_validation.R", local = TRUE, encoding = "UTF-8")


set.seed(NULL)
seed = runif(1, 1000000, 9999999)
set.seed(seed)

expected_random_1 <- rnorm(10)
expected_random_2 <- rnorm(10)
expected_random_3 <- rnorm(10)

set.seed(NULL)

test_that("quick_afs_simul don't alter RNG seed", {

  set.seed(seed)

  afs_1 <- quick_afs_simul(
    n_marker_sim = 1000,
    shape1 = 0.5,
    shape2 = 0.5,
    seed = 1)
  random_1 <- rnorm(10)

  afs_2 <- quick_afs_simul(
    n_marker_sim = 1000,
    shape1 = 0.5,
    shape2 = 0.5,
    seed = 1)
  random_2 <- rnorm(10)

  quick_afs_simul(
    n_marker_sim = 1000,
    shape1 = 0.5,
    shape2 = 0.5,
    seed = NULL)
  random_3 <- rnorm(10)

  expect_equal(afs_1, afs_2)
  expect_equal(random_1, expected_random_1)
  expect_equal(random_2, expected_random_2)
  expect_equal(random_3, expected_random_3)

  set.seed(NULL)

})

test_that("quick_geno_simul don't alter RNG seed", {

  afs <- quick_afs_simul(
    n_marker_sim = 1000,
    shape1 = 0.5,
    shape2 = 0.5,
    seed = NULL
  )

  set.seed(seed)

  geno_1 <- quick_geno_simul(
    afs,
    n_inds = 1000,
    seed = 2
  )
  random_1 <- rnorm(10)

  geno_2 <- quick_geno_simul(
    afs,
    n_inds = 1000,
    seed = 2
  )
  random_2 <- rnorm(10)

  quick_geno_simul(
    afs,
    n_inds = 1000,
    seed = NULL
  )
  random_3 <- rnorm(10)

  expect_equal(geno_1, geno_2)
  expect_equal(random_1, expected_random_1)
  expect_equal(random_2, expected_random_2)
  expect_equal(random_3, expected_random_3)

  set.seed(NULL)

})

test_that("quick_g0_simul don't alter RNG seed", {

  afs <- quick_afs_simul(
    n_marker_sim = 1000,
    shape1 = 0.5,
    shape2 = 0.5,
    seed = NULL
  )
  geno <- quick_geno_simul(
    afs,
    n_inds = 1000,
    seed = NULL
  )

  set.seed(seed)

  g0_1 <- quick_g0_simul(
    T1_sig_a2 = 100,
    T2_sig_a2 = 0.8,
    afs = afs,
    prop_pleio = 0.5,
    cor_pleio = -0.7,
    geno = geno,
    seed = 3
  )
  random_1 <- rnorm(10)

  g0_2 <- quick_g0_simul(
    T1_sig_a2 = 100,
    T2_sig_a2 = 0.8,
    afs = afs,
    prop_pleio = 0.5,
    cor_pleio = -0.7,
    geno = geno,
    seed = 3
  )
  random_2 <- rnorm(10)

  quick_g0_simul(
    T1_sig_a2 = 100,
    T2_sig_a2 = 0.8,
    afs = afs,
    prop_pleio = 0.5,
    cor_pleio = -0.7,
    geno = geno,
    seed = NULL
  )
  random_3 <- rnorm(10)

  expect_equal(g0_1, g0_2)
  expect_equal(random_1, expected_random_1)
  expect_equal(random_2, expected_random_2)
  expect_equal(random_3, expected_random_3)

  set.seed(NULL)

})



test_that("quick_pheno_simul don't alter RNG seed", {
  afs <- quick_afs_simul(
    n_marker_sim = 1000,
    shape1 = 0.5,
    shape2 = 0.5,
    seed = NULL
  )
  geno <- quick_geno_simul(
    afs,
    n_inds = 1000,
    seed = NULL
  )
  g0 <- quick_g0_simul(
    T1_sig_a2 = 100,
    T2_sig_a2 = 0.8,
    afs = afs,
    prop_pleio = 0.5,
    cor_pleio = -0.7,
    geno = geno,
    seed = NULL
  )

  set.seed(seed)

  pheno_1 <- quick_pheno_simul(
    mu = 100,
    sig_p = 700,
    sig = 200,
    sig_y = 300,
    g0 = g0[,1],
    seed = 4
  )
  random_1 <- rnorm(10)

  pheno_2 <- quick_pheno_simul(
    mu = 100,
    sig_p = 700,
    sig = 200,
    sig_y = 300,
    g0 = g0[,1],
    seed = 4
  )
  random_2 <- rnorm(10)

  quick_pheno_simul(
    mu = 100,
    sig_p = 700,
    sig = 200,
    sig_y = 300,
    g0 = g0[,2],
    seed = NULL
  )
  random_3 <- rnorm(10)

  expect_equal(pheno_1, pheno_2)
  expect_equal(random_1, expected_random_1)
  expect_equal(random_2, expected_random_2)
  expect_equal(random_3, expected_random_3)

  set.seed(NULL)

})
