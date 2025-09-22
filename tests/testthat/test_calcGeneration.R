library(testthat)

source("../../src/fun/func_admin.R", local = TRUE, encoding = "UTF-8")


# Test for calcGeneration function
test_that("calcGeneration works correctly", {
  # Simple pedigree with founders and one generation
  ped1 <- data.frame(
    id = c("F1", "F2", "G0"),
    parent1_id = c(NA, NA, "F1"),
    parent2_id = c(NA, NA, "F2")
  )
  result1 <- calcGeneration(ped1)
  expect_equal(result1, c(-1, -1, 0))

  # Simple pedigree without founders and one generation
  ped2 <- data.frame(
    id = c("G0"),
    parent1_id = c("F1"),
    parent2_id = c("F2")
  )
  result2 <- calcGeneration(ped2)
  expect_equal(result2, c(0))

  # Multi-generation pedigree
  ped3 <- data.frame(
    id = c("G0-1", "G0-2", "G1-1"),
    parent1_id = c("F1", "F1", "G0-1"),
    parent2_id = c("F2", "F2", "G0-2")
  )

  result3 <- calcGeneration(ped3)
  expect_equal(result3, c(0, 0, 1))

  # Test with specific ids parameter
  result4 <- calcGeneration(ped3, ids = c("G1-1"))
  expect_equal(result4, c(1))

  # Single founder
  ped5 <- data.frame(
    id = "F1",
    parent1_id = NA,
    parent2_id = NA
  )

  result5 <- calcGeneration(ped5)
  expect_equal(result5, -1)

  # Pedigree with haplodiplo (parent2 is NA)
  ped6 <- data.frame(
    id = c("F1", "C1"),
    parent1_id = c(NA, "F1"),
    parent2_id = c(NA, NA)
  )

  result6 <- calcGeneration(ped6)
  expect_equal(result6, c(-1, 0))


  # Pedigree with autofecundation
  ped7 <- data.frame(
    id = c("F1", "C1"),
    parent1_id = c(NA, "F1"),
    parent2_id = c(NA, "F1")
  )

  result7 <- calcGeneration(ped7)
  expect_equal(result7, c(-1, 0))


  # Check that function handles character and numeric IDs
  ped8 <- data.frame(
    id = c(1, 2, 3),
    parent1_id = c(NA, NA, 1),
    parent2_id = c(NA, NA, 2)
  )

  result8 <- calcGeneration(ped8)
  expect_equal(result8, c(-1, -1, 0))


  # Check with complex example
  ped9 <- data.frame(
    id = c("G0-1", "G0-2", "G1-1", "G1-2", "G2-1", "G2-2", "G2-3", "G2-4", "G2-5"),
    parent1_id = c("F1", "F2", "G0-1", "G0-1", "G1-1", "G1-1", "G1-2", "G1-1", "G1-1"),
    parent2_id = c(NA, NA, "G0-2", "G0-2", "G1-2", "G1-1", NA, "G0-1", "F1")
  )
  result9 <- calcGeneration(ped9)
  expect_equal(result9, c(0, 0, 1, 1, 2, 2, 2, 2, 2))


  # Test with bad pedigree (loop in the graph)
  ped10 <- data.frame(
    id = c("A", "B", "C"),
    parent1_id = c("B", "A", NA),
    parent2_id = c("C", "C", NA)
  )
  expect_error(calcGeneration(ped10))
})
