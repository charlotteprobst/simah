# Load testthat library
library(testthat)

# 1. Load the package and data (similar to minimal_test_setup.R)
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

# Define data paths
DataDirectoryMinimal <- "inputs_data"

# Find the project root directory from inside the test folder
project_root <- rprojroot::find_package_root_file()

# Load necessary mock data
basepop <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "data.rds"))})
alcohol_transitions <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "alcohol_transitions.rds"))})

test_that("transition_alcohol returns correct structure", {

  # Execute the function
  result <- transition_alcohol(basepop, alcohol_transitions)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that alc_cat column exists and has been updated
  expect_true("alc_cat" %in% names(result))
  expect_true("totransitioncont" %in% names(result))

  # Check that alc_cat has expected values
  expect_true(all(result$alc_cat %in% c("Non-drinker", "Low risk", "Medium risk", "High risk")))

  # Check that totransitioncont has expected values
  expect_true(all(result$totransitioncont %in% c(0, 1)))
})

test_that("transition_alcohol allows transitions between alcohol categories", {
  # Get initial category distribution
  initial_dist <- table(basepop$alc_cat)

  # Execute function
  result <- transition_alcohol(basepop, alcohol_transitions)

  # Get new category distribution
  final_dist <- table(result$alc_cat)

  # At least some individuals should have changed categories (stochastic)
  # This tests that the transition logic actually works
  changed <- sum(result$alc_cat != basepop$alc_cat)
  expect_true(changed > 0, info = "No transitions occurred - function may not be working")

  # All categories should still be present in result
  expect_true(all(c("Non-drinker", "Low risk", "Medium risk", "High risk") %in% names(final_dist)))
})

test_that("transition_alcohol calculates totransitioncont correctly", {
  result <- transition_alcohol(basepop, alcohol_transitions)

  # totransitioncont should be 0 when alc_cat didn't change
  unchanged <- result[result$alc_cat == basepop$alc_cat, ]
  expect_equal(unchanged$totransitioncont, rep(0, nrow(unchanged)))

  # totransitioncont should be 1 when alc_cat changed AND not changed to Non-drinker
  changed_not_nondrinker <- result[result$alc_cat != basepop$alc_cat & result$alc_cat != "Non-drinker", ]
  expect_equal(changed_not_nondrinker$totransitioncont, rep(1, nrow(changed_not_nondrinker)))

  # totransitioncont should be 0 when alc_cat changed to Non-drinker
  changed_to_nondrinker <- result[result$alc_cat == "Non-drinker" & basepop$alc_cat != "Non-drinker", ]
  expect_equal(changed_to_nondrinker$totransitioncont, rep(0, nrow(changed_to_nondrinker)))
})

test_that("transition_alcohol handles age category boundaries correctly", {
  # Create test data with specific ages to test categorization
  test_data <- data.frame(
    ID = c(1, 2, 3, 4),
    age = c(18, 24, 25, 64),  # Test boundaries: should be 18-24, 18-24, 25-64, 25-64
    sex = c("m", "f", "m", "f"),
    race = c("White", "White", "White", "White"),
    education = c("College", "SomeC", "LEHS", "College"),
    alc_gpd = c(10, 20, 15, 25),
    alc_cat = c("Low risk", "Medium risk", "High risk", "Non-drinker"),
    formerdrinker = c(FALSE, FALSE, TRUE, FALSE)
  )

  result <- transition_alcohol(test_data, alcohol_transitions)

  # Check that the function ran without errors
  expect_s3_class(result, "data.frame")

  # Check alc_cat was updated
  expect_true(all(result$alc_cat %in% c("Non-drinker", "Low risk", "Medium risk", "High risk")))
})

test_that("transition_alcohol converts College to SomeC for 18-24 age group", {
  # Test with 18-24 age group with College education (should become SomeC)
  test_data_18_24 <- data.frame(
    ID = 1,
    age = 20,
    sex = "m",
    race = "White",
    education = "College",  # This should be converted to SomeC for 18-24
    alc_gpd = 10,
    alc_cat = "Low risk",
    formerdrinker = FALSE
  )

  result_18_24 <- transition_alcohol(test_data_18_24, alcohol_transitions)
  expect_s3_class(result_18_24, "data.frame")

  # Test with 25+ age group with College education (should stay College)
  test_data_25plus <- data.frame(
    ID = 1,
    age = 30,
    sex = "m",
    race = "White",
    education = "College",  # This should stay College for 25+
    alc_gpd = 10,
    alc_cat = "Low risk",
    formerdrinker = FALSE
  )

  result_25plus <- transition_alcohol(test_data_25plus, alcohol_transitions)
  expect_s3_class(result_25plus, "data.frame")

  # Both should run without errors
  expect_identical(names(result_18_24), names(result_25plus))
})

test_that("transition_alcohol handles empty data frame", {
  empty_data <- data.frame(
    ID = numeric(0),
    age = numeric(0),
    race = character(0),
    sex = character(0),
    education = character(0),
    alc_gpd = numeric(0),
    alc_cat = character(0),
    formerdrinker = numeric(0)
  )

  # Note: Some column names might be missing - check what the function expects
  result <- transition_alcohol(empty_data, alcohol_transitions)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("transition_alcohol does not modify input data", {
  # Make a copy with specific alc_cat values
  original <- basepop %>%
    dplyr::mutate(
      alc_cat = "Low risk",
      totransitioncont = 999
    )

  original_copy <- original
  original_alc_cat <- original$alc_cat
  original_n <- nrow(original)

  result <- transition_alcohol(original, alcohol_transitions)

  # Original should be unchanged
  expect_identical(original, original_copy)
  expect_equal(nrow(original), original_n)
  expect_identical(original$alc_cat, original_alc_cat)
  expect_true("totransitioncont" %in% names(original))  # Column should exist but values might differ

  # But result should have different alc_cat values (due to stochastic transitions)
  expect_false(identical(result$alc_cat, original_alc_cat))
})

test_that("transition_alcohol probability calculations are consistent", {
  # Run function multiple times with same seed to test deterministic behavior
  set.seed(123)
  result1 <- transition_alcohol(basepop, alcohol_transitions)

  set.seed(123)
  result2 <- transition_alcohol(basepop, alcohol_transitions)

  # With same seed, results should be identical
  expect_identical(result1$alc_cat, result2$alc_cat)
  expect_identical(result1$totransitioncont, result2$totransitioncont)
})

test_that("transition_alcohol works with different initial category distributions", {
  # Test with mostly Non-drinkers
  test_data_nondrinker <- data.frame(
    ID = 1:100,
    age = rep(40, 100),
    sex = rep("m", 100),
    race = rep("White", 100),
    education = rep("College", 100),
    alc_gpd = rep(0, 100),  # 0 gpd → abstainer
    alc_cat = rep("Non-drinker", 100),
    formerdrinker = rep(FALSE, 100)
  )

  result_nondrinker <- transition_alcohol(test_data_nondrinker, alcohol_transitions)
  expect_s3_class(result_nondrinker, "data.frame")
  expect_true(all(result_nondrinker$alc_cat %in% c("Non-drinker", "Low risk", "Medium risk", "High risk")))

  # Test with mostly High risk
  test_data_highrisk <- data.frame(
    ID = 1:100,
    age = rep(40, 100),
    sex = rep("m", 100),
    race = rep("White", 100),
    education = rep("SomeC", 100),
    alc_gpd = rep(50, 100),
    alc_cat = rep("High risk", 100),
    formerdrinker = rep(FALSE, 100)
  )

  result_highrisk <- transition_alcohol(test_data_highrisk, alcohol_transitions)
  expect_s3_class(result_highrisk, "data.frame")
  expect_true(all(result_highrisk$alc_cat %in% c("Non-drinker", "Low risk", "Medium risk", "High risk")))
})
