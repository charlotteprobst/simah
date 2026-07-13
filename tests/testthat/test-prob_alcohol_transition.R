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

test_that("prob_alcohol_transition returns correct structure and columns", {

  # Execute the function
  result <- prob_alcohol_transition(basepop, alcohol_transitions)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that all expected columns exist
  expect_true("alc_cat" %in% names(result))
  expect_true("sex" %in% names(result))
  expect_true("agecat" %in% names(result))
  expect_true("race" %in% names(result))
  expect_true("education" %in% names(result))

  # Check that probability columns exist
  expect_true("prob_nondrinker" %in% names(result))
  expect_true("prob_low" %in% names(result))
  expect_true("prob_med" %in% names(result))
  expect_true("prob_high" %in% names(result))
})

test_that("prob_alcohol_transition probabilities are in valid range", {
  result <- prob_alcohol_transition(basepop, alcohol_transitions)

  # All probabilities should be between 0 and 1
  expect_true(all(result$prob_nondrinker >= 0 & result$prob_nondrinker <= 1))
  expect_true(all(result$prob_low >= 0 & result$prob_low <= 1))
  expect_true(all(result$prob_med >= 0 & result$prob_med <= 1))
  expect_true(all(result$prob_high >= 0 & result$prob_high <= 1))

  # Probabilities should sum to 1 for each row
  row_sums <- result$prob_nondrinker + result$prob_low +
    result$prob_med + result$prob_high
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("prob_alcohol_transition handles agecat values correctly", {
  result <- prob_alcohol_transition(basepop, alcohol_transitions)

  # Should have the expected age categories from the function
  expected_agecats <- c("18-24", "25-64", "65+")
  expect_true(all(result$agecat %in% expected_agecats))

  # Check that all age categories from the input data are represented
  input_agecats <- cut(basepop$age,
                       breaks = c(0, 24, 64, 100),
                       labels = c("18-24", "25-64", "65+"))
  expect_true(all(unique(input_agecats) %in% result$agecat))
})

test_that("prob_alcohol_transition groups by all expected dimensions", {
  result <- prob_alcohol_transition(basepop, alcohol_transitions)

  # Check that we have groups for all alc_cat values in input
  expect_true(all(c("Low risk", "Medium risk", "High risk", "Non-drinker") %in% result$alc_cat))

  # Check that all sex values are represented
  expect_true(all(c("f", "m") %in% result$sex))

  # Check that all race values from input are represented
  expect_true(all(unique(basepop$race) %in% result$race))

  # Check that all education values from input are represented
  expect_true(all(unique(basepop$education) %in% result$education))
})

test_that("prob_alcohol_transition handles different input category distributions", {
  # Test with mostly non-drinkers
  low_risk_data <- basepop %>%
    dplyr::filter(alc_cat == "Low risk") %>%
    dplyr::slice_head(n = 100)

  result_low <- prob_alcohol_transition(low_risk_data, alcohol_transitions)
  expect_s3_class(result_low, "data.frame")

  # Test with high risk group only
  high_risk_data <- basepop %>%
    dplyr::filter(alc_cat == "High risk") %>%
    dplyr::slice_head(n = 100)

  result_high <- prob_alcohol_transition(high_risk_data, alcohol_transitions)
  expect_s3_class(result_high, "data.frame")
})
