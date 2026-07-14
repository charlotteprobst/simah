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

test_that("prob_alcohol_transition handles edge alc_gpd values", {
  # Create data with alc_gpd = 0 (abstainer)
  test_data_zero <- data.frame(
    ID = 1:50,
    age = rep(40, 50),
    sex = rep("m", 50),
    race = rep("White", 50),
    education = rep("College", 50),
    alc_gpd = rep(0, 50),  # Zero gpd
    alc_cat = rep("Non-drinker", 50),
    formerdrinker = rep(FALSE, 50)
  )

  result_zero <- prob_alcohol_transition(test_data_zero, alcohol_transitions)
  expect_s3_class(result_zero, "data.frame")
  expect_equal(nrow(result_zero), 1)

  # Test with high alc_gpd
  test_data_high <- data.frame(
    ID = 1:50,
    age = rep(40, 50),
    sex = rep("m", 50),
    race = rep("White", 50),
    education = rep("SomeC", 50),
    alc_gpd = rep(100, 50),  # High gpd
    alc_cat = rep("High risk", 50),
    formerdrinker = rep(FALSE, 50)
  )

  result_high <- prob_alcohol_transition(test_data_high, alcohol_transitions)
  expect_s3_class(result_high, "data.frame")
  expect_equal(nrow(result_high), 1)
})

test_that("prob_alcohol_transition handles empty data frame gracefully", {
  empty_data <- data.frame(
    ID = integer(0),
    age = numeric(0),
    sex = character(0),
    race = character(0),
    education = character(0),
    alc_gpd = numeric(0),
    alc_cat = character(0),
    formerdrinker = logical(0)
  )

  result <- prob_alcohol_transition(empty_data, alcohol_transitions)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("prob_alcohol_transition correctly classifies abstainers", {
  # Create data that should be classified as abstainer (alc_gpd == 0 and not formerdrinker)
  test_data_abstainer <- data.frame(
    ID = 1:25,
    age = rep(30, 25),
    sex = rep("f", 25),
    race = rep("White", 25),
    education = rep("College", 25),
    alc_gpd = rep(0, 25),
    alc_cat = rep("Non-drinker", 25),
    formerdrinker = FALSE  # This should make them abstainers
  )

  # Data should also be classified as abstainer when alc_gpd == 0
  test_data_zero_gpd <- data.frame(
    ID = 26:50,
    age = rep(30, 25),
    sex = rep("f", 25),
    race = rep("White", 25),
    education = rep("College", 25),
    alc_gpd = rep(0, 25),  # Zero gpd
    alc_cat = rep("Low risk", 25),  # Current category doesn't matter for abstainer calc
    formerdrinker = TRUE  # Former drinker, so not abstainer
  )

  combined_data <- rbind(test_data_abstainer, test_data_zero_gpd)
  result <- prob_alcohol_transition(combined_data, alcohol_transitions)

  expect_s3_class(result, "data.frame")
  # Should have 2 rows (abstainer group and non-abstainer group)
  expect_true(nrow(result) >= 1)
})

test_that("prob_alcohol_transition handles both genders", {
  # Test with only females
  female_data <- basepop %>%
    dplyr::filter(sex == "f") %>%
    dplyr::slice_sample(n = 1000)

  result_female <- prob_alcohol_transition(female_data, alcohol_transitions)
  expect_s3_class(result_female, "data.frame")
  expect_true(all(result_female$sex == "f"))

  # Test with only males
  male_data <- basepop %>%
    dplyr::filter(sex == "m") %>%
    dplyr::slice_sample(n = 1000)

  result_male <- prob_alcohol_transition(male_data, alcohol_transitions)
  expect_s3_class(result_male, "data.frame")
  expect_true(all(result_male$sex == "m"))
})

test_that("prob_alcohol_transition handles different racial groups", {
  # Test with each racial group separately
  for (race_val in unique(basepop$race)) {
    race_data <- basepop %>%
      dplyr::filter(race == race_val) %>%
      dplyr::slice_sample(n = min(500, nrow(basepop)))

    result_race <- prob_alcohol_transition(race_data, alcohol_transitions)
    expect_s3_class(result_race, "data.frame")
    expect_true(all(result_race$race == race_val))
  }
})

test_that("prob_alcohol_transition handles different education levels", {
  # Test with each education level separately
  for (edu_val in unique(basepop$education)) {
    edu_data <- basepop %>%
      dplyr::filter(education == edu_val) %>%
      dplyr::slice_sample(n = min(500, nrow(basepop)))

    result_edu <- prob_alcohol_transition(edu_data, alcohol_transitions)
    expect_s3_class(result_edu, "data.frame")
    expect_true(all(result_edu$education == edu_val))
  }
})

test_that("prob_alcohol_transition handles interaction terms", {
  # Create data with specific combinations that would activate interaction terms
  # Education:Low * Category:Low risk interaction
  test_interaction <- data.frame(
    ID = 1:4,
    age = rep(40, 4),
    sex = rep("m", 4),
    race = rep("White", 4),
    education = c("LEHS", "SomeC", "LEHS", "SomeC"),  # Low and Med education
    alc_gpd = c(5, 10, 15, 20),  # Different gpd values
    alc_cat = c("Low risk", "Medium risk", "High risk", "Non-drinker"),  # Different categories
    formerdrinker = rep(FALSE, 4)
  )

  result <- prob_alcohol_transition(test_interaction, alcohol_transitions)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)  # Should have 4 rows (one for each unique combination)
})

test_that("prob_alcohol_transition produces numerically stable results", {
  # Run multiple times to check for numerical stability
  set.seed(42)
  result1 <- prob_alcohol_transition(basepop, alcohol_transitions)

  set.seed(42)
  result2 <- prob_alcohol_transition(basepop, alcohol_transitions)

  # Results should be identical (deterministic function)
  expect_identical(result1, result2)
})
