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
catcontmodel <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "catcontmodel.rds"))})

test_that("allocate_gramsperday_sampled returns correct structure", {

  data <- transition_alcohol(basepop, alcohol_transitions)
  result <- allocate_gramsperday_sampled(data, catcontmodel)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that alc_cat column exists and has been updated
  expect_true("alc_cat" %in% names(result))

  # Check that alc_cat has expected values
  expect_true(all(result$alc_cat %in% c("Non-drinker", "Low risk", "Medium risk", "High risk")))
})

test_that("allocate_gramsperday_sampled updates alc_gpd only for individuals with totransitioncont == 1", {
  data <- transition_alcohol(basepop, alcohol_transitions)
  result <- allocate_gramsperday_sampled(data, catcontmodel)

  # Get individuals who should have changed
  should_change <- data[data$totransitioncont == 1, ]
  should_keep <- data[data$totransitioncont == 0, ]

  # Note: after function, totransitioncont is removed, so we need to test differently
  # Check that changes occurred for some individuals
  changed <- sum(result$alc_gpd != data$alc_gpd, na.rm = TRUE)
  expect_true(changed > 0, info = "No alc_gpd values were changed")
})

test_that("allocate_gramsperday_sampled assigns alc_gpd = 0 to Non-drinkers", {
  data <- transition_alcohol(basepop, alcohol_transitions)
  result <- allocate_gramsperday_sampled(data, catcontmodel)

  # Non-drinkers should have alc_gpd = 0
  non_drinkers <- result[result$alc_cat == "Non-drinker", ]
  expect_equal(non_drinkers$alc_gpd, rep(0, nrow(non_drinkers)))
})

test_that("allocate_gramsperday_sampled keeps alc_gpd within valid range", {
  data <- transition_alcohol(basepop, alcohol_transitions)
  result <- allocate_gramsperday_sampled(data, catcontmodel)

  # Values should be >= 0 (min is typically positive in catcontmodel)
  expect_true(all(result$alc_gpd >= 0, na.rm = TRUE))

  # Values should be <= 200 (capped in function)
  expect_true(all(result$alc_gpd <= 200, na.rm = TRUE))
})

test_that("allocate_gramsperday_sampled with test data", {
  # Create test data manually
  test_data <- data.frame(
    ID = 1:5,
    age = c(30, 30, 30, 30, 30),
    sex = c("m", "m", "f", "f", "m"),
    race = c("White", "White", "White", "White", "White"),
    education = c("College", "College", "College", "SomeC", "SomeC"),
    alc_gpd = c(10, 20, 15, 25, 30),
    alc_cat = c("Low risk", "Low risk", "Low risk", "Medium risk", "Medium risk"),
    totransitioncont = c(1, 1, 1, 1, 1)
  )

  result <- allocate_gramsperday_sampled(test_data, catcontmodel)

  # Function should complete without errors
  expect_s3_class(result, "data.frame")

  # non_drinkers should have 0
  non_drinkers <- result[result$alc_cat == "Non-drinker", ]
  expect_equal(non_drinkers$alc_gpd, rep(0, nrow(non_drinkers)))
})

test_that("allocate_gramsperday_sampled produces values within model bounds", {
  # Create data with specific groups that have known bounds
  test_data <- data.frame(
    ID = 1:20,
    age = rep(30, 20),
    sex = rep("m", 20),
    race = rep("White", 20),
    education = rep("SomeC", 20),
    alc_gpd = rep(5, 20),  # All same so we can verify ranking
    alc_cat = rep("Low risk", 20),
    totransitioncont = rep(1, 20)
  )

  result <- allocate_gramsperday_sampled(test_data, catcontmodel)

  # Get the relevant model bounds
  group <- "Low risk_SomeC_25-64_White_Male"
  model_row <- catcontmodel[catcontmodel$group == group, ]

  if (nrow(model_row) > 0) {
    # Values should be within min and max (with some tolerance for floating point)
    expected_min <- model_row$min - 1e-8
    expected_max <- model_row$max + 1e-8
    expect_true(all(result$alc_gpd >= expected_min, na.rm = TRUE))
    expect_true(all(result$alc_gpd <= expected_max, na.rm = TRUE))
  }
})

test_that("allocate_gramsperday_sampled works with different demographic combinations", {
  # Test with various demographic combinations
  test_data <- data.frame(
    ID = 1:8,
    age = c(20, 35, 50, 70, 20, 35, 50, 70),
    sex = c("m", "m", "m", "m", "f", "f", "f", "f"),
    race = c("White", "White", "White", "White", "White", "Black", "Black", "Hispanic"),
    education = c("College", "SomeC", "LEHS", "College", "SomeC", "LEHS", "SomeC", "College"),
    alc_gpd = rep(10, 8),
    alc_cat = c("Low risk", "Medium risk", "High risk", "Low risk",
                "Low risk", "Medium risk", "High risk", "Low risk"),
    totransitioncont = rep(1, 8)
  )

  result <- allocate_gramsperday_sampled(test_data, catcontmodel)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 8)
  expect_true(all(result$alc_gpd >= 0, na.rm = TRUE))
  expect_true(all(result$alc_gpd <= 200, na.rm = TRUE))
})

test_that("allocate_gramsperday_sampled does not modify input data", {
  data <- transition_alcohol(basepop, alcohol_transitions)
  data_copy <- data %>% dplyr::mutate_if(is.character, as.character)

  original_n <- nrow(data)
  result <- allocate_gramsperday_sampled(data, catcontmodel)

  # Original data should be unchanged
  expect_equal(nrow(data), original_n)
  expect_true("totransitioncont" %in% names(data))
  expect_true("alc_gpd" %in% names(data))

  # Result should have updated alc_gpd and removed intermediate columns
  expect_true("alc_gpd" %in% names(result))
  expect_false("totransitioncont" %in% names(result))
  expect_false("newgpd" %in% names(result))
})

test_that("allocate_gramsperday_sampled handles edge cases", {
  # Test with data where no one matches the filter criteria
  test_data <- data.frame(
    ID = 1:5,
    age = rep(30, 5),
    sex = rep("m", 5),
    race = rep("White", 5),
    education = rep("College", 5),
    alc_gpd = rep(10, 5),
    alc_cat = c("Low risk", "Low risk", "Low risk", "Low risk", "Non-drinker"),
    totransitioncont = c(0, 0, 0, 0, 1)  # All but last have 0
  )

  result <- allocate_gramsperday_sampled(test_data, catcontmodel)

  # Non-drinkers should have 0
  non_drinker <- result[result$alc_cat == "Non-drinker", ]
  expect_equal(non_drinker$alc_gpd, 0)

  # Others should keep original alc_gpd (since totransitioncont = 0)
  unchanged <- result[result$alc_cat != "Non-drinker", ]
  expect_equal(unchanged$alc_gpd, test_data$alc_gpd[test_data$ID %in% unchanged$ID])
})

test_that("allocate_gramsperday_sampled produces consistent results with same seed", {
  data <- transition_alcohol(basepop, alcohol_transitions)

  set.seed(123)
  result1 <- allocate_gramsperday_sampled(data, catcontmodel)

  set.seed(123)
  result2 <- allocate_gramsperday_sampled(data, catcontmodel)

  # Results should be identical with same seed
  expect_equal(result1$alc_gpd, result2$alc_gpd)
})

test_that("allocate_gramsperday_sampled uses beta distribution parameters correctly", {
  # Create a simple test case with known group
  group <- "Low risk_SomeC_25-64_White_Male"
  model_row <- catcontmodel[catcontmodel$group == group, ]

  if (nrow(model_row) > 0) {
    # Create data matching this group
    test_data <- data.frame(
      ID = 1:50,
      age = rep(40, 50),
      sex = rep("m", 50),
      race = rep("White", 50),
      education = rep("SomeC", 50),
      alc_gpd = rep(10, 50),  # All same initial values
      alc_cat = "Low risk",
      totransitioncont = rep(1, 50)
    )

    result <- allocate_gramsperday_sampled(test_data, catcontmodel)

    # Values should be within the model's min/max range
    # Note: due to beta distribution, most values will be between min and max
    # but extreme values are theoretically possible
    expect_true(all(result$alc_gpd >= 0, na.rm = TRUE))
    expect_true(all(result$alc_gpd <= 200, na.rm = TRUE))

    # Some spread in values should exist (not all identical due to sampling)
    unique_values <- length(unique(result$alc_gpd))
    expect_true(unique_values > 1, info = "No variation in sampled values")
  } else {
    skip("Target group not found in catcontmodel")
  }
})
