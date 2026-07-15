# Load testthat library
library(testthat)

# 1. Load the package
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

# Define data paths
DataDirectoryMinimal <- "inputs_data"

# Find the project root directory from inside the test folder
project_root <- rprojroot::find_package_root_file()

# Load necessary mock data
basepop <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "data.rds"))})

test_that("update_alcohol correct structure", {

  result <- update_alcohol_cat(basepop)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that number of rows matches input
  expect_equal(nrow(result), nrow(basepop))

  # Check that same number of columns (alc_cat added/updated)
  expect_equal(ncol(result), ncol(basepop))
})

test_that("update_alcohol_cat produces valid category values", {
  result <- update_alcohol_cat(basepop)

  # Check that alc_cat column exists
  expect_true("alc_cat" %in% names(result))

  # Check that only valid categories exist
  expected_categories <- c("Non-drinker", "Low risk", "Medium risk", "High risk", NA_character_)
  expect_true(all(result$alc_cat %in% expected_categories))

  # Check that no NA values in expected categories
  expect_true(all(!is.na(result$alc_cat[!is.na(result$alc_cat)])))
})

test_that("update_alcohol_cat correctly classifies non-drinkers (alc_gpd == 0)", {
  # Create test data with all alc_gpd == 0
  test_data <- data.frame(
    ID = 1:100,
    sex = sample(c("m", "f"), 100, replace = TRUE),
    alc_gpd = rep(0, 100),
    other_col = letters[1:100]  # Additional column to test preservation
  )

  result <- update_alcohol_cat(test_data)

  # All should be classified as Non-drinker
  expect_equal(result$alc_cat, rep("Non-drinker", 100))
})
