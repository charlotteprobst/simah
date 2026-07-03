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

# 2. Define the test case
test_that("setup_education check the required columns have been created", {

  # Execute the function
  result <- setup_education(data = basepop)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true(all(c("agecat", "state", "cat", "prob") %in% names(result)))
})

test_that("setup_education_covid check the required columns have been created", {

  # Execute the function
  result <- setup_education_covid(data = basepop)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true(all(c("agecat", "state", "cat", "prob") %in% names(result)))
})
