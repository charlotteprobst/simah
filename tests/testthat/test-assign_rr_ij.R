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
risk_param <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "risk_param.rds"))})

# 2. Define the test case
description <- "assign_rr_ij computes the relative risk of intentional injuries (IJ)"

test_that(description, {

  # Check that the needed risk function parameters are in risk_param
  parameter_expect <- c("B_IJ_MEN", "B_IJ_WOMEN",
                        "IJ_FORMERDRINKER_MEN", "IJ_FORMERDRINKER_WOMEN")
  expect_true(all( parameter_expect %in% names(risk_param) ))

  # Execute the function
  result <- assign_rr_ij(data = basepop, risk_param = risk_param)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("RR_IJ" %in% names(result))
})
