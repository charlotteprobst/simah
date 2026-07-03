# Load testthat library
library(testthat)

# 1. Load the package and data (similar to minimal_test_setup.R)
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

# Define data paths
DataDirectoryMinimal <- "inputs_data"

# Load necessary mock data
basepop <- readr::read_rds(file.path(DataDirectoryMinimal, "data.rds"))
risk_param <- readr::read_rds(file.path(DataDirectoryMinimal, "risk_param.rds"))

# 2. Define the test case
description <- "assign_rr_dm computes the relative risk of diabetes mellitus type 2"

test_that(description, {

  # Check that the needed risk function parameters are in risk_param
  parameter_expect <- c("B_DM_MEN", "B_DM1_WOMEN",
                        "B_DM2_WOMEN", "B_DM3_WOMEN",
                        "DM_FORMERDRINKER_MEN", "DM_FORMERDRINKER_WOMEN")
  expect_true(all( parameter_expect %in% names(risk_param) ))

  # Execute the function
  result <- assign_rr_dm(data = basepop, risk_param = risk_param)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("RR_DM" %in% names(result))
})
