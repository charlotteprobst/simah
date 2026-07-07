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
education_transitions <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "education_transitions.rds"))})

# 2. Define the test case
description <- "transition_education checks that the correct column is created"

test_that(description, {

  # Prior: setup education
  data <- setup_education(data = basepop)

  # Execute the function
  result <- data %>% dplyr::group_by(cat) %>%
    dplyr::do(transition_education(., education_transitions))

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("newED" %in% names(result))
})
