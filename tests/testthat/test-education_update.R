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
education_transitions_covid <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "education_transitions_covid.rds"))})

# 2. Define the test case
test_that("education_update test non-COVID for year 2020 ", {

  covid_scenario <- 0  # non-COVID
  year <- 2020

  # Execute the function
  totransition <- basepop %>% dplyr::filter(age <= 34)
  result <- education_update(data = totransition, covid_scenario = covid_scenario, cyear=year,
                             education_transitions = education_transitions,
                             education_transitions_covid = education_transitions_covid)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("education_detailed" %in% names(result) )
})

test_that("education_update test non-COVID before 2020 and after 2022 / COVID in 2020-2022", {

  covid_scenario <- 1  # non-COVID
  year <- 2020

  # Execute the function
  totransition <- basepop %>% dplyr::filter(age <= 34)
  result <- education_update(data = totransition, covid_scenario = covid_scenario, cyear=year,
                             education_transitions = education_transitions,
                             education_transitions_covid = education_transitions_covid)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("education_detailed" %in% names(result) )
})

test_that("education_update test non-COVID before 2020 / COVID after 2020", {

  covid_scenario <- 2  # non-COVID
  year <- 2020

  # Execute the function
  totransition <- basepop %>% dplyr::filter(age <= 34)
  result <- education_update(data = totransition, covid_scenario = covid_scenario, cyear=year,
                             education_transitions = education_transitions,
                             education_transitions_covid = education_transitions_covid)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("education_detailed" %in% names(result) )
})
