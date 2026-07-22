# Load testthat library
library(testthat)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))

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
