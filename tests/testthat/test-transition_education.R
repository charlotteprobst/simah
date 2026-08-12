# Load testthat library
library(testthat)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))
education_transitions <- readr::read_rds(system.file("extdata", "education_transitions.rds", package = "simah"))

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
