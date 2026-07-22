# Load testthat library
library(testthat)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))
education_transitions <- readr::read_rds(system.file("extdata", "education_transitions.rds", package = "simah"))
education_transitions_covid <- readr::read_rds(system.file("extdata", "education_transitions_covid.rds", package = "simah"))

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
