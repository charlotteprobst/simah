# Load testthat library
library(testthat)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))
risk_param <- readr::read_rds(system.file("extdata", "risk_param.rds", package = "simah"))

# 2. Define the test case
description <- "assign_rr_istr computes the relative risk of ischaemic stroke (ISTR)"

test_that(description, {

  # Check that the needed risk function parameters are in risk_param
  parameter_expect <- c("B_ISTR1", "B_ISTR2", "B_ISTR3", "B_ISTR4",
                        "ISTR_FORMERDRINKER")
  expect_true(all( parameter_expect %in% names(risk_param) ))

  # Execute the function
  result <- assign_rr_istr(data = basepop, risk_param = risk_param)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("RR_ISTR" %in% names(result))
})
