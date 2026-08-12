# Load testthat library
library(testthat)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))
risk_param <- readr::read_rds(system.file("extdata", "risk_param.rds", package = "simah"))

# 2. Define the test case
description <- "assign_rr_hlvdc computes the relative risk of liver cirrhosis through hepatitis pathway"

test_that(description, {

  # Check that the needed risk function parameters are in risk_param
  expect_true(all( c("B_HLVDC1", "B_HLVDC2") %in% names(risk_param) ))

  # Execute the function
  result <- assign_rr_hlvdc(data = basepop, risk_param = risk_param)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("RR_HLVDC" %in% names(result))
})
