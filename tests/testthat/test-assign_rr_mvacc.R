# Load testthat library
library(testthat)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))
risk_param <- readr::read_rds(system.file("extdata", "risk_param.rds", package = "simah"))

# read in hed model
hed_model_list <- list(
  'youngmen' = xgboost::xgb.load(system.file("extdata", "xgbmodel_youngmen.json", package = "simah")),
  'else'     = xgboost::xgb.load(system.file("extdata", "xgbmodel_else.json", package = "simah")),
  'oldmen'   =  xgboost::xgb.load(system.file("extdata", "xgbmodel_oldmen.json", package = "simah"))
)

# 2. Define the test case
description <- "assign_rr_mvacc computes the relative risk of motor vehicle injuries (MVACC)"

test_that(description, {

  # Check that the needed risk function parameters are in risk_param
  parameter_expect <- c("B_MVACC1", "B_MVACC2", "MVACC_FORMERDRINKER")
  expect_true(all( parameter_expect %in% names(risk_param) ))

  # Update HED
  data <- update_hed(data = basepop,
                     hed_model1 = hed_model_list[[1]],
                     hed_model2 = hed_model_list[[2]],
                     hed_model3 = hed_model_list[[3]])

  # Execute the function
  result <- assign_rr_mvacc(data = data, risk_param = risk_param)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("RR_MVACC" %in% names(result))
})
