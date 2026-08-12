# Load testthat library
library(testthat)

options(microsim_verbosity = 0)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))
risk_param <- readr::read_rds(system.file("extdata", "risk_param.rds", package = "simah"))

# read in hed model
hed_model_list <- list(
  'youngmen' = xgboost::xgb.load(system.file("extdata", "xgbmodel_youngmen.json", package = "simah")),
  'else'     = xgboost::xgb.load(system.file("extdata", "xgbmodel_else.json", package = "simah")),
  'oldmen'   =  xgboost::xgb.load(system.file("extdata", "xgbmodel_oldmen.json", package = "simah"))
)

diseases <- c("AUD", "DM", "HLVDC", "HYPHD", "IHD", "IJ", "ISTR", "LVDC", "MVACC", "UIJ")
year <- 2010

# 2. Define the test case
description <- "assign_rr_uij computes the relative risk of other unintentional injuries (UIJ)"

test_that(description, {

  # Update HED
  data <- update_hed(data = basepop,
                     hed_model1 = hed_model_list[[1]],
                     hed_model2 = hed_model_list[[2]],
                     hed_model3 = hed_model_list[[3]])

  # Execute the function
  result <- assign_rr(data = data, diseases = diseases, risk_param = risk_param)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true(all( c("RR_HLVDC", "RR_LVDC", "RR_AUD", "RR_IJ", "RR_DM",
                     "RR_IHD", "RR_ISTR", "RR_HYPHD", "RR_MVACC",
                     "RR_UIJ") %in% names(result) ))
})
