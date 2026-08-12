# Load testthat library
library(testthat)

options(microsim_verbosity = 0)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))

# read in hed model
hed_model_list <- list(
  'youngmen' = xgboost::xgb.load(system.file("extdata", "xgbmodel_youngmen.json", package = "simah")),
  'else'     = xgboost::xgb.load(system.file("extdata", "xgbmodel_else.json", package = "simah")),
  'oldmen'   =  xgboost::xgb.load(system.file("extdata", "xgbmodel_oldmen.json", package = "simah"))
)

# 2. Define the test case
description <- "update_hed update heavy episodic drinking (HED) status"

test_that(description, {

  # Execute the function
  result <- update_hed(data = basepop,
                       hed_model1 = hed_model_list[[1]],
                       hed_model2 = hed_model_list[[2]],
                       hed_model3 = hed_model_list[[3]])

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the output column exists in result
  expect_true("hed_binary" %in% names(result))
})
