# Load testthat library
library(testthat)
library(xgboost)

# 1. Load the package and data (similar to minimal_test_setup.R)
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

# Define data paths
DataDirectoryMinimal <- "inputs_data"

# Load necessary mock data
basepop <- readr::read_rds(file.path(DataDirectoryMinimal, "data.rds"))
# read in hed model
hed_model_list <- list(
  'youngmen' = xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_youngmen.json")),
  'else'     = xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_else.json")),
  'oldmen'   = xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_oldmen.json"))
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
