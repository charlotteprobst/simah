# Load testthat library
library(testthat)
library(xgboost)

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
risk_param <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "risk_param.rds"))})
# read in hed model
hed_model_list <- list(
  'youngmen' = withr::with_dir(project_root, {xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_youngmen.json"))}),
  'else'     = withr::with_dir(project_root, {xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_else.json"))}),
  'oldmen'   = withr::with_dir(project_root, {xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_oldmen.json"))})
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
