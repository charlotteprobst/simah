# Load testthat library
library(testthat)
library(xgboost)

options(microsim_verbosity = 0)

# 1. Load the package
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

# Define data paths
DataDirectoryMinimal <- "inputs_data"

# Find the project root directory from inside the test folder
project_root <- rprojroot::find_package_root_file()

# Load necessary mock data
basepop <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "data.rds"))}) # size: 500,000
svy_data <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "svy_data.rds"))}) # formerly brfss
mort_data <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "mort_data.rds"))}) # formerly death_counts; already processed
base_rates <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "base_rates.rds"))})
risk_param <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "risk_param.rds"))})
education_transitions <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "education_transitions.rds"))})
education_transitions_covid <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "education_transitions_covid.rds"))})
alcohol_transitions <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "alcohol_transitions.rds"))})
catcontmodel <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "catcontmodel.rds"))})
migration_rates <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "migration_rates.rds"))})

hed_model_list <- list(
  'youngmen' = withr::with_dir(project_root, {xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_youngmen.json"))}),
  'else'     = withr::with_dir(project_root, {xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_else.json"))}),
  'oldmen'   = withr::with_dir(project_root, {xgb.load(file.path(DataDirectoryMinimal, "xgbmodel_oldmen.json"))})
)

# defaults
diseases <- c("AUD", "DM", "HLVDC", "HYPHD", "IHD", "IJ", "ISTR", "LVDC", "MVACC", "UIJ")
inflation_factors <- c(28, 3)
age_inflated <- list(c("18-24","25-34","35-44","45-54","55-64"), c("65-74", "75-79"))
output <- c("demographics", "alcoholcat", "alcoholcont", "hed", "hed_cat", "mortality")
strata <- list(
  alcoholcat  = c("sex", "agecat", "education", "race"),
  alcoholcont = c("sex", "agecat", "education", "race"),
  demographics = c("sex", "agecat", "education", "race"),
  mortality = c("sex", "agecat", "education", "race")
)

test_that("microsimulation runs without errors", {

  result <- microsimulation(
      data = basepop,
      svy_data = svy_data,
      maxyear = 2000,
      mort_data = mort_data,
      base_rates = base_rates,
      diseases = diseases,
      risk_param = risk_param,
      inflation_factors = inflation_factors,
      age_inflated = age_inflated,
      education_transitions = education_transitions,
      education_transitions_covid = education_transitions_covid,
      COVID_specific_tps = 1,
      updatingalcohol = TRUE,
      alcohol_transitions = alcohol_transitions,
      catcontmodel = catcontmodel,
      hed_model_list = hed_model_list,
      counterfactual = 0,
      migration_rates = migration_rates,
      output = output,
      strata = strata,
      seed = 1, nunc = 1, microsim_verbosity = 0
  )

  # Check that result is a list
  expect_type(result, "list")

  # Check that all requested output types are present
  expected_outputs <- c("demographics", "alcoholcat", "alcoholcont", "hed", "hed_cat", "mortality")
  expect_true(all(expected_outputs %in% names(result)))
})

test_that("microsimulation with counterfactual = 0 runs normally", {

  result <- microsimulation(
    data = basepop,
    svy_data = svy_data,
    maxyear = 2000,
    mort_data = mort_data,
    base_rates = base_rates,
    diseases = diseases,
    risk_param = risk_param,
    inflation_factors = inflation_factors,
    age_inflated = age_inflated,
    education_transitions = education_transitions,
    education_transitions_covid = education_transitions_covid,
    COVID_specific_tps = 1,
    updatingalcohol = TRUE,
    alcohol_transitions = alcohol_transitions,
    catcontmodel = catcontmodel,
    hed_model_list = hed_model_list,
    counterfactual = 0,
    migration_rates = migration_rates,
    output = output,
    strata = strata,
    seed = 1,
    nunc = 1,
    microsim_verbosity = 0
  )

  # Check that alcohol values are not all zeros (normal scenario)
  alcohol_table <- result$alcoholcont
  expect_true(any(alcohol_table[alcohol_table$year == 2000, ]$meansimulation > 0))
})

test_that("microsimulation with counterfactual = 1 sets all alc_gpd to zero", {

  result <- microsimulation(
    data = basepop,
    svy_data = svy_data,
    maxyear = 2000,
    mort_data = mort_data,
    base_rates = base_rates,
    diseases = diseases,
    risk_param = risk_param,
    inflation_factors = inflation_factors,
    age_inflated = age_inflated,
    education_transitions = education_transitions,
    education_transitions_covid = education_transitions_covid,
    COVID_specific_tps = 1,
    updatingalcohol = FALSE,
    alcohol_transitions = alcohol_transitions,
    catcontmodel = catcontmodel,
    hed_model_list = hed_model_list,
    counterfactual = 1,
    migration_rates = migration_rates,
    output = c("alcoholcont", "hed"),
    strata = strata,
    seed = 1,
    nunc = 1,
    microsim_verbosity = 0
  )

  # All alcohol values should be zero
  alcohol_table <- result$alcoholcont
  hed_table <- result$hed
  expect_true(all(alcohol_table[alcohol_table$year == 2000, ]$meansimulation == 0))
  expect_true(all(hed_table[hed_table$year == 2000, ]$n_hed == 0))
})
