# Load testthat library
library(testthat)

options(microsim_verbosity = 0)

# defaults
diseases <- c("AUD", "DM", "HLVDC", "HYPHD", "IHD", "IJ", "ISTR", "LVDC", "MVACC", "UIJ")
inflation_factors <- c(28, 3)
age_inflated <- list(c("18-24","25-34","35-44","45-54","55-64"), c("65-74", "75-79"))
output <- c("demographics", "alcoholcat", "alcoholcont", "hed", "mortality")
strata <- list(
  alcoholcat  = c("sex", "agecat", "education", "race"),
  alcoholcont = c("sex", "agecat", "education", "race"),
  demographics = c("sex", "agecat", "education", "race"),
  mortality = c("sex", "agecat", "education", "race")
)

test_that("microsimulation runs with default parameters", {

  result <- microsimulation()

  # Check that result is a list
  expect_type(result, "list")

  # Check that all requested output types are present
  expected_outputs <- c("demographics", "alcoholcat", "alcoholcont", "hed", "mortality")
  expect_true(all(expected_outputs %in% names(result)))
})

test_that("microsimulation runs without errors", {

  result <- microsimulation(
      maxyear = 2000,
      diseases = diseases,
      inflation_factors = inflation_factors,
      age_inflated = age_inflated,
      COVID_specific_tps = 1,
      updatingalcohol = TRUE,
      counterfactual = 0,
      policy = "none",  # allowed values are none and basic
      output = output,
      strata = strata,
      seed = 1, nunc = 1, microsim_verbosity = 0
  )

  # Check that result is a list
  expect_type(result, "list")

  # Check that all requested output types are present
  expected_outputs <- c("demographics", "alcoholcat", "alcoholcont", "hed", "mortality")
  expect_true(all(expected_outputs %in% names(result)))
})

test_that("microsimulation with counterfactual = 0 runs normally", {

  result <- microsimulation(
    maxyear = 2000,
    diseases = diseases,
    inflation_factors = inflation_factors,
    age_inflated = age_inflated,
    COVID_specific_tps = 1,
    updatingalcohol = TRUE,
    counterfactual = 0,
    policy = "none",  # allowed values are none and basic
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
    maxyear = 2000,
    diseases = diseases,
    inflation_factors = inflation_factors,
    age_inflated = age_inflated,
    COVID_specific_tps = 1,
    updatingalcohol = FALSE,
    counterfactual = 1,
    policy = "none",  # allowed values are none and basic
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

test_that("microsimulation with policy tax", {

  result <- microsimulation(
    maxyear = 2000,
    diseases = diseases,
    inflation_factors = inflation_factors,
    age_inflated = age_inflated,
    COVID_specific_tps = 1,
    updatingalcohol = TRUE,
    counterfactual = 0,
    policy = "basic",  # allowed values are none and basic
    year_policy = 2000,
    cons_elasticity = -0.1078,
    cons_elasticity_se = 0.0442,
    r_sim_obs = 0.8,
    output = output,
    strata = strata,
    seed = 1, nunc = 1, microsim_verbosity = 0
  )

  # Check that result is a list
  expect_type(result, "list")

  # Check that all requested output types are present
  expected_outputs <- c("demographics", "alcoholcat", "alcoholcont", "hed", "mortality")
  expect_true(all(expected_outputs %in% names(result)))
})

test_that("microsimulation pass data as input", {

  # 1. Locate the config file: allow user override, default to package internal
  config_path <- system.file("config", "default.yaml", package = "simah")

  if (config_path == "" || !file.exists(config_path)) {
    stop("Configuration file not found.")
  }

  input_config <- yaml::read_yaml(config_path)

  # 2. Define a helper to resolve paths
  get_data_path <- function(filename) {
    custom_dir <- input_config[["data_dir"]]

    # If custom_dir is set and file exists, use it
    if (!is.null(custom_dir) && nchar(custom_dir) > 0 && file.exists(file.path(custom_dir, filename))) {
      return(file.path(custom_dir, filename))
    }

    # Fallback to internal package data
    return(system.file("extdata", filename, package = "simah"))
  }

  # 3. Load the data using the helper
  basedata_config <- input_config[["basedata"]]

  basepop <- readr::read_rds(get_data_path(basedata_config[["data"]]))
  svy_data <- readr::read_rds(get_data_path(basedata_config[["svy_data"]]))
  mort_data <- readr::read_rds(get_data_path(basedata_config[["mort_data"]]))
  base_rates <- readr::read_rds(get_data_path(basedata_config[["base_rates"]]))
  risk_param <- readr::read_rds(get_data_path(basedata_config[["risk_param"]]))
  education_transitions <- readr::read_rds(get_data_path(basedata_config[["education_transitions"]]))
  education_transitions_covid <- readr::read_rds(get_data_path(basedata_config[["education_transitions_covid"]]))
  alcohol_transitions <- readr::read_rds(get_data_path(basedata_config[["alcohol_transitions"]]))
  catcontmodel <- readr::read_rds(get_data_path(basedata_config[["catcontmodel"]]))
  migration_rates <- readr::read_rds(get_data_path(basedata_config[["migration_rates"]]))

  # HED data
  heddata <- input_config[["hevdata"]]
  hed_model_list <- list(
    'youngmen' = xgboost::xgb.load(get_data_path(heddata[["youngmen"]])),
    'else'     = xgboost::xgb.load(get_data_path(heddata[["else"]])),
    'oldmen'   = xgboost::xgb.load(get_data_path(heddata[["oldmen"]]))
  )

  datalist <- list(data = basepop, svy_data = svy_data, mort_data = mort_data, base_rates = base_rates, risk_param = risk_param,
                  education_transitions = education_transitions, education_transitions_covid = education_transitions_covid,
                  alcohol_transitions = alcohol_transitions, catcontmodel = catcontmodel, migration_rates = migration_rates,
                  hed_model_list = hed_model_list)

  result <- microsimulation(datalist = datalist)

  # Check that all requested output types are present
  expected_outputs <- c("demographics", "alcoholcat", "alcoholcont", "hed", "mortality")
  expect_true(all(expected_outputs %in% names(result)))
})
