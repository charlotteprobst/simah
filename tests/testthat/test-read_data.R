# Load testthat library
library(testthat)

options(microsim_verbosity = 0)

test_that("read_data returns complete data structure", {
  data_list <- read_data()

  # Check that result is a list
  expect_type(data_list, "list")

  # Check that all expected elements exist
  expected_elements <- c("data", "svy_data", "mort_data", "base_rates", "risk_param",
                         "education_transitions", "education_transitions_covid",
                         "alcohol_transitions", "catcontmodel", "migration_rates",
                         "hed_model_list")
  expect_true(all(expected_elements %in% names(data_list)))

  # Check that no extra unexpected elements exist
  expect_equal(length(data_list), length(expected_elements))
})

test_that("read_data check that data has been read", {

  data_list <- read_data()

  data <- data_list[["data"]]
  svy_data <- data_list[["svy_data"]]
  mort_data <- data_list[["mort_data"]]
  base_rates <- data_list[["base_rates"]]
  risk_param <- data_list[["risk_param"]]
  education_transitions <- data_list[["education_transitions"]]
  education_transitions_covid <- data_list[["education_transitions_covid"]]
  alcohol_transitions <- data_list[["alcohol_transitions"]]
  catcontmodel <- data_list[["catcontmodel"]]
  migration_rates <- data_list[["migration_rates"]]
  hed_model_list <- data_list[["hed_model_list"]]

  expect_type(data_list, "list")
})

test_that("read_data returns correct data frames", {
  data_list <- read_data()

  # Check data components are data frames
  expect_s3_class(data_list[["data"]], "data.frame")
  expect_s3_class(data_list[["svy_data"]], "data.frame")
  expect_s3_class(data_list[["mort_data"]], "data.frame")
  expect_s3_class(data_list[["base_rates"]], "data.frame")
  expect_s3_class(data_list[["risk_param"]], "data.frame")
  expect_s3_class(data_list[["education_transitions"]], "data.frame")
  expect_s3_class(data_list[["education_transitions_covid"]], "data.frame")
  expect_s3_class(data_list[["alcohol_transitions"]], "data.frame")
  expect_s3_class(data_list[["catcontmodel"]], "data.frame")
  expect_s3_class(data_list[["migration_rates"]], "data.frame")
})

test_that("read_data returns HED model list with correct models", {
  data_list <- read_data()

  hed_model_list <- data_list[["hed_model_list"]]

  # Check that hed_model_list is a list
  expect_type(hed_model_list, "list")

  # Check that all expected models exist
  expected_models <- c("youngmen", "else", "oldmen")
  expect_true(all(expected_models %in% names(hed_model_list)))
})

test_that("read_data basepop has expected columns", {
  data_list <- read_data()
  data <- data_list[["data"]]

  # Check that required columns exist
  expected_columns <- c("ID", "age", "sex", "race", "education",
                        "education_detailed", "drinkingstatus",
                        "alc_cat", "alc_gpd", "formerdrinker",
                        "income", "BMI", "spawn_year", "agecat")
  expect_true(all(expected_columns %in% names(data)))

  # Check that column names match exactly (no extra columns)
  expect_equal(sort(names(data)), sort(expected_columns))
})

test_that("read_data returns correct data types for columns", {
  data_list <- read_data()
  data <- data_list[["data"]]

  # ID should be numeric
  expect_type(data$ID, "integer")

  # age should be numeric
  expect_type(data$age, "double")

  # sex should be character
  expect_type(data$sex, "character")

  # race should be character
  expect_type(data$race, "character")

  # education should be character
  expect_type(data$education, "character")

  # education_detailed should be character
  expect_type(data$education_detailed, "character")

  # drinkingstatus should be character
  expect_type(data$drinkingstatus, "double")

  # alc_cat should be character
  expect_type(data$alc_cat, "character")

  # alc_gpd should be numeric
  expect_type(data$alc_gpd, "double")

  # formerdrinker should be logical
  expect_type(data$formerdrinker, "double")  # Treated as numeric (0/1) in R
})

test_that("read_data svy_data has expected columns", {
  data_list <- read_data()
  svy_data <- data_list[["svy_data"]]

  # Check that required columns exist
  expected_columns <- c("YEAR", "age", "sex", "race", "education",
                        "education_detailed", "drinkingstatus",
                        "alc_cat", "alc_gpd", "formerdrinker")
  expect_true(all(expected_columns %in% names(svy_data)))
})

test_that("read_data mort_data has expected columns", {
  data_list <- read_data()
  mort_data <- data_list[["mort_data"]]

  # Check that required columns exist
  expect_true("year" %in% names(mort_data))
  expect_true("cat" %in% names(mort_data))

  # Check that at least one cause of mortality exists
  cause_columns <- grep("LVDCmort", names(mort_data), value = TRUE)
  expect_true(length(cause_columns) > 0)
})

test_that("read_data base_rates has expected columns", {
  data_list <- read_data()
  base_rates <- data_list[["base_rates"]]

  # Check that required columns exist
  expect_true("year" %in% names(base_rates))
  expect_true("cat" %in% names(base_rates))

  # Check that at least one rate column exists
  rate_columns <- grep("rate_DM", names(base_rates), value = TRUE)
  expect_true(length(rate_columns) > 0)
})

test_that("read_data risk_param has expected structure", {
  data_list <- read_data()
  risk_param <- data_list[["risk_param"]]

  # Check that risk_param is a data frame
  expect_s3_class(risk_param, "data.frame")

  # It should have columns for risk function parameters
  expect_gt(ncol(risk_param), 0)
  expect_gt(nrow(risk_param), 0)
})

test_that("read_data education_transitions has expected columns", {
  data_list <- read_data()
  education_transitions <- data_list[["education_transitions"]]

  # Check that required columns exist
  expect_true("cat" %in% names(education_transitions))
  expect_true("StateTo" %in% names(education_transitions))
  expect_true("cumsum" %in% names(education_transitions))
})

test_that("read_data education_transitions_covid has expected columns", {
  data_list <- read_data()
  education_transitions_covid <- data_list[["education_transitions_covid"]]

  # Check that required columns exist
  expect_true("cat" %in% names(education_transitions_covid))
  expect_true("StateTo" %in% names(education_transitions_covid))
  expect_true("cumsum" %in% names(education_transitions_covid))
})

test_that("read_data alcohol_transitions has expected structure", {
  data_list <- read_data()
  alcohol_transitions <- data_list[["alcohol_transitions"]]

  # Check that it's a data frame with coefficients
  expect_s3_class(alcohol_transitions, "data.frame")

  # Should have name and Value columns (from the regression model)
  expect_true("name" %in% names(alcohol_transitions))
  expect_true("Value" %in% names(alcohol_transitions))
})

test_that("read_data catcontmodel has expected structure", {
  data_list <- read_data()
  catcontmodel <- data_list[["catcontmodel"]]

  # Check that it's a data frame
  expect_s3_class(catcontmodel, "data.frame")

  # Should have columns for beta distribution parameters
  expect_gt(ncol(catcontmodel), 0)
  expect_gt(nrow(catcontmodel), 0)
})

test_that("read_data migration_rates has expected columns", {
  data_list <- read_data()
  migration_rates <- data_list[["migration_rates"]]

  # Check that required columns exist
  expect_true("agecat" %in% names(migration_rates))
  expect_true("race" %in% names(migration_rates))
  expect_true("sex" %in% names(migration_rates))
  expect_true("year" %in% names(migration_rates))
  expect_true("birthrate" %in% names(migration_rates))
  expect_true("migrationinrate" %in% names(migration_rates))
  expect_true("migrationoutrate" %in% names(migration_rates))
})

test_that("read_data hed_model_list has expected model structure", {
  data_list <- read_data()
  hed_model_list <- data_list[["hed_model_list"]]

  # Check that each model exists and is a list with xgb parameters
  for (model_name in names(hed_model_list)) {
    model <- hed_model_list[[model_name]]

    # Check that the model inherits from xgb.Booster
    expect_true(inherits(model, "xgb.Booster"))
  }
})
