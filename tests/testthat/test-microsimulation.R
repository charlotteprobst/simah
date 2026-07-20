# Load testthat library
library(testthat)

options(microsim_verbosity = 0)

# 1. Load the package
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

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
  expected_outputs <- c("demographics", "alcoholcat", "alcoholcont", "hed", "hed_cat", "mortality")
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
  expected_outputs <- c("demographics", "alcoholcat", "alcoholcont", "hed", "hed_cat", "mortality")
  expect_true(all(expected_outputs %in% names(result)))
})
