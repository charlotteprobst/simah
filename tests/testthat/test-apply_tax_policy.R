# Load testthat library
library(testthat)

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

test_that("apply_tax_policy returns correct structure", {
  scenario <- 0.1
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  result <- apply_tax_policy(
    basepop,
    scenario,
    cons_elasticity,
    cons_elasticity_se,
    r_sim_obs
  )

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that row count matches input (rows aren't added or removed)
  expect_equal(nrow(result), nrow(basepop))

  # Check that all expected columns are present
  expected_cols <- c("ID", "age", "race", "sex", "education",
                     "drinkingstatus", "alc_gpd", "formerdrinker",
                     "income", "BMI", "spawn_year", "agecat",
                     "education_detailed", "alc_cat")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("apply_tax_policy correctly handles non-drinkers (alc_gpd == 0)", {
  scenario <- 0.1
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  # Create test data with some non-drinkers
  test_data <- basepop %>%
    dplyr::filter(alc_gpd == 0) %>%
    dplyr::slice_head(n = 50)

  result <- apply_tax_policy(test_data, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)

  # Non-drinkers should remain non-drinkers (alc_gpd == 0)
  expect_equal(result$alc_gpd, rep(0, nrow(result)))
})

test_that("apply_tax_policy reduces alcohol consumption for drinkers", {
  scenario <- 0.1  # 10% price increase
  cons_elasticity <- -0.1  # Negative elasticity means consumption decreases
  cons_elasticity_se <- 0.01
  r_sim_obs <- 0.8

  # Get initial drinkers
  initial_drinkers <- basepop %>%
    dplyr::filter(alc_gpd > 0)

  initial_mean <- mean(initial_drinkers$alc_gpd)

  result <- apply_tax_policy(initial_drinkers, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)
  final_mean <- mean(result$alc_gpd[result$alc_gpd > 0])

  # With positive scenario (price increase) and negative elasticity,
  # consumption should decrease on average
  expect_true(final_mean < initial_mean)
})

test_that("apply_tax_policy with zero scenario has minimal effect", {
  scenario <- 0.0  # No price change
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  initial_data <- basepop %>%
    dplyr::filter(alc_gpd > 0)

  initial_mean <- mean(initial_data$alc_gpd)

  result <- apply_tax_policy(initial_data, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)
  final_mean <- mean(result$alc_gpd[result$alc_gpd > 0])

  # With zero scenario, there should be minimal change (only due to random variation)
  expect_true(abs(final_mean - initial_mean) < 1)
})

test_that("apply_tax_policy with negative scenario increases consumption", {
  scenario <- -0.1  # 10% price decrease (discount)
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  initial_data <- basepop %>%
    dplyr::filter(alc_gpd > 0)

  initial_mean <- mean(initial_data$alc_gpd)

  result <- apply_tax_policy(initial_data, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)
  final_mean <- mean(result$alc_gpd[result$alc_gpd > 0])

  # With negative scenario (discount) and negative elasticity,
  # consumption should increase on average
  expect_true(final_mean > initial_mean)
})

test_that("apply_tax_policy preserves all IDs in the data", {
  scenario <- 0.1
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  initial_ids <- sort(basepop$ID)
  result <- apply_tax_policy(basepop, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)
  result_ids <- sort(result$ID)

  # All IDs should be preserved
  expect_equal(result_ids, initial_ids)
})

test_that("apply_tax_policy only modifies drinkers (ID matching)", {
  scenario <- 0.1
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  # Get IDs of drinkers
  drinker_ids <- basepop %>%
    dplyr::filter(alc_gpd > 0) %>%
    dplyr::pull(ID)

  result <- apply_tax_policy(basepop, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)

  # Non-drinkers should still have alc_gpd == 0
  non_drinker_ids <- basepop %>%
    dplyr::filter(alc_gpd == 0) %>%
    dplyr::pull(ID)

  # Check that non-drinkers in result still have alc_gpd == 0
  non_drinker_result <- result %>%
    dplyr::filter(ID %in% non_drinker_ids)
  expect_equal(non_drinker_result$alc_gpd, rep(0, nrow(non_drinker_result)))
})

test_that("apply_tax_policy responds correctly to elasticity sign", {
  scenario <- 0.1  # 10% price increase

  # Test with negative elasticity (standard case)
  result_neg <- apply_tax_policy(basepop, scenario, -0.1, 0.01, 0.8)
  mean_neg <- mean(result_neg$alc_gpd[result_neg$alc_gpd > 0])

  # Test with positive elasticity (theoretical reverse case)
  result_pos <- apply_tax_policy(basepop, scenario, 0.1, 0.01, 0.8)
  mean_pos <- mean(result_pos$alc_gpd[result_pos$alc_gpd > 0])

  # With negative elasticity, price increase should reduce consumption
  # With positive elasticity, price increase should increase consumption
  expect_true(mean_neg < mean_pos)
})

test_that("apply_tax_policy handles different standard errors", {
  scenario <- 0.1
  cons_elasticity <- -0.1078

  # Test with low standard error (less variation)
  result_low_se <- apply_tax_policy(basepop, scenario, cons_elasticity, 0.01, 0.8)
  sd_low <- sd(result_low_se$alc_gpd[result_low_se$alc_gpd > 0])

  # Test with high standard error (more variation)
  result_high_se <- apply_tax_policy(basepop, scenario, cons_elasticity, 0.1, 0.8)
  sd_high <- sd(result_high_se$alc_gpd[result_high_se$alc_gpd > 0])

  # Higher standard error should lead to more variation in results
  expect_true(sd_high > sd_low)
})

test_that("apply_tax_policy handles different correlation values", {
  scenario <- 0.1
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442

  # Test with low correlation
  result_low_r <- apply_tax_policy(basepop, scenario, cons_elasticity, cons_elasticity_se, 0.1)

  # Test with high correlation
  result_high_r <- apply_tax_policy(basepop, scenario, cons_elasticity, cons_elasticity_se, 0.9)

  # Both should return data frames with same structure
  expect_s3_class(result_low_r, "data.frame")
  expect_s3_class(result_high_r, "data.frame")
  expect_equal(nrow(result_low_r), nrow(result_high_r))
})

test_that("apply_tax_policy handles extreme scenario values", {
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  # Test with very high price increase
  result_high <- apply_tax_policy(basepop, 0.5, cons_elasticity, cons_elasticity_se, r_sim_obs)
  expect_s3_class(result_high, "data.frame")
  expect_equal(nrow(result_high), nrow(basepop))

  # Test with very high price decrease
  result_low <- apply_tax_policy(basepop, -0.5, cons_elasticity, cons_elasticity_se, r_sim_obs)
  expect_s3_class(result_low, "data.frame")
  expect_equal(nrow(result_low), nrow(basepop))
})

test_that("apply_tax_policy is reproducible with same seed", {
  scenario <- 0.1
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  # Set seed before each run
  set.seed(123)
  result1 <- apply_tax_policy(basepop, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)

  set.seed(123)
  result2 <- apply_tax_policy(basepop, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)

  # Results should be identical with same seed
  expect_equal(result1$alc_gpd, result2$alc_gpd)
})

test_that("apply_tax_policy does not modify input data", {
  scenario <- 0.1
  cons_elasticity <- -0.1078
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  input_data <- basepop
  input_copy <- input_data
  input_alc_gpd <- input_data$alc_gpd

  result <- apply_tax_policy(input_data, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs)

  # Input data should be unchanged
  expect_identical(input_data, input_copy)

  # But result should have different alc_gpd values (due to tax policy)
  # Note: This is probabilistic, so we check that some changes occurred
  changed <- sum(result$alc_gpd != input_alc_gpd, na.rm = TRUE)
  expect_true(changed > 0)
})

test_that("apply_tax_policy works with various elasticity values", {
  scenario <- 0.1
  cons_elasticity_se <- 0.0442
  r_sim_obs <- 0.8

  # Test with different elasticity values
  elasticities <- c(-0.2, -0.1, -0.05, 0, 0.05)

  for (elast in elasticities) {
    result <- apply_tax_policy(basepop, scenario, elast, cons_elasticity_se, r_sim_obs)

    # Should return valid data frame
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(basepop))
  }
})
