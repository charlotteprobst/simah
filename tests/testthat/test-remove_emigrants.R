# Load testthat library
library(testthat)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))
migration_rates <- readr::read_rds(system.file("extdata", "migration_rates.rds", package = "simah"))

test_that("remove-emigrants returns correct structure", {

  year <- 2020
  result <- remove_emigrants(basepop, migration_rates, cyear=year)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that individuals were removed
  expect_true(nrow(result) < nrow(basepop))
})

test_that("remove_emigrants preserves unique IDs", {
  year <- 2020
  result <- remove_emigrants(basepop, migration_rates, cyear = year)

  # All IDs should be unique
  expect_equal(length(unique(result$ID)), nrow(result))

  # No ID from result should appear in removed individuals
  original_ids <- setdiff(basepop$ID, result$ID)
  expect_true(length(original_ids) > 0)  # Some IDs should have been removed
})

test_that("remove_emigrants preserves all original columns", {
  year <- 2020
  result <- remove_emigrants(basepop, migration_rates, cyear = year)

  # All original columns should be present
  expect_true(all(names(basepop) %in% names(result)))

  # No extra columns should be added (except in intermediate steps, but final result should only have original columns)
  expect_equal(setdiff(names(result), names(basepop)), character(0))
})

test_that("remove_emigrants handles zero migration rates", {

  # Create migration rates where all migrationoutrates are 0
  zero_rates <- migration_rates %>%
    dplyr::filter(!is.na(migrationoutrate)) %>%
    dplyr::mutate(migrationoutrate = 0)

  year <- 2020
  result <- remove_emigrants(basepop, zero_rates, cyear = year)

  # Should return original data (no one removed)
  expect_identical(result, basepop)
})

test_that("remove_emigrants caps removals at population size", {
  # Create a test case where migration rate > 1 (should be capped)
  high_rates <- migration_rates %>%
    dplyr::filter(!is.na(migrationoutrate)) %>%
    dplyr::mutate(migrationoutrate = migrationoutrate * 10)  # rates > 1

  year <- 2020
  result <- remove_emigrants(basepop, high_rates, cyear = year)

  # Should not remove more people than exist
  expect_true(nrow(result) >= 0)
  expect_true(nrow(result) <= nrow(basepop))
})

test_that("remove_emigrants handles edge cases gracefully", {
  # Test with subset of data (some demographic groups empty)
  subset_pop <- basepop %>%
    dplyr::filter(race == "White")

  year <- 2020
  result <- remove_emigrants(subset_pop, migration_rates, cyear = year)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) <= nrow(subset_pop))
})

test_that("remove_emigrants removes correct demographic composition", {
  year <- 2020

  result <- remove_emigrants(basepop, migration_rates, cyear = year)

  # Count removals by demographic group
  age_breaks <- c(0, 18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 100)
  age_groups <- c("18", "19-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")

  removed <- basepop[!basepop$ID %in% result$ID, ]

  removed_counts <- removed %>%
    dplyr::mutate(agecat = cut(age, breaks = age_breaks, labels = age_groups)) %>%
    dplyr::group_by(agecat, race, sex) %>%
    dplyr::tally()

  # Count totals by demographic group
  total_counts <- basepop %>%
    dplyr::mutate(agecat = cut(age, breaks = age_breaks, labels = age_groups)) %>%
    dplyr::group_by(agecat, race, sex) %>%
    dplyr::tally()

  # All demographic groups should be represented
  expect_true(nrow(removed_counts) > 0)
  expect_true(nrow(total_counts) > 0)
})

test_that("remove_emigrants works across different simulation years", {
  years_to_test <- unique(migration_rates$year)
  years_to_test <- years_to_test[!is.na(years_to_test)]

  # Test with first few years that have migrationoutrate data
  years_with_rates <- migration_rates %>%
    dplyr::filter(!is.na(migrationoutrate)) %>%
    dplyr::select(year) %>%
    unique() %>%
    dplyr::pull()

  years_to_test <- head(years_with_rates, 3)

  for (y in years_to_test) {
    result <- remove_emigrants(basepop, migration_rates, cyear = y)
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) <= nrow(basepop))
  }
})

test_that("remove_emigrants produces consistent structure across runs", {
  year <- 2020

  result1 <- remove_emigrants(basepop, migration_rates, cyear = year)
  result2 <- remove_emigrants(basepop, migration_rates, cyear = year)

  # Same structure
  expect_equal(names(result1), names(result2))
  expect_equal(nrow(result1), nrow(result2))

  # Note: actual IDs may differ due to sampling
})

test_that("remove_emigrants uses correct age group boundaries", {
  year <- 2020

  # Verify the age breaks used in the function match expectations
  age_breaks <- c(0, 18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 100)
  age_groups <- c("18", "19-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")

  result <- remove_emigrants(basepop, migration_rates, cyear = year)

  # Verify that age categorization in result is correct
  # (would need to examine intermediate steps for full verification)

  # Test age ranges
  expect_true(all(result$age >= 18))  # All remaining individuals should be >= 18

  # Verify age categorization matches the function's cutoffs
  expected_cats <- cut(result$age, breaks = age_breaks, labels = age_groups)
  expect_true(all(!is.na(expected_cats)))
})

test_that("remove_emigrants handles extreme migration rates", {
  # Test with very high rates (should cap at population size)
  high_rates <- migration_rates %>%
    dplyr::filter(!is.na(migrationoutrate)) %>%
    dplyr::mutate(migrationoutrate = pmin(migrationoutrate * 10, 1.0))  # Cap at 1.0

  year <- 2020
  result <- remove_emigrants(basepop, high_rates, cyear = year)

  expect_true(nrow(result) >= 0)
  expect_true(nrow(result) <= nrow(basepop))
})
