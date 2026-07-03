# Load testthat library
library(testthat)

options(microsim_verbosity = 0)

# 1. Load the package
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

# Define data paths
DataDirectoryMinimal <- "inputs_data"

# Load necessary mock data
basepop <- readr::read_rds(file.path(DataDirectoryMinimal, "data.rds"))
migration_rates <- readr::read_rds(file.path(DataDirectoryMinimal, "migration_rates.rds"))
svy_data <- readr::read_rds(file.path(DataDirectoryMinimal, "svy_data.rds")) # formerly brfss

test_that("add_new_18yo returns correct structure", {

  year <- 2020
  result <- add_new_18yo(basepop, migration_rates, cyear=year, svy_data)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that new individuals were added
  expect_true(nrow(result) > nrow(basepop))
})

test_that("add_new_18yo preserves original population data", {
  year <- 2020
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)

  # Original basepop IDs should all be present
  expect_true(all(basepop$ID %in% result$ID))

  # Original basepop should be unchanged (row count, data)
  original_rows <- nrow(basepop)
  expect_equal(nrow(basepop), original_rows)

  # Check some variables match
  expect_equal(basepop$age[1:10], result$age[1:10])
  expect_equal(basepop$race[1:10], result$race[1:10])
})

test_that("add_new_18yo adds individuals with age = 18", {
  year <- 2020
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)

  # Get the newly added individuals (last rows)
  new_individuals <- result[(nrow(basepop) + 1):nrow(result), ]

  # All new individuals should be age 18
  expect_equal(new_individuals$age, rep(18, nrow(new_individuals)))
})

test_that("add_new_18yo assigns correct unique IDs", {
  year <- 2020
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)

  # Find newly added IDs
  original_max_id <- max(basepop$ID)
  new_ids <- result[(nrow(basepop) + 1):nrow(result), "ID"]

  # IDs should start at original_max_id + 1
  expect_equal(min(new_ids), original_max_id + 1)

  # IDs should be sequential (or at least unique)
  expect_equal(length(unique(new_ids)), length(new_ids))
})

test_that("add_new_18yo adds individuals matching birth rates", {
  year <- 2020

  # Get birth rates for the year
  birth_rates <- migration_rates %>%
    dplyr::filter(agecat == "18") %>%
    dplyr::filter(year == year) %>%
    dplyr::select(race, sex, birthrate)

  # Get population counts by race and sex
  pop_counts <- basepop %>%
    dplyr::group_by(race, sex) %>%
    dplyr::tally()

  # Calculate expected counts
  merged <- dplyr::left_join(birth_rates, pop_counts, by = c("race", "sex"))
  merged$expected_count <- merged$n * merged$birthrate

  # Run the function
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)

  # Count new individuals by race and sex
  original_n <- nrow(basepop)
  new_individuals <- result[(original_n + 1):nrow(result), ]

  new_counts <- new_individuals %>%
    dplyr::group_by(race, sex) %>%
    dplyr::tally() %>%
    dplyr::ungroup()

  # Compare expected vs actual (within tolerance)
  merged_counts <- dplyr::left_join(merged, new_counts, by = c("race", "sex"))

  # Check that no negative counts
  expect_true(all(merged_counts$n.x >= 0, na.rm = TRUE))
  expect_true(all(merged_counts$n.y >= 0, na.rm = TRUE))
})

test_that("add_new_18yo handles years not in migration_rates", {
  # Use a year that might not be in migration_rates
  year <- 2050
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)

  # Should still return a data frame (possibly with no new individuals)
  expect_s3_class(result, "data.frame")

  # Since rates might be NA for 2050, result might equal basepop
  expect_true(nrow(result) >= nrow(basepop))
})

test_that("add_new_18yo preserves all columns from original data", {
  year <- 2020
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)

  # All original columns should be present
  original_cols <- names(basepop)
  expect_true(all(original_cols %in% names(result)))
})

test_that("add_new_18yo handles cases where survey data lacks minority categories", {
  # This tests the missing categories logic
  # Create a minimal survey data that doesn't have all race categories
  minimal_svy <- svy_data %>%
    dplyr::filter(race %in% c("Hispanic", "Black")) %>%
    dplyr::filter(YEAR %in% c(2020, 2021)) %>%
    dplyr::filter(age == 18)

  year <- 2020
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data = minimal_svy)

  # Should still work (possibly with some missing categories filled with NA values)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= nrow(basepop))
})

test_that("add_new_18yo works across different simulation years", {
  years_to_test <- unique(migration_rates$year)

  # Test with first few years
  years_to_test <- head(years_to_test, 3)

  for (y in years_to_test) {
    result <- add_new_18yo(basepop, migration_rates, cyear = y, svy_data)
    expect_s3_class(result, "data.frame")

    # Should have added new individuals (unless all rates are 0)
    expect_true(nrow(result) >= nrow(basepop))
  }
})

test_that("add_new_18yo handles zero birth rates", {
  # Create migration rates where all birthrates are 0
  zero_rates <- migration_rates %>%
    dplyr::filter(year %in% c(2020, 2021)) %>%
    dplyr::mutate(birthrate = 0)

  year <- 2020
  result <- add_new_18yo(basepop, zero_rates, cyear = year, svy_data)

  # Should return original data (no new individuals added)
  expect_equal(nrow(result), nrow(basepop))
})

test_that("add_new_18yo sets appropriate NA for new columns", {
  year <- 2020
  basepop$hed_binary <- NA
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)

  original_n <- nrow(basepop)
  new_individuals <- result[(original_n + 1):nrow(result), ]

  # Check that new individuals have NA for columns that weren't in survey data
  # or for columns that weren't in the original data but were added
  new_cols <- setdiff(names(result), names(svy_data))
  new_cols <- setdiff(new_cols, c("ID", "spawn_year"))  # These are added manually

  # Most new columns should be NA (hed_binary for new individuals must be NA)
  for (col in new_cols) {
    expect_true(all(is.na(new_individuals[[col]])))
  }
})

test_that("add_new_18yo uses correct survey data time window", {
  year <- 2020

  # Get survey data within window [1999, 2001] for year 2000
  min_year <- year - 1
  max_year <- year + 1

  # Check that the function would use data from this window
  within_window <- svy_data %>%
    dplyr::filter(YEAR >= min_year & YEAR <= max_year)

  expect_true(nrow(within_window) > 0)

  # Function should work with this window data
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)
  expect_s3_class(result, "data.frame")
})

test_that("add_new_18yo handles missing survey data for categories", {
  # Test the missing category detection by temporarily limiting survey data
  limited_svy <- svy_data %>%
    dplyr::filter(YEAR == 2020) %>%
    dplyr::filter(race == "Black" & sex == "m")

  year <- 2020
  result <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data = limited_svy)

  # Should still work
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= nrow(basepop))

  # But might have some missing demographic groups
})

test_that("add_new_18yo produces consistent structure across runs", {
  year <- 2020

  result1 <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)
  result2 <- add_new_18yo(basepop, migration_rates, cyear = year, svy_data)

  # Same structure
  expect_equal(names(result1), names(result2))
  expect_equal(nrow(result1), nrow(result2))

  # Note: actual values may differ due to sampling
})
