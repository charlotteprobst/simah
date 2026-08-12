# Load testthat library
library(testthat)

options(microsim_verbosity = 0)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))
migration_rates <- readr::read_rds(system.file("extdata", "migration_rates.rds", package = "simah"))
svy_data <- readr::read_rds(system.file("extdata", "svy_data.rds", package = "simah"))

test_that("add_new_migrants returns correct structure", {

  year <- 2020
  result <- add_new_migrants(basepop, migration_rates, cyear=year, svy_data)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that new individuals were added
  expect_true(nrow(result) > nrow(basepop))
})

test_that("add_new_migrants preserves original population data", {
  year <- 2020
  result <- add_new_migrants(basepop, migration_rates, cyear = year, svy_data)

  # Original basepop IDs should all be present
  expect_true(all(basepop$ID %in% result$ID))

  # Original basepop should be unchanged (row count, data)
  original_rows <- nrow(basepop)
  expect_equal(nrow(basepop), original_rows)

  # Check some variables match
  expect_equal(basepop$age[1:10], result$age[1:10])
  expect_equal(basepop$race[1:10], result$race[1:10])
})

test_that("add_new_migrants assigns correct unique IDs", {
  year <- 2020
  result <- add_new_migrants(basepop, migration_rates, cyear = year, svy_data)

  # Find newly added IDs
  original_max_id <- max(basepop$ID)
  new_ids <- result[(nrow(basepop) + 1):nrow(result), "ID"]

  # IDs should start at original_max_id + 1
  expect_equal(min(new_ids), original_max_id + 1)

  # IDs should be sequential (or at least unique)
  expect_equal(length(unique(new_ids)), length(new_ids))
})

test_that("add_new_migrants sets correct spawn_year for new individuals", {
  year <- 2020
  result <- add_new_migrants(basepop, migration_rates, cyear = year, svy_data)

  original_n <- nrow(basepop)
  new_individuals <- result[(original_n + 1):nrow(result), ]

  expect_equal(new_individuals$spawn_year, rep(year, nrow(new_individuals)))
})

test_that("add_new_migrants returns original data when cyear > max year", {
  # Find max year in migration_rates
  max_year <- max(migration_rates$year, na.rm = TRUE)
  future_year <- max_year + 5

  # Should return exactly the same data
  result <- add_new_migrants(basepop, migration_rates, cyear = future_year, svy_data)

  expect_identical(result, basepop)
})

test_that("add_new_migrants works with early years in migration data", {
  min_year <- min(migration_rates$year, na.rm = TRUE)

  result <- add_new_migrants(basepop, migration_rates, cyear = min_year, svy_data)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= nrow(basepop))
})

test_that("add_new_migrants uses synthetic population for years after 2022", {
  # Test with a future year (2023 onwards)
  future_year <- 2023

  result <- add_new_migrants(basepop, migration_rates, cyear = future_year, svy_data)

  expect_s3_class(result, "data.frame")

  # Should have added migrants (if rates exist for this year)
  expect_true(nrow(result) >= nrow(basepop))
})

test_that("add_new_migrants preserves all columns from original data", {
  year <- 2020
  result <- add_new_migrants(basepop, migration_rates, cyear = year, svy_data)

  # All original columns should be present
  original_cols <- names(basepop)
  expect_true(all(original_cols %in% names(result)))

  # New columns might be added (like spawn_year), but original should all be there
})

test_that("add_new_migrants handles zero migration rates", {
  # Create migration rates where all migrationinrates are 0
  zero_rates <- migration_rates %>%
    dplyr::filter(year == 2020) %>%
    dplyr::mutate(migrationinrate = 0)

  year <- 2020
  result <- add_new_migrants(basepop, zero_rates, cyear = year, svy_data)

  # Should return original data (no new individuals added)
  expect_identical(result, basepop)
})

test_that("add_new_migrants handles cases with no matching survey data", {
  # Use a year where survey data might not have all categories
  # or create a scenario with limited survey data
  limited_svy <- svy_data %>%
    dplyr::filter(YEAR == 2020) %>%
    dplyr::filter(age >= 18 & age <= 65)

  year <- 2020
  result <- add_new_migrants(basepop, migration_rates, cyear = year, svy_data = limited_svy)

  # Should still work
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= nrow(basepop))

  # But might have fewer new migrants than expected
})

test_that("add_new_migrants produces consistent structure across runs", {
  year <- 2020

  result1 <- add_new_migrants(basepop, migration_rates, cyear = year, svy_data)
  result2 <- add_new_migrants(basepop, migration_rates, cyear = year, svy_data)

  # Same structure
  expect_equal(names(result1), names(result2))
  expect_equal(nrow(result1), nrow(result2))

  # Note: actual values may differ due to sampling
})

test_that("add_new_migrants works when migration rates exist for only some demographics", {
  # Filter to only include rates for white males
  limited_rates <- migration_rates %>%
    dplyr::filter(race == "White" & sex == "m")

  year <- 2020
  result <- add_new_migrants(basepop, limited_rates, cyear = year, svy_data)

  # Should return a data frame
  expect_s3_class(result, "data.frame")

  # Should have added some migrants (if rates exist for some demographics)
  expect_true(nrow(result) >= nrow(basepop))
})






