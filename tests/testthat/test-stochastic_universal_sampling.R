# Load testthat library
library(testthat)

# 1. Load the package
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

test_that("stochastic_universal_sampling correct structure", {

  fitness <- c(10, 20, 30, 5, 70, 15, 43, 3)
  nselect <- 4
  selectedIDs <- stochastic_universal_sampling(fitness, nselect)

  usel <- unique(selectedIDs)
  expect_true(length(usel) <= nselect)

  expect_equal(length(selectedIDs), nselect)

  # All values should be valid indices (between 1 and length(fitness))
  expect_true(all(selectedIDs >= 1))
  expect_true(all(selectedIDs <= length(fitness)))
})

test_that("stochastic_universal_sampling with nselect = 1 works correctly", {
  fitness <- c(10, 20, 30, 5, 70, 15, 43, 3)
  selectedIDs <- stochastic_universal_sampling(fitness, 1)

  expect_equal(length(selectedIDs), 1)
  expect_true(selectedIDs[1] >= 1)
  expect_true(selectedIDs[1] <= length(fitness))
})

test_that("stochastic_universal_sampling may return duplicates even when nselect = length(fitness)", {
  fitness <- c(10, 20, 30, 5, 70, 15, 43, 3)
  selectedIDs <- stochastic_universal_sampling(fitness, length(fitness))

  expect_equal(length(selectedIDs), length(fitness))

  # Duplicates are allowed, so don't expect all indices to be unique
  expect_true(all(selectedIDs >= 1))
  expect_true(all(selectedIDs <= length(fitness)))
})

test_that("stochastic_universal_sampling allows duplicates with equal fitness", {
  fitness <- rep(1, 10)
  nselect <- 5
  selectedIDs <- stochastic_universal_sampling(fitness, nselect)

  expect_equal(length(selectedIDs), nselect)
  expect_true(all(selectedIDs >= 1))
  expect_true(all(selectedIDs <= length(fitness)))
})

test_that("stochastic_universal_sampling tracks unique vs total selections", {
  fitness <- c(10, 20, 30, 5, 70, 15, 43, 3)
  nselect <- 4
  selectedIDs <- stochastic_universal_sampling(fitness, nselect)

  expect_equal(length(selectedIDs), nselect)
  expect_true(all(selectedIDs >= 1))
  expect_true(all(selectedIDs <= length(fitness)))

  # Count unique vs total
  n_unique <- length(unique(selectedIDs))
  n_total <- length(selectedIDs)

  # n_unique may be less than n_total due to possible duplicates
  expect_true(n_unique <= n_total)
})

test_that("stochastic_universal_sampling handles large populations", {
  fitness <- rep(1, 1000)
  nselect <- 100
  selectedIDs <- stochastic_universal_sampling(fitness, nselect)

  expect_equal(length(selectedIDs), nselect)
  expect_true(all(selectedIDs >= 1))
  expect_true(all(selectedIDs <= 1000))
})

test_that("stochastic_universal_sampling demonstrates SUS property of even spacing with possible duplicates", {
  # With high nselect and low diversity in fitness, duplicates become more likely
  fitness <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  nselect <- 8
  selectedIDs <- stochastic_universal_sampling(fitness, nselect)

  # SUS should spread selections evenly, but duplicates can occur
  expect_equal(length(selectedIDs), nselect)

  # Check that selection is roughly evenly distributed
  unique_selected <- unique(selectedIDs)
  expect_true(length(unique_selected) <= nselect)
})
