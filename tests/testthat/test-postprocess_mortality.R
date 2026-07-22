# Load testthat library
library(testthat)

options(microsim_verbosity = 0)

# Load necessary mock data
mort_data <- readr::read_rds(system.file("extdata", "mort_data.rds", package = "simah"))

test_that("postprocess_mortality works without mort_data", {

  year <- 2000
  diseases <- c("AUD", "DM", "HLVDC")

  dsummary <- list(
    data.frame(
      year = year,
      sex = c("m", "m", "f", "f"),
      race = c("White", "Black", "White", "Black"),
      agecat = c("18-24", "18-24", "18-24", "18-24"),
      education = c("College", "LEHS", "College", "LEHS"),
      n = c(1000, 500, 1200, 600),  # population counts
      mort_AUD = c(10, 15, 8, 12),
      yll_AUD = c(100, 150, 80, 120),
      mort_DM = c(5, 8, 3, 6),
      yll_DM = c(50, 80, 30, 60),
      mort_HLVDC = c(3, 5, 2, 4),
      yll_HLVDC = c(30, 50, 20, 40),
      mort_REST = c(20, 25, 18, 22),
      yll_REST = c(150, 180, 130, 160),
      max_risk = c(0.8, 0.9, 0.7, 0.85),
      stringsAsFactors = FALSE
    )
  )

  results <- postprocess_mortality(dsummary)
  expect_true("simulated_yll_n" %in% names(results))
  expect_true("simulated_mortality_n" %in% names(results))
  expect_true(nrow(results) > 0)
})

test_that("postprocess_mortality merges observed data correctly", {

  year <- 2000
  diseases <- c("AUD", "DM", "HLVDC")

  dsummary <- list(
    data.frame(
      year = year,
      sex = c("m", "m", "f", "f"),
      race = c("White", "Black", "White", "Black"),
      agecat = c("18-24", "18-24", "18-24", "18-24"),
      education = c("College", "LEHS", "College", "LEHS"),
      n = c(1000, 500, 1200, 600),  # population counts
      mort_AUD = c(10, 15, 8, 12),
      yll_AUD = c(100, 150, 80, 120),
      mort_DM = c(5, 8, 3, 6),
      yll_DM = c(50, 80, 30, 60),
      mort_HLVDC = c(3, 5, 2, 4),
      yll_HLVDC = c(30, 50, 20, 40),
      mort_REST = c(20, 25, 18, 22),
      yll_REST = c(150, 180, 130, 160),
      max_risk = c(0.8, 0.9, 0.7, 0.85),
      stringsAsFactors = FALSE
    )
  )

  results <- postprocess_mortality(dsummary, mort_data)
  expect_true("observed_mortality_n" %in% names(results))
  expect_true("simulated_mortality_n" %in% names(results))
  expect_true("simulated_yll_n" %in% names(results))
  expect_true(nrow(results) > 0)
})

test_that("postprocess_mortality handles zero counts", {

  dsummary <- list(data.frame(
    year = 2000, sex = "m", race = "White", agecat = "18-24",
    education = "College", n = 100,
    mort_AUD = 0, yll_AUD = 0,  # zero counts
    stringsAsFactors = FALSE
  ))

  results <- postprocess_mortality(dsummary)
  expect_equal(results$simulated_mortality_n, 0)
  expect_equal(results$simulated_yll_n, 0)
  expect_equal(results$popcount, 100)
})

test_that("postprocess_mortality preserves data types", {

  year <- 2000
  diseases <- c("AUD", "DM", "HLVDC")

  dsummary <- list(
    data.frame(
      year = year,
      sex = c("m", "m", "f", "f"),
      race = c("White", "Black", "White", "Black"),
      agecat = c("18-24", "18-24", "18-24", "18-24"),
      education = c("College", "LEHS", "College", "LEHS"),
      n = c(1000, 500, 1200, 600),  # population counts
      mort_AUD = c(10, 15, 8, 12),
      yll_AUD = c(100, 150, 80, 120),
      mort_DM = c(5, 8, 3, 6),
      yll_DM = c(50, 80, 30, 60),
      mort_HLVDC = c(3, 5, 2, 4),
      yll_HLVDC = c(30, 50, 20, 40),
      mort_REST = c(20, 25, 18, 22),
      yll_REST = c(150, 180, 130, 160),
      max_risk = c(0.8, 0.9, 0.7, 0.85),
      stringsAsFactors = FALSE
    )
  )

  results <- postprocess_mortality(dsummary)

  expect_s3_class(results, "data.frame")

  expect_is(results$year, "numeric")
  expect_is(results$sex, "character")
  expect_is(results$race, "character")
  expect_is(results$agecat, "character")
  expect_is(results$education, "character")
})

test_that("postprocess_mortality converts sex codes to labels", {

  dsummary <- list(data.frame(
    year = 2000,
    sex = c("m", "f"),
    race = "White",
    agecat = "18-24",
    education = "College",
    n = 100,
    mort_AUD = c(10, 8),
    yll_AUD = c(100, 80),
    stringsAsFactors = FALSE
  ))

  results <- postprocess_mortality(dsummary)

  expect_true("Men" %in% results$sex)
  expect_true("Women" %in% results$sex)
  expect_true(nrow(results) == 2)
})
