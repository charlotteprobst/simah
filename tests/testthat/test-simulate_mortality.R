# Load testthat library
library(testthat)

# Set verbosity to suppress warnings during tests
options(microsim_verbosity = 0)

test_that("simulate_mortality adds expected columns", {
  # Create minimal test data with required columns
  test_data <- data.frame(
    drinkingstatus = c(FALSE, TRUE, TRUE, TRUE),
    formerdrinker = c(FALSE, FALSE, FALSE, FALSE),
    age = c(30, 50, 60, 80),
    RR_liver = c(1.0, 1.5, 2.0, 2.5),
    rate_liver = c(0.001, 0.002, 0.003, 0.004)
  )

  result <- simulate_mortality(test_data, diseases = "liver")

  # Check that new columns were added
  expect_identical(names(result), c(names(test_data), "risk_liver", "mort_liver", "yll_liver"))
})

test_that("simulate_mortality calculates risks correctly", {
  test_data <- data.frame(
    drinkingstatus = c(FALSE, TRUE, TRUE, TRUE),
    formerdrinker = c(FALSE, FALSE, FALSE, FALSE),
    age = c(30, 50, 60, 80),
    RR_liver = c(1.0, 1.5, 2.0, 2.5),
    rate_liver = c(0.001, 0.002, 0.003, 0.004)
  )

  result <- simulate_mortality(test_data, diseases = "liver")

  # Expected risk = RR * rate
  expected_risk <- test_data$RR_liver * test_data$rate_liver
  expect_equal(result$risk_liver, expected_risk, tolerance = 1e-10)
})

test_that("simulate_mortality sets risk to 0 for AUD in lifetime abstainers", {
  test_data <- data.frame(
    drinkingstatus = c(FALSE, TRUE, TRUE, TRUE),  # 0 = lifetime abstainer
    formerdrinker = c(FALSE, FALSE, FALSE, FALSE),
    age = c(30, 50, 60, 80),
    RR_AUD = c(1.0, 1.5, 2.0, 2.5),
    rate_AUD = c(0.001, 0.002, 0.003, 0.004)
  )

  result <- simulate_mortality(test_data, diseases = "AUD")

  # First row (lifetime abstainer) should have risk = 0
  expect_equal(result$risk_AUD[1], 0)

  # Others should have risk = RR * rate
  expect_equal(result$risk_AUD[2], 1.5 * 0.002, tolerance = 1e-10)
})

test_that("simulate_mortality handles multiple diseases with cumulative risk", {
  test_data <- data.frame(
    drinkingstatus = c(TRUE, TRUE),
    formerdrinker = c(FALSE, FALSE),
    age = c(50, 60),
    RR_liver = c(1.0, 1.0),
    rate_liver = c(0.1, 0.1),
    RR_dm = c(1.0, 1.0),
    rate_dm = c(0.05, 0.05)
  )

  result <- simulate_mortality(test_data, diseases = c("liver", "dm"))

  # liver risk: 1.0 * 0.1 = 0.1
  # dm risk: 0.1 + (1.0 * 0.05) = 0.15 (cumulative)
  expect_equal(result$risk_liver, c(0.1, 0.1), tolerance = 1e-10)
  expect_equal(result$risk_dm, c(0.15, 0.15), tolerance = 1e-10)
})

test_that("simulate_mortality assigns mortality proportionally to risk", {
  n <- 1000  # number of individuals
  test_data <- data.frame(
    drinkingstatus = rep(TRUE, n),
    formerdrinker = rep(FALSE, n),
    age = rep(50, n),
    RR_liver = rep(2.0, n),
    rate_liver = rep(0.1, n)
  )

  result <- simulate_mortality(test_data, diseases = "liver")

  # All have same risk = 0.2, so ~20% should die
  mortality_rate <- mean(result$mort_liver)
  expect_equal(mortality_rate, 0.2, tolerance = 0.05)
})

test_that("simulate_mortality calculates YLL correctly", {
  test_data <- data.frame(
    drinkingstatus = c(TRUE, TRUE, TRUE, TRUE),
    formerdrinker = c(FALSE, FALSE, FALSE, FALSE),
    age = c(30, 50, 70, 80),  # 75+ won't get YLL
    RR_liver = c(1.0, 1.0, 1.0, 1.0),
    rate_liver = c(10, 10, 10, 10)  # Very high to ensure mortality
  )

  result <- simulate_mortality(test_data, diseases = "liver")

  # All should have mort = 1 (risk = 10 > prob which is 0-1)
  expect_equal(result$mort_liver, c(1, 1, 1, 1))

  # YLL only for age < 75
  expect_equal(result$yll_liver, c(45, 25, 5, 0))
})

test_that("simulate_mortality warns when risk exceeds 1", {
  n <- 10  # number of individuals
  test_data <- data.frame(
    drinkingstatus = rep(TRUE, n),
    formerdrinker = rep(FALSE, n),
    age = rep(50, n),
    RR_liver = rep(11, n),  # Will make risk = 11 * 0.1 = 1.0 > 1
    rate_liver = rep(0.1, n)
  )

  options(microsim_verbosity = 1)
  expect_output(
    simulate_mortality(test_data, diseases = "liver"), "exceeds 1"
  )
})

test_that("simulate_mortality converts input to data.frame", {
  test_data <- tibble::tibble(
    drinkingstatus = c(TRUE, TRUE),
    formerdrinker = c(FALSE, FALSE),
    age = c(50, 60),
    RR_liver = c(1.0, 1.0),
    rate_liver = c(0.1, 0.1)
  )

  result <- simulate_mortality(test_data, diseases = "liver")

  expect_s3_class(result, "data.frame")
})

test_that("simulate_mortality removes prob column", {
  test_data <- data.frame(
    drinkingstatus = c(TRUE, TRUE),
    formerdrinker = c(FALSE, FALSE),
    age = c(50, 60),
    RR_liver = c(1.0, 1.0),
    rate_liver = c(0.1, 0.1)
  )

  result <- simulate_mortality(test_data, diseases = "liver")

  expect_false("prob" %in% names(result))
})

test_that("simulate_mortality handles empty diseases vector", {
  test_data <- data.frame(
    drinkingstatus = c(TRUE),
    formerdrinker = c(FALSE),
    age = c(50),
    RR_liver = c(1.0),
    rate_liver = c(0.1)
  )
  options(microsim_verbosity = 0)

  # This should return data unchanged except for type coercion
  result <- simulate_mortality(test_data, diseases = character(0))

  expect_identical(names(result), c(names(test_data)))
})
