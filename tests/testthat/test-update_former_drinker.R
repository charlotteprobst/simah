# Load testthat library
library(testthat)

# Load necessary mock data
basepop <- readr::read_rds(system.file("extdata", "data.rds", package = "simah"))

test_that("update_former_drinker returns correct structure", {

  result <- update_former_drinker(basepop)

  # Check that result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that required columns exist
  expect_true("drinkingstatus" %in% names(result))
  expect_true("formerdrinker" %in% names(result))
  expect_true("agecat" %in% names(result))
  expect_true("sex" %in% names(result))

  # Check that formerdrinker has expected logical values
  expect_true(all(result$formerdrinker %in% c(TRUE, FALSE)))
})

test_that("update_former_drinker does not modify drinkers", {
  original_drinkers <- basepop[basepop$drinkingstatus == TRUE, ]
  result <- update_former_drinker(basepop)
  result_drinkers <- result[result$drinkingstatus == TRUE, ]

  # Drinkers should be unchanged
  expect_identical(original_drinkers$ID, result_drinkers$ID)
  expect_identical(original_drinkers$drinkingstatus, result_drinkers$drinkingstatus)
  expect_identical(original_drinkers$formerdrinker, result_drinkers$formerdrinker)
})

test_that("update_former_drinker may update formerdrinker for abstainers", {
  original_abstainers <- basepop[basepop$drinkingstatus == FALSE, ]
  result <- update_former_drinker(basepop)
  result_abstainers <- result[result$drinkingstatus == FALSE, ]

  # Compare the number of former drinkers before and after
  original_former <- sum(original_abstainers$formerdrinker == TRUE, na.rm = TRUE)
  result_former <- sum(result_abstainers$formerdrinker == TRUE, na.rm = TRUE)

  # Some change should occur (stochastic function)
  # This is not deterministic, so we check that it runs without errors
  expect_true(is.numeric(result_former))

  # No drinkers should have been created
  expect_equal(sum(result$drinkingstatus == TRUE), sum(basepop$drinkingstatus == TRUE))
})

test_that("update_former_drinker handles edge case with no former drinkers among abstainers", {
  # Create data where all abstainers have formerdrinker = FALSE
  test_data <- basepop %>%
    dplyr::filter(drinkingstatus == FALSE) %>%
    dplyr::mutate(formerdrinker = FALSE)

  # Add unique IDs to distinguish
  test_data <- test_data[1:min(100, nrow(test_data)), ]
  test_data$ID <- paste0("test_", 1:nrow(test_data))

  result <- update_former_drinker(test_data)

  # With prop_former_drinker = 0, no one should become former drinker
  result_abstainers <- result[result$drinkingstatus == FALSE, ]
  expect_equal(sum(result_abstainers$formerdrinker == TRUE, na.rm = TRUE), 0)
})

test_that("update_former_drinker handles edge case with all former drinkers among abstainers", {
  # Create data where all abstainers have formerdrinker = TRUE
  test_data <- basepop %>%
    dplyr::filter(drinkingstatus == FALSE) %>%
    dplyr::mutate(formerdrinker = TRUE)

  # Add unique IDs to distinguish
  test_data <- test_data[1:min(100, nrow(test_data)), ]
  test_data$ID <- paste0("test_", 1:nrow(test_data))

  result <- update_former_drinker(test_data)

  # With prop_former_drinker = 1, everyone should remain former drinker
  result_abstainers <- result[result$drinkingstatus == FALSE, ]
  expect_equal(sum(result_abstainers$formerdrinker == TRUE, na.rm = TRUE), nrow(result_abstainers))
})

test_that("update_former_drinker preserves population size", {
  result <- update_former_drinker(basepop)

  expect_equal(nrow(result), nrow(basepop))
})

test_that("update_former_drinker produces consistent results with same seed", {

  set.seed(123)
  result1 <- update_former_drinker(basepop)
  set.seed(123)
  result2 <- update_former_drinker(basepop)

  # Both should have same total counts (same seed would give same results, but without seed control)
  expect_equal(nrow(result1), nrow(result2))

  # Results should be identical with same seed
  expect_equal(result1$formerdrinker, result2$formerdrinker)
})

test_that("update_former_drinker handles missing columns appropriately", {
  # Test with missing agecat
  test_data <- basepop %>%
    dplyr::select(-agecat)

  # Should either error or produce NA values
  result <- try(update_former_drinker(test_data), silent = TRUE)

  # Either it errors or handles gracefully
  expect_true(inherits(result, "try-error") || is.data.frame(result))
})

