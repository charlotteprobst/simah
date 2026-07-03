# Load testthat library
library(testthat)

# Set verbosity to suppress warnings during tests
options(microsim_verbosity = 0)

# 1. Load the package and data (similar to minimal_test_setup.R)
# We assume the working directory is the project root
PackageDirectory <- "."
devtools::load_all(PackageDirectory)

# Define data paths
DataDirectoryMinimal <- "inputs_data"

# Load necessary mock data
basepop <- readr::read_rds(file.path(DataDirectoryMinimal, "data.rds"))
mort_data <- readr::read_rds(file.path(DataDirectoryMinimal, "mort_data.rds"))

# 2. Define the test case
test_that("apply_death_counts removes individuals and returns correct structure", {

  # Setup parameters
  y <- 2003
  diseases <- c("AUD", "DM", "HLVDC", "HYPHD", "IHD", "IJ", "ISTR", "LVDC", "MVACC", "UIJ")

  # Execute the function
  result <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = y, diseases = diseases)

  # 3. Assertions
  # Check that it returns a list with the expected names
  expect_type(result, "list")
  expect_named(result, c("data", "deaths_REST"))

  # Check that 'data' is a data frame and has fewer or equal rows than basepop
  expect_s3_class(result$data, "data.frame")
  expect_true(nrow(result$data) <= nrow(basepop))

  # Check that IDs in deaths_REST are no longer in result$data
  expect_false(any(result$deaths_REST$ID %in% result$data$ID))

  # Check that the columns in deaths_REST are correct
  expect_true(all(c("ID", "age", "race", "sex", "education") %in% names(result$deaths_REST)))
})

test_that("apply_death_counts properly removes deceased individuals from population", {
  y <- 2003
  diseases <- c("AUD", "DM")

  result <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = y, diseases = diseases)

  # No ID from deaths_REST should appear in the returned data
  expect_identical(
    intersect(result$data$ID, result$deaths_REST$ID),
    integer(0)  # Should be empty
  )

  # Total rows should decrease
  expect_true(nrow(result$data) < nrow(basepop))
})

test_that("apply_death_counts calculates YLL correctly for REST deaths", {
  y <- 2003
  diseases <- c("AUD")

  result <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = y, diseases = diseases)

  # YLL should be 75 - age for those under 75, 0 otherwise
  expected_yll <- ifelse(result$deaths_REST$age < 75, 75 - result$deaths_REST$age, 0)
  expect_equal(result$deaths_REST$yll_REST, expected_yll)

  # mort_REST should all be 1 (all rows in deaths_REST died)
  expect_equal(result$deaths_REST$mort_REST, rep(1, nrow(result$deaths_REST)))
})

test_that("apply_death_counts handles different years correctly", {
  # Test with multiple years if available in mort_data
  years <- unique(mort_data$year)

  # Test with first year
  result1 <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = years[1], diseases = c("AUD"))
  expect_type(result1$data, "list")  # data frame is a list

  # Test with another year if available
  if (length(years) > 1) {
    result2 <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = years[2], diseases = c("AUD"))
    expect_type(result2$data, "list")
  }
})

test_that("apply_death_counts works with different disease selections", {
  y <- 2003

  # Test with no explicitly modelled diseases (all causes become REST)
  diseases_none <- character(0)
  result1 <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = y, diseases = diseases_none)
  expect_type(result1, "list")
  expect_named(result1, c("data", "deaths_REST"))

  # Test with many explicitly modelled diseases
  diseases_many <- c("AUD", "DM", "HLVDC")
  result2 <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = y, diseases = diseases_many)
  expect_type(result2, "list")
})

test_that("apply_death_counts includes all expected columns", {
  y <- 2003
  diseases <- c("AUD")

  result <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = y, diseases = diseases)

  # Check data columns (should be original columns without deceased)
  expect_true(all(c("ID", "age", "sex", "race", "education") %in% names(result$data)))

  # Check deaths_REST columns
  expect_true(all(c("ID", "age", "race", "sex", "education", "mort_REST", "yll_REST") %in% names(result$deaths_REST)))
})

test_that("apply_death_counts does not modify input data", {
  y <- 2003
  diseases <- c("AUD")

  # Make a copy to compare later
  basepop_copy <- basepop
  original_n <- nrow(basepop)

  result <- apply_death_counts(data = basepop, mort_data = mort_data, cyear = y, diseases = diseases)

  # Original should be unchanged
  expect_identical(basepop, basepop_copy)
  expect_identical(nrow(basepop), original_n)
})
