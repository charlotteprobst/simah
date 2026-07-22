# Load testthat library
library(testthat)

# Set verbosity to suppress warnings during tests
options(microsim_verbosity = 0)

# 2. Define the test case
test_that("summary_disease ", {

  # Minimal test data with just 2 demographic groups
  data <- data.frame(
    ID = 1:4,
    age = c(25, 45, 65, 72),
    sex = c("m", "f", "m", "f"),
    race = c("White", "Black", "White", "Black"),
    education = c("College", "LEHS", "SomeC", "College"),
    mort_AUD = c(10, 5, 8, 3),
    yll_AUD = c(100, 50, 80, 30),
    max_risk = c(0.8, 0.7, 0.85, 0.75),
    stringsAsFactors = FALSE
  )

  # Create with rest mortality (mort_REST and yll_REST)
  rsummary <- data.frame(
    sex = c("m", "f", "m", "f"),
    agecat = c("18-24", "18-24", "18-24", "18-24"),
    race = c("White", "Black", "White", "Black"),
    education = c("College", "LEHS", "SomeC", "College"),
    mort_REST = c(5, 3, 4, 2),
    yll_REST = c(50, 30, 40, 20),
    stringsAsFactors = FALSE
  )

  result <- summary_disease(data, rsummary, cyear = 2000, diseases = "AUD")

  # Verify output structure
  expect_named(result, c("year", "sex", "race", "agecat", "education",
                         "n", "max_risk", "mort_AUD", "yll_AUD",
                         "mort_REST", "yll_REST"), ignore.order = TRUE)
  expect_equal(nrow(result), 4)
  expect_true(all(result$year == 2000))
})
