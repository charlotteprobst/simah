# Load testthat library
library(testthat)

# 2. Define the test case
description <- "log_verbosity returns formatted message when requested"
test_that(description, {

  # Set verbosity to ensure output is shown
  options(microsim_verbosity = 3)

  result <- log_verbosity("test", level = 1, type = "info", return_msg = TRUE)
  expect_type(result, "character")
  expect_match(result, "\\[INFO\\]")
  expect_match(result, "test")

  # Test info type
  expect_output(
    log_verbosity("test message", level = 1, type = "info"),
    regex = "\\[INFO\\].*test message"
  )

  # Test warn type
  expect_output(
    log_verbosity("warning", level = 1, type = "warn"),
    regex = "\\[WARN\\]"
  )

  # Test error type
  expect_output(
    log_verbosity("error", level = 0, type = "error"),
    regex = "\\[ERROR\\]"
  )

  # High verbosity - should print
  options(microsim_verbosity = 2)
  expect_output(log_verbosity("msg", level = 2), "msg")

  # Low verbosity - should not print
  options(microsim_verbosity = 0)
  expect_silent(log_verbosity("msg", level = 1))
})
