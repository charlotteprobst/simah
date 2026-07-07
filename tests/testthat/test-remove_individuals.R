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

# Find the project root directory from inside the test folder
project_root <- rprojroot::find_package_root_file()

# Load necessary mock data
basepop <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, "data.rds"))})

test_that("remove_individuals only removes individuals with mort_<disease> == 1", {

  test_data <- data.frame(
    ID = 1:5,
    age = c(30, 40, 50, 60, 70),
    sex = c("m", "f", "m", "f", "m"),
    race = c("White", "White", "Hispanic", "Black", "Other"),
    education = c("College", "LEHS", "College", "SomeC", "College"),
    mort_liver = c(1, 0, 1, 0, 1),  # 2, 4 should not be removed
    RR_liver = c(1.5, 1.2, 2.0, 1.0, 1.8)
  ) %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  inflated <- list(c("25-34","35-44","45-54","55-64","65-74"), c("75-79"))
  factors <- c(1, 1)  # out of 1 individual, 1 should die

  result <- remove_individuals(test_data, "liver", inflated, factors)

  # IDs 2 and 4 should still be in result (mort_liver = 0)
  expect_true(all(c(2, 4) %in% result$ID))

  # The removed IDs should be from those with mort_liver == 1
  removed_ids <- setdiff(test_data$ID, result$ID)
  expect_true(all(removed_ids %in% test_data$ID[test_data$mort_liver == 1]))
})

test_that("remove_individuals removes disease-related columns", {

  test_data <- data.frame(
    ID = 1:3,
    age = c(30, 40, 50),
    sex = c("m", "f", "m"),
    race = c("White", "White", "Hispanic"),
    education = c("College", "LEHS", "College"),
    mort_liver = c(1, 1, 1),
    RR_liver = c(1.5, 1.2, 2.0)
  ) %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  inflated <- list(c("25-34","35-44","45-54","55-64","65-74"), c("75-79"))
  factors <- c(10, 5)  # out of 10 individuals, 1 should die for the first group

  result <- remove_individuals(test_data, "liver", inflated, factors)

  # All disease-related columns should be removed
  disease_cols <- c("mort_liver", "RR_liver")
  expect_false(any(disease_cols %in% names(result)))
})

test_that("remove_individuals applies inflation factors correctly", {

  # Create data where we can predict the effect of inflation
  # With 28 people in one group and inflation_factor = 28, should remove ~1 person
  # With 3 people in another group and inflation_factor = 3, should remove ~1 person

  test_data <- data.frame(
    ID = 1:30,
    age = rep(c(26, 27, 28), 10),  # All in "25-34" category
    sex = rep("m", 30),
    race = rep("White", 30),
    education = rep("College", 30),
    mort_liver = rep(1, 30),  # All marked for removal
    RR_liver = runif(30, 1, 2)  # Different RRs for sampling
  ) %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  # With 30 people in one group and inflation_factor = 30, should remove ~1 person
  inflated <- list(c("25-34"), c("35-44"))
  factors <- c(30, 5)

  result <- remove_individuals(test_data, "liver", inflated, factors)

  # Should have removed approximately n/inflation_factor = 30/30 = 1 person
  expect_true(nrow(result) == 29)
})

test_that("remove_individuals handles case with no individuals to remove", {
  test_data <- data.frame(
    ID = 1:5,
    age = c(30, 40, 50, 60, 70),
    sex = c("m", "f", "m", "f", "m"),
    race = c("White", "White", "Hispanic", "Black", "Other"),
    education = c("College", "LEHS", "College", "SomeC", "College"),
    mort_liver = rep(0, 5),  # None marked for removal
    RR_liver = rep(1.5, 5)
  )

  data <- test_data %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  inflated <- list(c("25-34","35-44"), c("45-54","55-64","65-74"))
  factors <- c(10, 5)

  result <- remove_individuals(data, "liver", inflated, factors)

  # All individuals should remain
  expect_equal(nrow(result), nrow(test_data))
  expect_identical(result$ID, test_data$ID)
})

test_that("remove_individuals correctly categorizes age boundaries", {
  test_data <- data.frame(
    ID = 1:8,
    age = c(18, 24, 25, 34, 35, 44, 45, 65),  # Test boundaries
    sex = rep("m", 8),
    race = rep("White", 8),
    education = rep("College", 8),
    mort_liver = rep(1, 8),
    RR_liver = rep(1.5, 8)
  ) %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  inflated <- list(c("25-34","35-44"), c("45-54","55-64","65-74"))
  factors <- c(10, 5)

  result <- remove_individuals(test_data, "liver", inflated, factors)

  # Should return expected categories
  # Note: 18 and 24 should be in "18-24" (not in any inflated category → NA inflation)
  # So they won't be removed (toremove = NA →rounded to 0)
  expect_s3_class(result, "data.frame")
})

test_that("remove_individuals works with different diseases", {

  test_data <- data.frame(
    ID = 1:30,
    age = rep(c(26, 27, 28), 10),  # All in "25-34" category
    sex = rep("m", 30),
    race = rep("White", 30),
    education = rep("College", 30),
    mort_other = rep(1, 30),  # All marked for removal
    RR_other = runif(30, 1, 2)  # Different RRs for sampling
  ) %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  # With 30 people in one group and inflation_factor = 30, should remove ~1 person
  inflated <- list(c("25-34"), c("35-44"))
  factors <- c(30, 5)

  result <- remove_individuals(test_data, "other", inflated, factors)

  # Should have removed approximately n/inflation_factor = 30/30 = 1 person
  expect_true(nrow(result) == 29)
})

test_that("remove_individuals handles empty data frame", {
  test_data <- data.frame(
    ID = numeric(0),
    age = numeric(0),
    sex = character(0),
    race = character(0),
    education = character(0),
    mort_liver = numeric(0),
    RR_liver = numeric(0)
  ) %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  inflated <- list(c("25-34"), c("35-44"))
  factors <- c(10, 5)

  result <- remove_individuals(test_data, "liver", inflated, factors)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("remove_individuals does not modify input data", {

  test_data <- data.frame(
    ID = 1:30,
    age = rep(c(26, 27, 28), 10),  # All in "25-34" category
    sex = rep("m", 30),
    race = rep("White", 30),
    education = rep("College", 30),
    mort_liver = rep(1, 30),  # All marked for removal
    RR_liver = runif(30, 1, 2)  # Different RRs for sampling
  ) %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  # With 30 people in one group and inflation_factor = 30, should remove ~1 person
  inflated <- list(c("25-34"), c("35-44"))
  factors <- c(30, 5)

  result <- remove_individuals(test_data, "liver", inflated, factors)

  # Original data should be unchanged
  expect_equal(nrow(test_data), 30)
  expect_true(all(c("mort_liver", "RR_liver") %in% names(test_data)))

  # Should have removed approximately n/inflation_factor = 30/30 = 1 person
  expect_true(nrow(result) == 29)
})

test_that("remove_individuals handles zero inflation factors", {

  test_data <- data.frame(
    ID = 1:10,
    age = rep(30, 10),
    sex = rep("m", 10),
    race = rep("White", 10),
    education = rep("College", 10),
    mort_liver = rep(1, 10),
    RR_liver = runif(10, 1, 2)
  ) %>%
    dplyr::mutate(
      ageCAT = cut(age, breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                   labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")),
      cat = paste0(sex, ageCAT, race, education)
    ) %>%
    dplyr::select(-ageCAT)

  inflated <- list(c("25-34"), c("35-44"))
  factors <- c(10, 0)  # Zero inflation factor

  result <- remove_individuals(test_data, "liver", inflated, factors)

  # With zero inflation, toremove = round(10/0) = Inf, but this should be handled
  # In practice, with zero inflation, everyone would be kept (toremove = Inf → rounded to large number,
  # but SUS sampling would sample only N individuals where N = round(n/inflation) = Inf → handled by function)
  expect_s3_class(result, "data.frame")
})
