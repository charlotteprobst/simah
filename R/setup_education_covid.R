#' @title Setup population subgroups for education transitions (COVID-19)
#' @description Categorise individuals into population subgroups that align with the COVID-19 education transition model.
#' Assign a random value from a uniform distribution to prepare subsequent education transitions.
#' @param data a data frame containing the synthetic population, with at least columns \code{age}, \code{sex},
#'                \code{race} and \code{education_detailed}
#' @return a data frame identical to \code{data} with additional columns containing \code{agecat}, \code{state},
#'          \code{cat} and \code{prob} values
#' @keywords education, COVID-19 education transition model
#' @export
setup_education_covid <- function(data) {
  # summarise population subgroup categories by age, sex, race and ethnicity, and education state,
  # aligned with the categories used in the COVID-19 education transition model

  data$agecat <-
    dplyr::case_when(
      data$age == 18 ~ "18",
      data$age == 19 ~ "19",
      data$age == 20 ~ "20",
      data$age >= 21 & data$age <= 25 ~ "21-25",
      data$age >  25 ~ "26+",
      TRUE ~ NA_character_  # Explicit catch-all
    )

  data$state <-
    dplyr::case_when(
      data$education_detailed == "LEHS" ~ 1,
      data$education_detailed == "SomeC1" ~ 2,
      data$education_detailed == "SomeC2" ~ 3,
      data$education_detailed == "SomeC3" ~ 4,
      data$education_detailed == "College" ~ 5,
      TRUE ~ NA  # Explicit catch-all
    )

  data$racecat <-
    dplyr::case_when(
      data$race == "Black"    ~ "black",
      data$race == "White"    ~ "white",
      data$race == "Hispanic" ~ "hispanic",
      data$race == "Others"   ~ "other",
      TRUE ~ NA_character_  # Explicit catch-all
    )

  data$cat <- paste(data$agecat,
                       data$sex,
                       data$racecat,
                       "STATEFROM",
                       data$state,
                       sep = "_")
  # remove education-specific categories (not used in the main simulation)
  data$racecat <- NULL
  # assign a random value sampled from a uniform distribution to each individual
  data$prob <- stats::runif(nrow(data))
  return(data)
}
