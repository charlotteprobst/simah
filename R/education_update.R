#' @title Education update
#' @description Updates education categories for the individuals in data
#' @param covid_scenario indicator specifying which COVID scenario to model; 0 (= non-COVID),
#' 1 (non-COVID before 2020 and after 2022 / COVID in 2020-2022), or 2 (= non-COVID before 2020 / COVID after 2020)
#' @param cyear current simulation year
#' @param education_transitions a data frame of non-COVID cumulative transition probabilities for each population category and
#'      destination education state, with at least \code{cat}, \code{StateTo} and \code{cumsum}
#' @param education_transitions_covid a data frame of COVID cumulative transition probabilities for each population category and
#'      destination education state, with at least \code{cat}, \code{StateTo} and \code{cumsum}
#' @return the synthetic population with education_detailed column updated
#' @keywords microsimulation and education
#' @export
education_update <- function(data, covid_scenario, cyear, education_transitions, education_transitions_covid) {

  if (covid_scenario == 2) {
    if (cyear >= 2020) {
      log_verbosity("Education setup for post-Covid years", level = 1, type = "info")
      data <- setup_education_covid(data)
    } else {
      log_verbosity("Education setup for pre-Covid years", level = 1, type = "info")
      data <- setup_education(data)
    }
  } else if (covid_scenario == 1) {
    if (cyear >= 2020 & cyear <= 2022) {
      log_verbosity("Education setup for Covid years", level = 1, type = "info")
      data <- setup_education_covid(data)
    } else {
      log_verbosity("Education setup excluding Covid years", level = 1, type = "info")
      data <- setup_education(data)
    }
  } else {
    log_verbosity("Education setup", level = 1, type = "info")
    data <- setup_education(data)
  }

  if (covid_scenario == 2) {
    # apply transitions using covid TPs permanently after 2020
    if (cyear >= 2020) {
      log_verbosity("Processing education transitions for post-Covid years", level = 1, type = "info")
      data <- data %>% dplyr::group_by(cat) %>%
        dplyr::do(transition_education(., education_transitions_covid))
    } else {
      log_verbosity("Processing education transitions for pre-Covid years", level = 1, type = "info")
      data <- data %>% dplyr::group_by(cat) %>%
        dplyr::do(transition_education(., education_transitions))
    }
  } else if (covid_scenario == 1) {
    # apply covid TPs to real covid time
    if (cyear >= 2020 & cyear <= 2022) {
      log_verbosity("Processing education transitions during Covid years", level = 1, type = "info")
      data <- data %>% dplyr::group_by(cat) %>%
        dplyr::do(transition_education(., education_transitions_covid))
    } else {
      log_verbosity("Processing education transitions excluding Covid years", level = 1, type = "info")
      data <- data %>% dplyr::group_by(cat) %>%
        dplyr::do(transition_education(., education_transitions))
    }
  } else {
    # apply tps for pre-covid throughout
    log_verbosity("Processing education transitions", level = 1, type = "info")
    data <- data %>% dplyr::group_by(cat) %>%
      dplyr::do(transition_education(., education_transitions))
  }

  # code newly assigned education category
  data$education_detailed <- data$newED
  data$education <-
    dplyr::case_when(
      data$education_detailed == "LEHS" ~ "LEHS",
      data$education_detailed == "SomeC1" ~ "SomeC",
      data$education_detailed == "SomeC2" ~ "SomeC",
      data$education_detailed == "SomeC3" ~ "SomeC",
      data$education_detailed == "College" ~ "College",
      TRUE ~ NA_character_  # Explicit catch-all
    )

  # Removes the any grouping that was applied to the data frame and also removes four columns
  data <- data %>% dplyr::ungroup() %>% dplyr::select(-c(prob, state, newED, cat))

  return(data)
}
