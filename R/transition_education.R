#' @title Transition individuals through educational attainment levels
#' @description Assign a new education attainment level to individuals aged 34 or less based on the cumulative transition probabilities
#' estimated from a calibrated multi-state Markov model.
#' @param data a data frame containing a subgroup of synthetic population corresponding to a particular category \code{cat}
#' @param transitions a data frame of cumulative transition probabilities for each population category and destination education state.
#' @return a data frame identical to \code{data} with an additional column \code{newED} containing the assigned educational attainment level.
#' @keywords education transition, markov model
#' @export
transition_education <- function(data, transitions) {
  # identify the population category and filter cumulative transition probabilities for this group
  selected <- unique(data$cat)
  rates <- transitions %>% dplyr::filter(cat == selected)
  # compare the individual's cumulative transition probability to prob and assign new educational attainment level

  data$newED <- dplyr::case_when(
    data$prob <= rates$cumsum[1] ~ "LEHS",
    data$prob <= rates$cumsum[2] & data$prob > rates$cumsum[1] ~ "SomeC1",
    data$prob <= rates$cumsum[3] & data$prob > rates$cumsum[2] ~ "SomeC2",
    data$prob <= rates$cumsum[4] & data$prob > rates$cumsum[3] ~ "SomeC3",
    data$prob <= rates$cumsum[5] & data$prob > rates$cumsum[4] ~ "College",
    TRUE ~ NA_character_
  )

  return(data)
}
