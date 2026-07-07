#' @title Updates former drinker status among abstainers
#' @description Reassigns the \code{formerdrinker} variable for abstainers based on the
#' age- and sex-specific distribution of former drinkers observed among abstainers.
#' Assignment is performed using a uniform random draw.
#' @param data a data frame containing the synthetic population, with minimum variables \code{drinkingstatus},
#' \code{formerdrinker}, \code{agecat}, and \code{sex}
#' @return the synthetic population with updated former drinker status for abstainers
#' @keywords microsimulation, former drinker, abstainer
#' @export
update_former_drinker <- function(data){
  # filter data for abstainers and calculate proportion of former drinkers by sex and age category
  temp <- data %>%
    dplyr::filter(drinkingstatus == FALSE) %>%
    dplyr::group_by(agecat, sex,formerdrinker) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(agecat, sex) %>%
    dplyr::mutate(prop_former_drinker=n/sum(n))

  # store and add former drinker proportion to full data
  temp2 <- temp %>%
    dplyr::filter(formerdrinker == TRUE) %>%
    dplyr::select(-(formerdrinker))

  temp3 <- dplyr::left_join(data, temp2, by = c("sex", "agecat"))

  # assign probability using a uniform random draw and update former drinker status
  temp3$prob <- stats::runif(nrow(temp3))

  abstainers <- temp3 %>%
    dplyr::filter(drinkingstatus == FALSE) %>%
    dplyr::mutate(
      formerdrinker = ifelse(prob<=prop_former_drinker, TRUE, FALSE)
      ) %>%
    dplyr::select(-c(prob, n, prop_former_drinker))

  # combine and return individuals who drink (unchanged) and abstainers with updated former drinker status
  drinkers <- data %>%
    dplyr::filter(drinkingstatus == TRUE)

  data_post <- rbind(drinkers, abstainers)

  return(data_post)
}

# What this function does:
# 1. Filters for abstainers (drinkingstatus == FALSE)
# 2. Calculates proportion of former drinkers by agecat and sex among abstainers
# 3. Joins these proportions back to full dataset
# 4. Generates uniform random values
# 5. Updates former drinker status for abstainers: prop_former_drinker >= prob -> TRUE, else FALSE
# 6. Combines drinkers (unchanged) and abstainers (with updated formerdrinker)
# 7. Returns the full data frame

# Key observations:
# Only changes formerdrinker for abstainers (drinkingstatus == FALSE)
# Drinkers (drinkingstatus == TRUE) remain unchanged
# Uses agecat column (already present in data) for stratification
# Uses sex for stratification
# Proportions calculated from current data, then applied stochastically


