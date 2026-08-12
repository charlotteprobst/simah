#' @title Updates alcohol category of individuals
#' @description Updates the alcohol category for each individual in the synthetic population based on their sex and grams-per-day.
#' @param data a data frame containing the synthetic population, with at least columns \code{sex} and \code{alc_gpd}.
#' @return a data frame identical to \code{basepop} with updated \code{alc_cat} values.
#' @keywords microsimulation, alcohol
#' @export
update_alcohol_cat <- function(data) {
  data <- data %>%
    dplyr::mutate(
      alc_cat = dplyr::case_when(
        alc_gpd == 0 ~ "Non-drinker",
        sex == "m" & alc_gpd > 0 & alc_gpd <= 40 ~ "Low risk",
        sex == "f" & alc_gpd > 0 & alc_gpd <= 20 ~ "Low risk",
        sex == "m" & alc_gpd > 40 & alc_gpd <= 60 ~ "Medium risk",
        sex == "f" & alc_gpd > 20 & alc_gpd <= 40 ~ "Medium risk",
        sex == "m" & alc_gpd > 60 ~ "High risk",
        sex == "f" & alc_gpd > 40 ~ "High risk",
        TRUE ~ NA_character_  # Explicit catch-all
      )
    )
  return(data)
}
