#' @title Remove outward migrants
#' @description Remove outward migrant individuals from the synthetic population for a given simulation year.
#'              The number of individuals removed is determined using outward migration rates by age group, sex and race,
#'              and individuals are randomly selected from the existing synthetic population within each demographic subgroup.
#' @param data a data frame containing the synthetic population, with at least columns \code{ID}, \code{age}, \code{sex}, \code{race}
#' @param migration_rates a data frame containing outward migration rates by age categories, race, sex and year,
#'                        with at least columns \code{agecat}, \code{race}, \code{sex}, \code{year}, and \code{migrationoutrate}
#' @param cyear current simulation year
#' @return the synthetic population remaining after removing outward migrant individuals
#' @keywords outward migration, population exit
#' @export
remove_emigrants <- function(data, migration_rates, cyear) {

  # calculate the denominator of the outward migration rates,
  # which is the total population counts by age categories, race and sex in the current synthetic population
  age_breaks <- c(0, 18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 100)
  age_groups <- c("18", "19-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")
  emigrants <- data %>%
    dplyr::mutate(n = 1, agecat = cut(age, breaks = age_breaks, labels = age_groups)) %>%
    dplyr::group_by(agecat, race, sex, .drop = FALSE) %>%
    dplyr::summarise(n = sum(n))

  # extract outward migration rates for the specified simulation year, stratified by age categories, race and sex
  migout <- dplyr::filter(migration_rates, year == cyear) %>%
    dplyr::select(agecat, sex, race, migrationoutrate) %>%
    dplyr::distinct() %>%
    tidyr::drop_na()

  # convert outward migration rates into absolute numbers of individuals to be removed from each demographic subgroup;
  # if the calculated number to remove exceeds the available population size in a subgroup, cap removals at the subgroup population size
  emigrants <- dplyr::left_join(emigrants, migout, by = c("agecat", "race", "sex"))
  emigrants <- emigrants %>% dplyr::mutate(toremove = migrationoutrate * n,
                                           toremove = ifelse(toremove > n, n, toremove)) %>%
    dplyr::select(agecat, race, sex, toremove)

  # assign age categories to each individual in the synthetic population to enable matching with subgroup-level removal targets
  temp_data <- data
  temp_data$agecat <- cut(data$age, breaks = age_breaks, labels = age_groups)
  temp_data <- dplyr::left_join(temp_data, emigrants, by = c("race", "sex", "agecat"))

  # replace missing removal targets (i.e., no outward migration rate) with zero removals
  temp_data$toremove[is.na(temp_data$toremove)] <- 0

  # remove outward migrant individuals
  if (length(unique(temp_data$toremove)) == 1) {
    # if no subgroup requires removals, return the population unchanged
    basepopremoved <- data
  } else if (length(unique(temp_data$toremove)) > 1) {
    # sample the required number of individuals for subgroups with positive removal targets
    toremove <- temp_data %>% dplyr::group_by(agecat, race, sex) %>%
      dplyr::do(dplyr::sample_n(., size = unique(toremove), replace = FALSE))
    # identify IDs selected for removal
    ids <- unique(toremove$ID)
    # remove selected individuals from the synthetic population
    basepopremoved <- temp_data %>% dplyr::filter(!ID %in% ids) %>% dplyr::select(-toremove)
  }

  return(basepopremoved)
}

# This function:
# 1. Calculates population counts grouped by agecat, race, sex
# 2. Extracts outward migration rates for the specified year
# 3. Converts rates to counts: toremove = migrationoutrate * n, capped at n
# 4. Assigns age categories to each individual in data
# 5. Joins removal targets to individuals
# 6. Replaces missing rates with 0 (no removals)
# 7. Samples and removes individuals from subgroups with positive removal targets
# 8. Returns remaining population (without the toremove column)

# Key observations:
# Uses different age breaks than other functions: c(0, 18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 100) with labels 18, 19-24, ..., 75-79
# Groups by agecat, race, sex
# Uses dplyr::left_join(..., by = c("agecat", "race", "sex"), .drop = FALSE) to preserve all groups
# Caps removals at population size to avoid negative counts
# Uses sample_n(size = unique(toremove), replace = FALSE) for sampling without replacement

