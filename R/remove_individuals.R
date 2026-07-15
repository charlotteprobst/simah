#' @title Removes individuals for alcohol-related causes of death
#' @description Remove individuals who are staged to die due to alcohol-related causes of
#' death from the synthetic population. Individuals are sampled by subgroup conditional on the
#' cause-specific relative risk corresponding to their alcohol use.
#' @param data a data frame containing the synthetic population at the end of the microsimulation cycle, i.e.,
#' with variables staging individual to die due to alcohol-related causes of death specified in \code{diseases}
#' @param disease a given element in \code{diseases}, i.e., a alcohol-related cause of death
#' @param age_inflated a list with age categories that were inflated using \code{inflation_factors}
#' @param inflation_factors a vector with inflation factors that were applied to age categories with low
#' death counts (in \code{age_inflated}) to stabilize mortality simulation
#' @return the synthetic population remaining after removing individuals who died from alcohol-related causes of death
#' @keywords microsimulation, mortality, cause of death
#' @export
remove_individuals <- function(data, disease, age_inflated, inflation_factors) {

  # filter those who were staged to die from a given cause of death (disease)
    toremove <- data %>%
    dplyr::filter(!!rlang::sym(paste0('mort_', disease)) == 1) %>%
    # calculate N to be removed by population subgroup
    dplyr::group_by(cat) %>%
    dplyr::add_tally() %>%   # this adds the n column with the per-group row count to every row
    dplyr::mutate(
      ageCAT = cut(
        age,
        breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
        labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")
      ),
      # identify inflation factor by age category
      inflation_factor = ifelse(
        ageCAT %in% age_inflated[[1]],
        inflation_factors[1],
        ifelse(ageCAT %in% age_inflated[[2]], inflation_factors[2], NA)
      ),
      # de-inflate to identify correct number of individuals to remove
      nremove = round(n / inflation_factor)
    )

  # ---- HELPER FUNCTION ----
  # note: function to remove individuals based on their relative risk and subgroup
  removingfunction <- function(toremove, disease) {
    # extract N to remove
    N <- unique(toremove$nremove)
    # skip sampling if nothing to remove; return empty row with same structure
    if (length(N) != 1 || is.na(N) || N <= 0)
      return(toremove[0, ])
    # identify each individual's relative risk
    RRs <- toremove %>% dplyr::ungroup() %>% dplyr::select(!!rlang::sym(paste0('RR_', disease)))
    RRs <- as.numeric(unlist(RRs))
    # sample individuals to be removed conditional on their relative risk and using ppswor
    samples <- ppswor(RRs, N)
    toremove <- toremove[samples, ]
    return(toremove)
  }

  # remove individuals
  removed <- toremove %>%
    dplyr::ungroup() %>%
    dplyr::filter(nremove >= 1) %>%
    dplyr::group_by(cat) %>%
    dplyr::do(removingfunction(., disease = disease))

  # store IDs to be removed
  ids <- removed$ID
  # remove the individuals with the specified IDs
  data <- data %>%
    dplyr::filter(!ID %in% ids)

  # remove unnecessary columns of a given cause of death (disease)
  data <- data %>%
    dplyr::select(-c(
      !!rlang::sym(paste0("mort_", disease)),
      !!rlang::sym(paste0("RR_", disease))
    ))

  return(data)
}
