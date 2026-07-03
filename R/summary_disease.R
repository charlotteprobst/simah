#' @title Disease Summary
#' @description Generates disease summary
#' @param data dataframe with individuals characteristics
#' @param rsummary dataframe that contains population mortality summary of coditions that are NOT explicitly modelled
#' @param cyear current simulation year
#' @param diseases a vector of specific causes of death that are modelled explicitly in relation to alcohol use
#' @param inflation_factors a vector with inflation factors that are applied to age categories with low
#' observed mortality rates (specified in \code{age_inflated}) to stabilize simulated mortality
#' @param age_inflated a list with age categories to be inflated using \code{inflation_factors}
#' @param full_strata vector of individual characteristics to be used for grouping data
#' @param age_breaks used to generate age groups and specifies the age cutting points
#' @param age_groups used to generate age groups and specifies the group names
#' @export
summary_disease <- function(data,
                            rsummary,
                            cyear,
                            diseases = c("AUD", "DM", "HLVDC", "HYPHD", "IHD", "IJ", "ISTR", "LVDC", "MVACC", "UIJ"),
                            inflation_factors = c(28, 3),
                            age_inflated = list(c("18-24", "25-34", "35-44", "45-54", "55-64"), c("65-74", "75-79")),
                            full_strata = c("sex", "agecat", "education", "race"),
                            age_breaks = c(0, 24, 34, 44, 54, 64, 74, 79),
                            age_groups = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")
) {
  summary_list <- list()
  for (disease in diseases) {

    # generate and add the summary to the list with automatic naming
    summary_list[[paste0(disease)]] <- data %>%
      dplyr::mutate(
        agecat = cut(age, breaks = age_breaks, labels = age_groups),
        inflation_factor = ifelse(
          agecat %in% age_inflated[[1]],
          inflation_factors[1],
          ifelse(agecat %in% age_inflated[[2]], inflation_factors[2], NA)
        )
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(full_strata))) %>%
      dplyr::summarise(
        !!paste0("mort_", disease) := sum(!!dplyr::sym(paste0("mort_", disease)) / inflation_factor),
        !!paste0("yll_", disease) := sum(!!dplyr::sym(paste0("yll_", disease)) / inflation_factor)
      )
  }

  data_out <- data %>%
    dplyr::mutate(agecat = cut(age, breaks = age_breaks, labels = age_groups)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(full_strata))) %>%
    dplyr::tally() %>%
    dplyr::mutate(year = cyear)

  data_out$max_risk <- unique(data$max_risk)

  # join population summary with stored mortality summary of cod that are NOT explicitly modelled
  data_out <- data_out %>%
      dplyr::left_join(rsummary, by = full_strata) %>%
      tidyr::replace_na(list(mort_REST = 0, yll_REST = 0))

  # add mortality summary from coditions that are explicitly modelled
  for (disease in diseases) {
    data_out <-
      dplyr::left_join(data_out, summary_list[[paste0(disease)]], by = full_strata)
  }

  return(data_out)
}
