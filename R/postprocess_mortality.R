#' @title Post-process simulated and observed alcohol-related mortality outcomes by demographic subgroup
#' @description Combines alcohol-related, cause-specific mortality outputs (i.e., death counts and and years of
#' life lost (YLL)) for each simulated year with observed death counts, harmonizing age, sex, race, and education
#' subgroup stratification.
#' @param DiseaseSummary list of data frames that contain cause-specific mortality outputs by subgroup for each simulated year
#' @param mort_data option to include observed cause- and subgroup-specific death counts from external empirical data
#' @return a long-format data frame suitable for comparison of simulated and observed mortality outcomes by cause
#' @keywords microsimulation, mortality, output
#' @export
postprocess_mortality <- function(DiseaseSummary, mort_data = NULL) {
  # combine simulated mortality output for all simulated years
  Diseases <- do.call(rbind, DiseaseSummary)

  # option to output observed mortality data alongside simulated mortality outputs
  if (!is.null(mort_data)) {
    # reshape observed mortality data
    mort_data_new <- mort_data %>%
      tidyr::pivot_longer(cols = tidyselect::contains("mort")) %>%
      dplyr::mutate(
        agecat =  dplyr::case_when(
          grepl("18-24", cat) ~ "18-24",
          grepl("25-29", cat) | grepl("30-34", cat) ~ "25-34",
          grepl("35-39", cat) | grepl("40-44", cat) ~ "35-44",
          grepl("45-49", cat) | grepl("50-54", cat) ~ "45-54",
          grepl("55-59", cat) | grepl("60-64", cat) ~ "55-64",
          grepl("65-69", cat) | grepl("70-74", cat) ~ "65-74",
          grepl("75-79", cat) ~ "75-79",
          TRUE ~ NA_character_  # Explicit catch-all
        ),
        sex = dplyr::case_when(grepl("f", cat) ~ "f", grepl("m", cat) ~ "m"),
        education = dplyr::case_when(
          grepl("LEHS", cat) ~ "LEHS",
          grepl("SomeC", cat) ~ "SomeC",
          grepl("College", cat) ~ "College"
        ),
        race = dplyr::case_when(
          grepl("White", cat) ~ "White",
          grepl("Black", cat) ~ "Black",
          grepl("Hispanic", cat) ~ "Hispanic",
          grepl("Others", cat) ~ "Others"
        )
      ) %>%
      dplyr::group_by(year, sex, agecat, race, education, name) %>%
      # sum up observed mortality data by subgroup
      dplyr::summarise(mortality_observed = sum(value)) %>%
      dplyr::mutate(name = gsub("mort", "", name)) %>%
      dplyr::mutate(
        education = ifelse(
          education == "Some",
          "SomeC",
          ifelse(education == "Coll", "College", education)
        ),
        cat = paste0(sex, agecat, race, education)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(year, agecat, sex, race, education, name, mortality_observed) %>%
      tidyr::pivot_wider(
        names_from = name,
        values_from = mortality_observed,
        names_prefix = "mortality_observed_"
      )

    # combine simulated and observed mortality
    Diseases <- dplyr::left_join(Diseases,
                                 mort_data_new,
                                 by = c("agecat", "sex", "race", "education", "year"))
  }

  # rename popcount
  Diseases <- Diseases %>%
    dplyr::rename(popcount = n)

  # reshape mortality output into long-format table
  long_format <- Diseases %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("mort_") |
        dplyr::starts_with("yll_") |
        dplyr::starts_with("mortality_observed_"),
      names_to = "name",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      type =  dplyr::case_when(
        grepl("mortality_observed_", name) ~ "mortality_observed",
        grepl("mort_", name) ~ "mortality_simulated",
        grepl("yll_", name) ~ "yll_simulated"
      ),
      cause = gsub("mortality_observed_", "", name),
      cause = gsub("mort_", "", cause),
      cause = gsub("yll_", "", cause)
    ) %>%
    dplyr::select(-name) %>%
    tidyr::pivot_wider(names_from = type, values_from = value)

  if (!is.null(mort_data)) {
    long_format <- long_format %>%
      dplyr::select(
        year,
        sex,
        race,
        agecat,
        education,
        cause,
        popcount,
        mortality_observed,
        mortality_simulated,
        yll_simulated # ,
        # # note: include max_risk in mortality output to check if maximum individual cumulative risk exceeds 1
        # max_risk
      ) %>%
      dplyr::rename(
        observed_mortality_n = mortality_observed,
        simulated_mortality_n = mortality_simulated,
        simulated_yll_n = yll_simulated
      ) %>%
      dplyr::mutate(sex = ifelse(sex == "f", "Women", "Men"))
  } else{
    long_format <- long_format %>%
      dplyr::select(
        year,
        sex,
        race,
        agecat,
        education,
        cause,
        popcount,
        mortality_simulated,
        yll_simulated # ,
        # # note: include max_risk in mortality output to check if maximum individual cumulative risk exceeds 1
        # max_risk
      ) %>%
      dplyr::rename(simulated_mortality_n = mortality_simulated,
                    simulated_yll_n = yll_simulated) %>%
      dplyr::mutate(sex = ifelse(sex == "f", "Women", "Men"))

  }
  return(long_format)
}
