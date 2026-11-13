#' @title Apply death counts for causes that are not explicitly modelled
#' @description Remove individuals who die from causes that are not explicitly modelled (i.e., all causes that are not specified in the \code{diseases} vector)
#' @param data a data frame containing the synthetic population, with at least columns \code{age}, \code{sex}, \code{race} and \code{education}
#' @param mort_data a data frame containing the cause-specific death counts and population demographics by year
#' @param cyear current year of simulation
#' @param diseases a vector of specific causes of death that are modelled explicitly in relation to alcohol use
#' @return a list with (1) a data frame containing the synthetic population remaining after removing individuals
#' who died from causes that are not explicitly modelled; and (2) a data frame of individuals who died
#' @importFrom dplyr %>% mutate filter select group_by summarise left_join do .data
#' @importFrom tidyr pivot_wider pivot_longer complete
#' @importFrom stats runif
#' @keywords microsimulation, mortality
#' @export
apply_death_counts <- function(data, mort_data, cyear, diseases) {
  # summarise synthetic population into population subgroups that match those in the mort_data file, and then merge the two datasets

  age_breaks <- c(0, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 100)
  age_groups <- c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")
  data <- data %>% dplyr::mutate(
    agecat = cut(.data$age, breaks = age_breaks, labels = age_groups),
    cat = paste0(.data$sex, .data$agecat, .data$race, .data$education)
  )
  summary <- data %>%
    dplyr::mutate(n = 1) %>%
    tidyr::complete(.data$cat, fill = list(n = 0)) %>%
    dplyr::group_by(.data$cat, .drop = FALSE) %>%
    dplyr::summarise(n = sum(.data$n))
  mort_data <- mort_data %>% dplyr::filter(.data$year == cyear)
  summary <- dplyr::left_join(summary, mort_data, by = c("cat"))

  # reformat the dataframe and convert the death counts into death rates for each population subgroup
  summary <- summary %>% tidyr::pivot_longer(
    cols = tidyselect::contains("mort"),
    names_to = "cause",
    values_to = "count"
  ) %>%
    dplyr::mutate(cause = gsub("mort", "", .data$cause))

  # calculate the stacked cumulative probability of death across all causes included in the mort_data file
  summary <- summary %>% dplyr::group_by(.data$cat) %>%
    dplyr::mutate(
      count = round(.data$count, digits = 0),
      proportion = .data$count / .data$n,
      cprob = cumsum(.data$proportion)
    )
  options(digits = 22)

  rates <- summary %>% dplyr::select(.data$cat, .data$cause, .data$cprob) %>% dplyr::group_by(.data$cat)

  alldiseases <- gsub("mort", "", names(mort_data)[grepl("mort", names(mort_data))])

  sample_cause <- function(sampled_prob, cumulative_probs, disease_names) {
    if (length(disease_names) != length(cumulative_probs)) {
      stop("disease_names and cumulative_probs must have same length")
    }

    idx <- findInterval(sampled_prob, c(0, cumulative_probs))

    if (idx == 0 || idx > length(disease_names)) {
      return("alive")
    }
    log_verbosity(paste("Cause of death:", disease_names[idx]), level = 3, type = "info")
    return(disease_names[idx])
  }

  # function to identify individuals who die from causes of deaths that are not explicitly modelled
  sample_causes <- function(data, rates) {
    selectedcat <- unique(data$cat)
    rate <- rates %>%
      dplyr::filter(cat == selectedcat) %>%
      tidyr::pivot_wider(names_from = .data$cause, values_from = .data$cprob)

    # Extract cumulative probabilities in the correct order
    cum_probs <- unlist(rate[alldiseases])

    data$sampledprob <- stats::runif(nrow(data))

    # Use the helper function
    data$cause <- sapply(data$sampledprob, function(p) {
      sample_cause(p, cum_probs, alldiseases)
    })

    # filter out those with alive status
    deaths <- data %>% dplyr::filter(.data$cause != "alive")
    # filter out those selected to die from a cause that is explicitly modelled
    deaths <- deaths %>% dplyr::filter(!.data$cause %in% diseases)
    deaths <- deaths %>% dplyr::select(.data$ID, .data$age, .data$race, .data$sex, .data$education)
    # return a list of individuals selected to die from causes that are not explicitly modelled
    return(deaths)
  }

  # identify individuals who die from causes of deaths that are not explicitly modelled
  deaths <- data %>%
    dplyr::group_by(.data$cat) %>%
    dplyr::do(sample_causes(., rates = rates)) %>%
    dplyr::mutate(mort_REST = 1,
                  yll_REST = ifelse(.data$mort_REST == 1 & .data$age < 75, 75 - .data$age, 0))

  # remove individuals who die from the causes that are not explicitly modelled from the synthetic population
  basepopremoved <- data %>% dplyr::filter(!.data$ID %in% deaths$ID)

  # return both death records ("REST") and remaining synthetic population
  return(list(
    data = basepopremoved,
    deaths_REST = deaths
  ))
}

# What this functions does:
# 1. Categorizes individuals by age, sex, race, and education into population subgroups (cat)
# 2. Joins with mortality data for the specified year
# 3. Calculates cumulative probabilities of death for each cause in mort_data
# 4. Samples cause of death for each individual using cumulative probabilities
# 5. Filters results: keeps deaths from non-modelled causes (not in diseases vector) and excludes alive individuals
# 6. Returns list with: (1) remaining population, (2) death records with mort_REST and yll_REST columns


