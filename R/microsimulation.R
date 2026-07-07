#' @title Main microsimulation function
#' @description This function implements and schedules the core simulation processes that advance the synthetic population according to
#' population dynamics, life course transitions, and potential policy changes. The simulation progresses in annual steps. For each
#' simulation year from 2000 to \code{maxyear}, the same sequence is executed and outputs are summarized according to \code{output}
#' and population \code{strata}. Essential input data (e.g., synthetic baseline population in 2000) and model parameters
#' (e.g., mortality statistics, education and alcohol transitions, and parameters linking alcohol consumption to specific causes of death)
#' must be supplied.
#' @param data a data frame containing the synthetic baseline population, with at least columns \code{ID}, \code{age}, \code{sex}, \code{race},
#'      \code{education}, \code{education_detailed}, \code{drinkingstatus}, \code{alc_cat}, \code{alc_gpd}, \code{formerdrinker}
#' @param svy_data a survey data frame to supply 18-year-olds and migrants joining the synthetic population over time, with at least
#'      columns \code{YEAR}, \code{age}, \code{sex}, \code{race}, \code{education}, \code{education_detailed}, \code{drinkingstatus},
#'      \code{alc_cat}, \code{alc_gpd}, \code{formerdrinker}
#' @param maxyear a numeric value of the maximum simulation year
#' @param mort_data a data frame containing the cause-specific death counts by population subgroup and year, with at least columns \code{year},
#'      \code{cat} as well as \code{CAUSEmort} variables
#' @param base_rates a data frame containing the cause-specific mortality base rates by population subgroup and year, representing mortality
#'      rates at the theoretical minimal risk exposure level, with at least \code{year}, \code{cat} as well as \code{rate_CAUSE} variables
#' @param diseases a vector of specific causes of death that are modelled explicitly in relation to alcohol use
#' @param risk_param a data frame containing the risk function parameters for all causes of death specified in \code{diseases}
#' @param inflation_factors a vector with inflation factors that are applied to age categories with low
#'      observed mortality rates (specified in \code{age_inflated}) to stabilize simulated mortality
#' @param age_inflated a list with age categories to be inflated using \code{inflation_factors}
#' @param education_transitions a data frame of non-COVID cumulative transition probabilities for each population category and
#'      destination education state, with at least \code{cat}, \code{StateTo} and \code{cumsum}
#' @param education_transitions_covid a data frame of COVID cumulative transition probabilities for each population category and
#'      destination education state, with at least \code{cat}, \code{StateTo} and \code{cumsum}
#' @param COVID_specific_tps indicator specifying which COVID scenario to model; 0 (= non-COVID),
#'      1 (non-COVID before 2020 and after 2022 / COVID in 2020-2022), or 2 (= non-COVID before 2020 / COVID after 2020)
#' @param updatingalcohol indicator for whether to update alcohol use; FALSE or TRUE
#' @param alcohol_transitions a data frame containing the coefficients of the ordinal regression model to inform transitions between
#'      alcohol use categories
#' @param catcontmodel a data frame containing the parameters of the beta distributions of grams per day by alcohol use category and
#'      population subgroup
#' @param hed_model_list a list with machine learning models used to annually update heavy episodic drinking (HED) status
#' @param counterfactual indicator for whether to model a counterfactual scenario where everyone is at the theoretical
#'      minimal risk exposure level of alcohol use; 0 or 1
#' @param migration_rates a data frame containing age-18 entry and migration rates by race, sex and year,
#'      with at least columns \code{agecat}, \code{race}, \code{sex}, \code{year}, \code{birthrate}, \code{migrationinrate},
#'      and \code{migrationoutrate}
#' @param output a character vector specifying the types of outputs to summarize in each annual cycle of the simulation; options include
#'      "demographics", "alcoholcat", "alcoholcont", and "mortality"
#' @param strata a named list specifying stratification variables for each output type; options include "sex", "agecat", "education",
#'      and "race"
#' @param seed random numeric seed used for stochastic processes in the microsimulation
#' @param nunc numeric identifier for the unique combination of microsimulation parameters
#' @param microsim_verbosity integer controlling output level: 0 = silent (only errors),
#'      1 = default (progress info), 2+ = full verbose (detailed logs)
#' @return a list containing outputs specified in \code{output}, summarized by \code{strata}, for each simulated year
#' @keywords microsimulation, main function
#' @export
microsimulation <- function(data, svy_data, maxyear = 2030,
                            mort_data, base_rates,
                            diseases = c("AUD", "DM", "HLVDC", "HYPHD", "IHD", "IJ", "ISTR", "LVDC", "MVACC", "UIJ"),
                            risk_param,
                            inflation_factors = c(28, 3),
                            age_inflated = list(c("18-24","25-34","35-44","45-54","55-64"), c("65-74", "75-79")),
                            education_transitions,
                            education_transitions_covid,
                            COVID_specific_tps = 1,
                            updatingalcohol = TRUE,
                            alcohol_transitions,
                            catcontmodel,
                            hed_model_list,
                            counterfactual = 0,
                            migration_rates,
                            output = c("demographics", "alcoholcat", "alcoholcont", "mortality"), # sbi - policy_sbi_cascade
                            strata = list(
                              alcoholcat  = c("sex", "agecat", "education", "race"),
                              alcoholcont = c("sex", "agecat", "education", "race"),
                              demographics = c("sex", "agecat", "education", "race"),
                              mortality = c("sex", "agecat", "education", "race")
                            ),
                            seed = 1, nunc = 1, microsim_verbosity = 1
                            ){
  set.seed(seed)

  options(microsim_verbosity = microsim_verbosity)

  # set start year of microsimulation
  minyear <- 2000

  # prepare data objects
  Summary <- list()
  DiseaseSummary <- list()
  PopPerYear <- list()
  RestSummary <- list()
  # PLACEHOLDER: Insert some warnings/plausibility checks, e.g., are data objects provided if needed (COVID TPs etc.)

# ===== simulation loop in annual steps [y] from 2000 ====

  for (y in minyear:maxyear) {
    log_verbosity(paste("Simulating year", y), level = 1, type = "info")

    if (counterfactual==0 & y >= minyear){
      # update HED
      data <- update_hed(data, hed_model_list[[1]], hed_model_list[[2]], hed_model_list[[3]])
    }

    # to model counterfactual scenario at theoretical minimum risk exposure level
    if (counterfactual == 1 & y >= minyear) {
      # counterfactual: no alcohol use
      data$alc_gpd <- 0
      data$formerdrinker <- FALSE
      data$drinkingstatus <- FALSE
      updatingalcohol <- FALSE
      data$hed_binary <- FALSE
    }

    if (counterfactual==2 & y >= minyear){
      # counterfactual: no HED between 0 and 60 g/day
      data <- data %>%
          dplyr::mutate(
            hed_binary = dplyr::case_when(
              drinkingstatus == FALSE ~ FALSE,
              drinkingstatus == TRUE & alc_gpd <  60 ~ FALSE,
              drinkingstatus == TRUE & alc_gpd >= 60 ~ TRUE,
              TRUE ~ 0
            )
          )
    }

    # create alcohol outputs
    if (any(grepl("alcohol", output))) {
      base_strata <- c("year")

      # --- Summary output for continuous alcohol use ---
      if ("alcoholcont" %in% output) {
        full_strata <- unique(c(base_strata, strata[["alcoholcont"]]))

        Summary[["alcoholcont"]][[paste(y)]] <- data %>%
          dplyr::mutate(year = y, agecat = cut(
            age,
            breaks = c(0, 24, 64, 100),
            labels = c("18-24", "25-64", "65+")
          )) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(full_strata)), .drop = FALSE) %>%
          dplyr::reframe(
            meansimulation = mean(alc_gpd, na.rm = TRUE),
            sdsimulation   = stats::sd(alc_gpd, na.rm = TRUE),
            n_hed = sum(hed_binary, na.rm = TRUE),
            hed_prop = mean(hed_binary, na.rm = TRUE),
            seed = seed,
            nunc = nunc
          )
      }

      # --- Summary output for categorical alcohol use ---
      if ("alcoholcat" %in% output) {
        full_strata <- unique(c(base_strata, strata[["alcoholcat"]]))

        Summary[["alcoholcat"]][[paste(y)]] <- data %>%
          dplyr::mutate(
            year = y,
            agecat = cut(
              age,
              breaks = c(0, 24, 64, 100),
              labels = c("18-24", "25-64", "65+")
            ),
            education = ifelse(agecat == "18-24" &
                                 education == "College", "SomeC", education)
          ) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c(
            full_strata, "alc_cat"
          ))), .drop = FALSE) %>%
          dplyr::reframe(n = dplyr::n(),
                         hed_prop = mean(hed_binary, na.rm = TRUE),
                         n_hed = sum(hed_binary)) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(full_strata)), .drop = FALSE) %>%
          dplyr::mutate(
            propsimulation = n / sum(n),
            seed = seed,
            nunc = nunc
          ) %>%
          dplyr::ungroup()
      }

      # --- Summary output for heavy episodic drinking ---
      if ("hed" %in% output) {
        full_strata <- unique(c(base_strata, strata[["hed"]]))

        Summary[["hed"]][[paste(y)]] <- data %>%
          dplyr::mutate(year = y,
                        agecat = cut(age, breaks = c(0, 20, 34, 64, 100), labels = c("18-20", "21-34", "35-64", "65+")),
                        education = ifelse(agecat == "18-20" & education == "College", "SomeC", education)) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(full_strata)), .drop = FALSE) %>%
          dplyr::reframe(n = dplyr::n(),
                         n_hed = sum(hed_binary),
                         hed_prop = mean(hed_binary, na.rm = TRUE),
                         seed = seed,
                         nunc = nunc
          )
      }

      # --- Summary output for heacy episodic drinking - new categories ---
      if ("hed_cat" %in% output) {
        full_strata <- unique(c(base_strata, strata[["hed_cat"]]))

        age_breaks <- c(0, 20, 34, 64, 100)
        age_groups <- c("18-20", "21-34", "35-64", "65+")
        Summary[["hed_cat"]][[paste(y)]] <- data %>%
          dplyr::mutate(year = y, agecat = cut(age, breaks = age_breaks, labels = age_groups),
                        education = ifelse(agecat == "18-20" & education == "College", "SomeC", education),
                        alc_cat = dplyr::case_when(
                          alc_gpd < 1 ~ "Minimal and non-drinker",
                          alc_gpd >= 1 & alc_gpd < 60 ~ "Occasional and regular drinker",
                          alc_gpd >= 60 ~ "Heavy drinker",
                          TRUE ~ NA_character_
                        )) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(full_strata)), .drop = FALSE) %>%
          dplyr::summarise(n = dplyr::n(),
                           n_hed = sum(hed_binary),
                           hed_prop = mean(hed_binary, na.rm = TRUE),
                           seed = seed,
                           nunc = nunc
          )
      }
    }

    # store summary of the synthetic population
    PopPerYear[[paste(y)]] <- data %>%
      dplyr::mutate(year = y, seed = seed, nunc = nunc)

    # MORTALITY
    # simulate mortality for causes that are not explicitly modelled ("REST");
    # and store in list alongside updated synthetic population
    data_list <- apply_death_counts(data, mort_data, y, diseases)
    data <- data_list$data

    # record and summarise mortality by population subgroup for causes that are not explicitly modelled ("REST")
    age_breaks <- c(0, 24, 34, 44, 54, 64, 74, 79)
    age_groups <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")

    full_strata <- strata[["mortality"]]
    rsummary <- data_list$deaths_REST %>%
      dplyr::mutate(agecat = cut(age, breaks = age_breaks, labels = age_groups)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(full_strata))) %>%
      dplyr::summarise(mort_REST = sum(mort_REST),
                       yll_REST = sum(yll_REST))
    RestSummary[[paste(y)]] <- rsummary

    # if diseases are specified in the disease vector, simulate mortality from those specific diseases
    if (!rlang::is_empty(diseases)) {  # Returns TRUE if length is 0 or if NULL

      data <- assign_rr(data, diseases, risk_param)

      data <- data %>%
        dplyr::mutate(
          ageCAT = cut(age, breaks = age_breaks, labels = age_groups),
          cat = paste0(sex, ageCAT, race, education)
        ) %>%
        dplyr::select(-ageCAT)

      # merge mortality rates at the theoretical minimal risk exposure level
      rates <- base_rates %>%
        dplyr::filter(year == y) %>%
        dplyr::select(cat, dplyr::all_of(paste0("rate_", diseases)))
      data <- dplyr::left_join(data, rates, by = c("cat"))

      # stage if and from which cause an individual dies
      diseases_string <- paste(diseases, collapse = ", ")
      lmsg <- paste("Simulate mortality for causes that are not explicitly modelled, and these are:", diseases_string)
      log_verbosity(lmsg, level = 1, type = "info")
      data <- simulate_mortality(data, diseases)

      dsummary <- summary_disease(data, rsummary, y, diseases, inflation_factors, age_inflated, full_strata,
                                  age_breaks, age_groups)
      DiseaseSummary[[paste(y)]] <- dsummary

      # sample the correct proportion of those to be removed (due to inflated mortality rate)
      for (disease in diseases) {

        # Remove unnecessary columns for a given cause of death
        data <- data %>%
          dplyr::select(-c(
            !!rlang::sym(paste0("risk_", disease)),
            !!rlang::sym(paste0("rate_", disease)),
            !!rlang::sym(paste0("yll_", disease))
          ))

        # remove individuals due to their assigned disease-specific risk
        data <- remove_individuals(data, disease, age_inflated, inflation_factors)
      }

      # Remove cat column
      data$cat <- NULL
    }

    # transition education for individuals aged 34 and under
    totransition <- data %>% dplyr::filter(age <= 34)
    totransition <- education_update(data = totransition, covid_scenario=COVID_specific_tps, cyear=y,
                                     education_transitions = education_transitions,
                                     education_transitions_covid = education_transitions_covid)
    tostay <- data %>% dplyr::filter(age > 34)
    data <- rbind(totransition, tostay)

    # update alcohol use categories
    if (updatingalcohol == TRUE) {
      log_verbosity("Processing alcohol transitions", level = 1, type = "info")
      data <- transition_alcohol(data, alcohol_transitions)
      # allocate a new numeric grams per day to individuals that have changed alcohol use categories
      if (is.null(catcontmodel) == FALSE) {
        log_verbosity("Sample grams per day for individuals previously transitioned", level = 1, type = "info")
        data <- allocate_gramsperday_sampled(data, catcontmodel)
        data <- data %>% dplyr::mutate(drinkingstatus = ifelse(alc_gpd == 0, FALSE, TRUE))
        # allocate former drinker status - note: this is not tracked over time
        data <- update_former_drinker(data)
      } else if (is.null(catcontmodel) == TRUE) {
        data$totransitioncont <- NULL
      }
    }

    # age everyone by 1 year and update age category
    age_breaks <- c(0, 19, 24, 34, 44, 54, 64, 74, 100)
    age_groups <- c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-79")
    data <- data %>%
      dplyr::mutate(age = age + 1,
                    agecat = cut(age, breaks = age_breaks, labels = age_groups))

    # remove anyone over 79
    data <- subset(data, age <= 79)

    # add 18-year olds, add new migrants and remove emigrants
    if (y <= 2030) {
      log_verbosity("Adding 18 years old to the population", level = 1, type = "info")
      data <- add_new_18yo(data, migration_rates, y, svy_data)
      log_verbosity("Adding migrants to the population", level = 1, type = "info")
      data <- add_new_migrants(data, migration_rates, y, svy_data)
      log_verbosity("Removing emigrants from the population", level = 1, type = "info")
      data <- remove_emigrants(data, migration_rates, y)
    }

  }

  log_verbosity("Simulation complete, processing outputs", level = 1, type = "info")

  # save output

  # --- store mortality output in summary ---
  # if ("mortality" %in% output) {
  if ("mortality" %in% output & !is.null(diseases)) {
    Summary$mortality <- postprocess_mortality(DiseaseSummary, mort_data = NULL) %>%
      dplyr::mutate(seed = seed, nunc = nunc)
  } else if ("mortality" %in% output & is.null(diseases)) {
    Summary$mortality <- lapply(names(RestSummary), function(y) {
      RestSummary[[paste(y)]] %>% dplyr::mutate(year = y)
    }) %>%
      do.call(rbind, .) %>% dplyr::mutate(seed = seed, nunc = nunc)
  }

  # --- store demographics output in summary ---
  if ("demographics" %in% output) {
    base_strata <- c("year", "seed", "nunc")
    full_strata <- unique(c(base_strata, strata[["demographics"]])) # user-defined strata

    age_breaks <- c(0, 18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 100)
    age_groups <- c("18", "19-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                    "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")
    for (i in seq_along(PopPerYear)) {
      PopPerYear[[i]]$agecat <- cut(
        PopPerYear[[i]]$age,
        breaks = age_breaks,
        labels = age_groups
      )

      PopPerYear[[i]] <- PopPerYear[[i]] %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(full_strata))) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
    }

    Summary$demographics <- do.call(rbind, PopPerYear)
  }


  # --- store alcohol output in summary ---
  if (any(grepl("alcohol", output))) {
    # find all output types that are alcohol-related
    alcohol_keys <- names(Summary)[grepl("^alcohol", names(Summary))]

    # combine yearly entries for each type
    for (k in alcohol_keys) {
      if (length(Summary[[k]]) > 0 && is.list(Summary[[k]][[1]])) {
        Summary[[k]] <- dplyr::bind_rows(Summary[[k]])
      }
    }
  }

  # --- store hed output in summary ---
  if ("hed" %in% output) {
    Summary$hed <- do.call(rbind, Summary[["hed"]])
  }

  if ("hed_cat" %in% output) {
    Summary$hed_cat <- do.call(rbind, Summary[["hed_cat"]])
  }

  # --- return final combined summary ---
  return(Summary)
}
