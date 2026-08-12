#' @title Reads project datasets
#' @description Read the datasets that are essential for running the simah microsimulation
#' @return a list of data frames, and these consist of:
#' data a data frame containing the synthetic baseline population, with at least columns \code{ID}, \code{age}, \code{sex}, \code{race},
#'      \code{education}, \code{education_detailed}, \code{drinkingstatus}, \code{alc_cat}, \code{alc_gpd}, \code{formerdrinker}
#' svy_data a survey data frame to supply 18-year-olds and migrants joining the synthetic population over time, with at least
#'      columns \code{YEAR}, \code{age}, \code{sex}, \code{race}, \code{education}, \code{education_detailed}, \code{drinkingstatus},
#'      \code{alc_cat}, \code{alc_gpd}, \code{formerdrinker}
#' mort_data a data frame containing the cause-specific death counts by population subgroup and year, with at least columns \code{year},
#'      \code{cat} as well as \code{CAUSEmort} variables
#' base_rates a data frame containing the cause-specific mortality base rates by population subgroup and year, representing mortality
#'      rates at the theoretical minimal risk exposure level, with at least \code{year}, \code{cat} as well as \code{rate_CAUSE} variables
#' risk_param a data frame containing the risk function parameters for all causes of death specified in \code{diseases}
#' education_transitions a data frame of non-COVID cumulative transition probabilities for each population category and
#'      destination education state, with at least \code{cat}, \code{StateTo} and \code{cumsum}
#' education_transitions_covid a data frame of COVID cumulative transition probabilities for each population category and
#'      destination education state, with at least \code{cat}, \code{StateTo} and \code{cumsum}
#' alcohol_transitions a data frame containing the coefficients of the ordinal regression model to inform transitions between
#'      alcohol use categories
#' catcontmodel a data frame containing the parameters of the beta distributions of grams per day by alcohol use category and
#'      population subgroup
#' migration_rates a data frame containing age-18 entry and migration rates by race, sex and year,
#'      with at least columns \code{agecat}, \code{race}, \code{sex}, \code{year}, \code{birthrate}, \code{migrationinrate},
#'      and \code{migrationoutrate}
#' hed_model_list a list with machine learning models used to annually update heavy episodic drinking (HED) status
#' @export
read_data <- function(config_path = NULL) {

  # 1. Locate the config file: allow user override, default to package internal
  if (is.null(config_path)) {
    config_path <- system.file("config", "default.yaml", package = "simah")
  }

  if (config_path == "" || !file.exists(config_path)) {
    stop("Configuration file not found.")
  }

  input_config <- yaml::read_yaml(config_path)

  # 2. Define a helper to resolve paths
  get_data_path <- function(filename) {
    custom_dir <- input_config[["data_dir"]]

    # If custom_dir is set and file exists, use it
    if (!is.null(custom_dir) && nchar(custom_dir) > 0 && file.exists(file.path(custom_dir, filename))) {
      return(file.path(custom_dir, filename))
    }

    # Fallback to internal package data
    return(system.file("extdata", filename, package = "simah"))
  }


  # 3. Load the data using the helper
  basedata_config <- input_config[["basedata"]]

  basepop <- readr::read_rds(get_data_path(basedata_config[["data"]]))
  svy_data <- readr::read_rds(get_data_path(basedata_config[["svy_data"]]))
  mort_data <- readr::read_rds(get_data_path(basedata_config[["mort_data"]]))
  base_rates <- readr::read_rds(get_data_path(basedata_config[["base_rates"]]))
  risk_param <- readr::read_rds(get_data_path(basedata_config[["risk_param"]]))
  education_transitions <- readr::read_rds(get_data_path(basedata_config[["education_transitions"]]))
  education_transitions_covid <- readr::read_rds(get_data_path(basedata_config[["education_transitions_covid"]]))
  alcohol_transitions <- readr::read_rds(get_data_path(basedata_config[["alcohol_transitions"]]))
  catcontmodel <- readr::read_rds(get_data_path(basedata_config[["catcontmodel"]]))
  migration_rates <- readr::read_rds(get_data_path(basedata_config[["migration_rates"]]))

  # HED data
  heddata <- input_config[["hevdata"]]
  hed_model_list <- list(
    'youngmen' = xgboost::xgb.load(get_data_path(heddata[["youngmen"]])),
    'else'     = xgboost::xgb.load(get_data_path(heddata[["else"]])),
    'oldmen'   = xgboost::xgb.load(get_data_path(heddata[["oldmen"]]))
  )

  outlist <- list(data = basepop, svy_data = svy_data, mort_data = mort_data, base_rates = base_rates, risk_param = risk_param,
                  education_transitions = education_transitions, education_transitions_covid = education_transitions_covid,
                  alcohol_transitions = alcohol_transitions, catcontmodel = catcontmodel, migration_rates = migration_rates,
                  hed_model_list = hed_model_list)
}
