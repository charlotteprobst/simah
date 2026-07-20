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
read_data <- function() {

  # Find the project root directory
  project_root <- rprojroot::find_package_root_file()

  # Load config file
  config_file <- file.path(project_root, "config/default.yaml")
  input_config <- yaml::read_yaml(config_file)

  # Folder with data
  DataDirectoryMinimal <- input_config[["data_dir"]]

  # Base data
  basedata <- input_config[["basedata"]]
  basepop <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["data"]]))})
  svy_data <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["svy_data"]]))}) # formerly brfss
  mort_data <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["mort_data"]]))}) # formerly death_counts; already processed
  base_rates <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["base_rates"]]))})
  risk_param <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["risk_param"]]))})
  education_transitions <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["education_transitions"]]))})
  education_transitions_covid <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["education_transitions_covid"]]))})
  alcohol_transitions <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["alcohol_transitions"]]))})
  catcontmodel <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["catcontmodel"]]))})
  migration_rates <- withr::with_dir(project_root, {readr::read_rds(file.path(DataDirectoryMinimal, basedata[["migration_rates"]]))})

  # HED data
  heddata <- input_config[["hevdata"]]
  hed_model_list <- list(
    'youngmen' = withr::with_dir(project_root, {xgboost::xgb.load(file.path(DataDirectoryMinimal, heddata[["youngmen"]]))}),
    'else'     = withr::with_dir(project_root, {xgboost::xgb.load(file.path(DataDirectoryMinimal, heddata[["else"]]))}),
    'oldmen'   = withr::with_dir(project_root, {xgboost::xgb.load(file.path(DataDirectoryMinimal, heddata[["oldmen"]]))})
  )

  outlist <- list(data = basepop, svy_data = svy_data, mort_data = mort_data, base_rates = base_rates, risk_param = risk_param,
                  education_transitions = education_transitions, education_transitions_covid = education_transitions_covid,
                  alcohol_transitions = alcohol_transitions, catcontmodel = catcontmodel, migration_rates = migration_rates,
                  hed_model_list = hed_model_list)
}
