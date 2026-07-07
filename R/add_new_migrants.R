#' @title Add inward migrants
#' @description Add new inward migrant individuals into the synthetic population for a given simulation year.
#'              The number of entrants is determined using inward migration rates by age group, sex and race and ethnicity (race).
#'              Individuals are sampled from survey data.
#' @param data a data frame containing the synthetic population, with at least columns \code{ID}, \code{age}, \code{sex}, \code{race}
#' @param migration_rates a data frame containing inward migration rates by age categories, race, sex and year,
#'                        with at least columns \code{agecat}, \code{race}, \code{sex}, \code{year}, and \code{migrationinrate}
#' @param svy_data a survey data frame with at least columns \code{YEAR}, \code{age}, \code{sex}, \code{race}, \code{education},
#'                 \code{drinkingstatus}, \code{alc_gpd}, \code{BMI}, \code{income}, \code{formerdrinker}, \code{education_detailed}, \code{alc_cat}
#' @param cyear current simulation year
#' @return a data frame identical to \code{data}, with newly added inward migrant individuals appended to the synthetic population
#' @keywords inward migration, population entry
#' @export
add_new_migrants <- function(data, migration_rates, cyear, svy_data) {

  # In case the simulation year is higher than the maximum year in the migration year
  # then don't add new migrants
  maximum_year <- max(migration_rates$year, na.rm = TRUE)
  if(cyear > maximum_year) {
    msg <- "Current simulation year is higher than the maximum year in the migration rates.
    A new migrants cohort will not be added to the current population."
    log_verbosity(msg, level = 1, type = "warn")
    return(data)
  }

  # extract inward migration rates for the specified simulation year, stratified by age categories, race and sex
  migrants <- migration_rates %>%
    dplyr::filter(year == cyear) %>%
    dplyr::select(agecat, race, sex, migrationinrate) %>%
    tidyr::drop_na()

  # calculate the denominator of the inward migration rates,
  # which is the total population counts by age categories, race and sex in the current synthetic population
  age_breaks <- c(0, 18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79)
  age_groups <- c("18", "19-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")
  denominator <- data %>%
    dplyr::mutate(agecat = cut(age, breaks = age_breaks, labels = age_groups)) %>%
    dplyr::group_by(race, sex, agecat) %>%
    dplyr::tally()

  # convert inward migration rates into absolute counts of new inward migrants
  migrants <- dplyr::left_join(migrants, denominator, by = c("agecat", "race", "sex"))
  migrants$toadd <- migrants$n * migrants$migrationinrate

  # create population category identifiers
  migrants$cat <- paste(migrants$sex, migrants$agecat, migrants$race, sep = "_")
  tojoin <- migrants %>% dplyr::ungroup() %>% dplyr::select(cat, toadd) %>% dplyr::distinct()
  # cats <- unique(tojoin$cat)

  # define a time window for pool selection
  windowmin <- cyear - 1
  windowmax <- cyear + 1

  # construct the donor population pool
  if (cyear <= 2022) {
    # for years up to and including 2022, use survey data
    pool <- svy_data %>%
      dplyr::filter(YEAR >= windowmin & YEAR <= windowmax) %>%
      dplyr::mutate(
        agecat = cut(age, breaks = age_breaks, labels = age_groups),
        cat = paste(sex, agecat, race, sep = "_")
      )
  }

  if (cyear > 2022) {
    # for years after 2022, use the existing synthetic population
    pool <- data %>%
      dplyr::mutate(
        agecat = cut(age, breaks = age_breaks, labels = age_groups),
        cat = paste(sex, agecat, race, sep = "_")
      ) %>%
      dplyr::select(age:BMI, agecat:alc_cat, cat)
  }

  # identify population categories required for entry that are missing from the donor population pool
  # brfsscats <- unique(pool$cat)
  # missing <- setdiff(cats, brfsscats)

  # if (length(missing) > 0) {
  #   # summarise missing population categories
  #   summarymissing <- data.frame(
  #     YEAR = y,
  #     ncatsmissing = length(missing),
  #     whichcatsmissing = paste(missing),
  #     npopmissing = summary$toadd,
  #     npoptotal = sum(tojoin$toadd),
  #     percentmissing = summary$toadd / sum(tojoin$toadd)
  #   )
  #
  # } else {
  #   # no missing population categories
  #   summarymissing <- data.frame(
  #     YEAR = y,
  #     ncatsmissing = 0,
  #     whichcatsmissing = 0,
  #     npopmissing = 0,
  #     npoptotal = sum(tojoin$toadd, na.rm = T),
  #     percentmissing = 0
  #   )
  # }

  # create a filtered pool of individuals to be sampled for entry into the synthetic population
  filtered_pool <- dplyr::left_join(pool, tojoin, by = c("cat")) %>% dplyr::filter(toadd != 0)
  if(nrow(filtered_pool) == 0) {
    msg <- "There are no individuals in the sample pool.
    A new migrants cohort will not be added to the current population."
    log_verbosity(msg, level = 1, type = "warn")
    return(data)
  }

  # sample the required number of new individuals for each population category and prepare them for entry into the synthetic population
  toadd <- filtered_pool %>% dplyr::group_by(cat) %>%
    dplyr::do(dplyr::sample_n(., size = unique(toadd), replace = TRUE)) %>%
    dplyr::mutate(spawn_year = cyear) %>% dplyr::ungroup()

  # The following columns are not needed in the final data frame
  if (cyear <= 2022) {
    # Remove survey data columns and temporary columns
    toadd <- toadd %>% dplyr::select(-c("YEAR", "State", "region", "cat", "toadd"))
  } else {
    # Remove temporary columns
    toadd <- toadd %>% dplyr::select(-c("cat", "toadd"))
  }

  # sample the required number of new individuals for each population category and prepare them for entry into the synthetic population
  # toadd <- dplyr::left_join(pool, tojoin, by = c("cat")) %>% dplyr::filter(toadd != 0) %>% dplyr::group_by(cat) %>%
  #   dplyr::do(dplyr::sample_n(., size = unique(toadd), replace = TRUE)) %>%
  #   dplyr::mutate(spawn_year = y, hed_binary = NA) %>% dplyr::ungroup() %>%
  #   dplyr::select(
  #     age,
  #     race,
  #     sex,
  #     education,
  #     drinkingstatus,
  #     alc_gpd,
  #     BMI,
  #     income,
  #     spawn_year,
  #     agecat,
  #     formerdrinker,
  #     education_detailed,
  #     alc_cat,
  #     hed_binary
  #   )

  # assign unique IDs to newly added inward migrants
  from <- max(data$ID) + 1
  to <- (nrow(toadd)) + max(data$ID)
  ID <- from:to
  toadd <- cbind(ID, toadd)

  # Find missing columns in toadd and set them to NA
  toadd[setdiff(names(data), names(toadd))] <- NA

  # add new inward migrants to the existing synthetic population
  basepopnew <- rbind(data, toadd)
  return(basepopnew)
}

# This function:
# 1. Extracts migration rates for the specified year (not filtering by agecat, unlike add_new_18yo)
# 2. Calculates denominator (population counts) grouped by agecat, race, sex
# 3. Converts rates to counts: toadd = n * migrationinrate
# 4. Creates category identifiers: cat = paste(sex, agecat, race, sep="_")
# 5. Defines time window: cyear - 1 to cyear + 1
# 6. Selects pool: For cyear <= 2022: uses svy_data; For cyear > 2022: uses data (synthetic population)
# 7. Finds filtered pool and samples required number per category
# 8. Assigns new IDs (sequential from max ID + 1)
# 9. Sets missing columns to NA
# 10. Appends new migrants to existing population

# Key differences from add_new_18yo:
# Uses migrationinrate instead of birthrate
# Groups by agecat, race, sex (not just race, sex)
# Switches pool source based on year (survey vs synthetic population)
# All new migrants have spawn_year = cyear
