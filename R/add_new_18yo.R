#' @title Add new 18-year-old cohort
#' @description Add new 18-year-old individuals into the synthetic population for a given simulation year. The number
#' of entrants is determined using age-18 entry (birth) rates categorised by sex and race and ethnicity. Individuals
#' are sampled from survey data to match the required demographic subgroups.
#' @param data a data frame containing the synthetic population, with at least columns \code{ID}, \code{age},
#' \code{sex}, \code{race}
#' @param migration_rates a data frame containing age-18 entry (birth) rates by race, sex and year, with at least
#' columns \code{agecat}, \code{race}, \code{sex}, \code{year}, and \code{birthrate}
#' @param svy_data a survey data frame with at least columns \code{YEAR}, \code{age}, \code{sex}, \code{race},
#' \code{education}, \code{drinkingstatus}, \code{alc_gpd}, \code{BMI}, \code{income}, \code{formerdrinker},
#' \code{education_detailed}, \code{alc_cat}
#' @param cyear current simulation year
#' @return a data frame identical to \code{data}, with newly added 18-year-old individuals appended to the synthetic
#' population
#' @keywords births, population entry, 18-year-old population
#' @export
add_new_18yo <- function(data, migration_rates, cyear, svy_data) {

  # In case the simulation year is higher than the maximum year in the migration year
  # then don't add new 18 years old
  maximum_year <- max(migration_rates$year, na.rm = TRUE)
  if(cyear > maximum_year) {
    msg <- "Current simulation year is higher than the maximum year in the migration rates.
    A new 18 years old cohort will not be added to the current population."
    log_verbosity(msg, level = 1, type = "warn")
    return(data)
  }

  # extract age-18 entry (birth) rates for the specified simulation year, stratified by race and sex
  births <- migration_rates %>% dplyr::filter(agecat == "18") %>%
    dplyr::filter(year == cyear) %>%
    dplyr::select(agecat, race, sex, birthrate) %>%
    tidyr::drop_na()

  # calculate the denominator of the rates, which is the total population counts by race and sex in the current
  # synthetic population
  age_breaks <- c(0, 18, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79)
  age_groups <- c("18", "19-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79")
  denominator <- data %>%
    dplyr::mutate(agecat = cut(age, breaks = age_breaks, labels = age_groups)) %>%
    dplyr::group_by(race, sex) %>%
    dplyr::tally()

  # convert entry (birth) rates into absolute counts of new 18-year-olds
  births <- dplyr::left_join(births, denominator, by = c("race", "sex"))
  births$toadd <- births$n * births$birthrate

  # create population category identifiers
  births$cat <- paste(births$sex, births$agecat, births$race, sep = "_")
  tojoin <- births %>% dplyr::ungroup() %>% dplyr::select(cat, toadd)
  # cats <- unique(tojoin$cat)

  # define a time window for pool selection
  windowmin <- cyear - 1
  windowmax <- cyear + 1

  # construct a donor population pool based on survey data
  pool <- svy_data %>%
    dplyr::filter(YEAR >= windowmin & YEAR <= windowmax) %>%
    dplyr::mutate(
      agecat = cut(age, breaks = age_breaks, labels = age_groups),
      cat = paste(sex, agecat, race, sep = "_")
    )

  # create a filtered pool of individuals to be sampled for entry into the synthetic population
  filtered_pool <- dplyr::left_join(pool, tojoin, by = c("cat")) %>% dplyr::filter(toadd != 0)
  if(nrow(filtered_pool) == 0) {
    msg <- "There are no individuals in the sample pool.
    A new 18 years old cohort will not be added to the current population."
    log_verbosity(msg, level = 1, type = "warn")
    return(data)
  }

  # sample the required number of new individuals for each population category and prepare them for entry into the
  # synthetic population
  toadd <- filtered_pool %>% dplyr::group_by(cat) %>%
    dplyr::do(dplyr::sample_n(., size = unique(toadd), replace = TRUE)) %>%
    dplyr::mutate(spawn_year = cyear) %>% dplyr::ungroup()

  # Remove survey data columns, and temporary columns, from toadd that are not needed in the final data frame
  toadd <- toadd %>% dplyr::select(-c("YEAR", "State", "region", "cat", "toadd"))

  # assign unique IDs to newly added individuals
  from <- max(data$ID) + 1
  to <- (nrow(toadd)) + max(data$ID)
  ID <- from:to
  toadd <- cbind(ID, toadd)

  # Find missing columns in toadd and set them to NA
  toadd[setdiff(names(data), names(toadd))] <- NA

  # add new 18-year-olds to the existing synthetic population
  basepopnew <- rbind(data, toadd)
  return(basepopnew)
}

# This function
# 1. Extracts birth rates for the specified year where agecat == "18" from migration_rates
# 2. Calculates population denominator by grouping data by race and sex (using age categories [0,18,24,...])
# 3. Converts rates to counts: toadd = n * birthrate
# 4. Creates category identifiers: cat = paste(sex, agecat, race, sep="_")
# 5. Constructs survey data pool: survey data from years y-1 to y+1
# 6. Finds missing categories between required categories and available survey data
# 7. Samples required number of individuals from survey pool for each category
# 8. Adds new IDs (sequential from max ID + 1)
# 9. Sets missing columns to NA
# 10. Appends new individuals to existing population

# Key observations:
# Only uses survey data with matching demographics
# Adds exactly n * birthrate individuals per demographic group
# Preserves original data and adds to it
# New individuals all have spawn_year = y
# IDs are sequential
# All missing columns set to NA
