#' @title Transition individuals through alcohol use categories
#' @description Individuals in the synthetic population are sampled to change their alcohol use category
#' using a calibrated ordinal regression model.
#' @param data a data frame containing the synthetic population
#' @param model a data frame containing the coefficients of the ordinal regression model
#' @return a data frame identical to \code{data} with updated \code{alc_cat}
#' @keywords microsimulation, alcohol transition, ordinal regression model
#' @export
transition_alcohol <- function(data, model) {

  if(nrow(data) == 0) {
    msg <- "The population data frame is empty. Alcohol categories are not being updated."
    log_verbosity(msg, level = 1, type = "warn")
    return(data)
  }

  if(nrow(model) == 0) {
    msg <- "The ordinal regression model is empty. Alcohol categories are not being updated."
    log_verbosity(msg, level = 1, type = "warn")
    return(data)
  }

  # create prediction data set with the required variables dummy coded
  data_prediction <- data %>%
    dplyr::mutate(
      agecat = cut(
        age,
        breaks = c(0, 24, 64, 100),
        labels = c("18-24", "25-64", "65+")
      ),
      education = ifelse(education == "College" & agecat == "18-24", "SomeC", education),
      abstainer = ifelse(alc_cat == "Non-drinker" & formerdrinker == FALSE, TRUE, FALSE)
    ) %>%
    dplyr::select(agecat, sex, race, education, alc_gpd, alc_cat, formerdrinker) %>%
    dplyr::mutate(
      Women = ifelse(sex == "f", 1, 0),
      age2564 = ifelse(agecat == "25-64", 1, 0),
      age65 = ifelse(agecat == "65+", 1, 0),
      raceblack = ifelse(race == "Black", 1, 0),
      racehispanic = ifelse(race == "Hispanic", 1, 0),
      raceother = ifelse(race == "Others", 1, 0),
      edulow = ifelse(education == "LEHS", 1, 0),
      edumed = ifelse(education == "SomeC", 1, 0),
      formerdrinker = ifelse(formerdrinker == TRUE, TRUE, FALSE),
      abstainer = ifelse(alc_gpd == 0, TRUE, FALSE),
      cat1 = ifelse(alc_cat == "Low risk", 1, 0),
      cat2 = ifelse(alc_cat == "Medium risk", 1, 0),
      cat3 = ifelse(alc_cat == "High risk", 1, 0),
      cat0 = ifelse(alc_cat == "Non-drinker", 1, 0),
      alc_scaled = (alc_gpd - mean(alc_gpd)) / stats::sd(alc_gpd)
    )

  # reshape model coefficients
  coefs <- model %>% dplyr::select(name, Value) %>%
    tidyr::pivot_wider(names_from = name, values_from = Value)

  # calculate linear predictions using ordinal regression model
  data_prediction$linearpred <-
    as.numeric(coefs['lagged_catLow risk']) * data_prediction$cat1 +
    as.numeric(coefs['lagged_catMedium risk']) * data_prediction$cat2 +
    as.numeric(coefs['lagged_catNon-drinker']) * data_prediction$cat0 +
    as.numeric(coefs['female.factor_2Women']) * data_prediction$Women +
    as.numeric(coefs['lagged_age25-64']) * data_prediction$age2564 +
    as.numeric(coefs['lagged_age65+']) * data_prediction$age65 +
    as.numeric(coefs['lagged_educationLow']) * data_prediction$edulow +
    as.numeric(coefs['lagged_educationMed']) * data_prediction$edumed +
    as.numeric(coefs['race.factor_2Black, non-Hispanic']) * data_prediction$raceblack +
    as.numeric(coefs['race.factor_2Hispanic']) * data_prediction$racehispanic +
    as.numeric(coefs['race.factor_2Other, non-Hispanic']) * data_prediction$raceother +
    as.numeric(coefs['lagged_educationLow:lagged_catLow risk']) * data_prediction$edulow *
    data_prediction$cat1 +
    as.numeric(coefs['lagged_educationMed:lagged_catLow risk']) * data_prediction$edumed *
    data_prediction$cat1 +
    as.numeric(coefs['lagged_educationLow:lagged_catMedium risk']) * data_prediction$edulow *
    data_prediction$cat2 +
    as.numeric(coefs['lagged_educationMed:lagged_catMedium risk']) * data_prediction$edumed *
    data_prediction$cat2 +
    as.numeric(coefs['lagged_educationLow:lagged_catNon-drinker']) * data_prediction$edulow *
    data_prediction$cat0 +
    as.numeric(coefs['lagged_educationMed:lagged_catNon-drinker']) * data_prediction$edumed *
    data_prediction$cat0

  intercept1 <- as.numeric(coefs["Non-drinker|Low risk"])
  intercept2 <- as.numeric(coefs["Low risk|Medium risk"])
  intercept3 <- as.numeric(coefs["Medium risk|High risk"])

  data_prediction$p1 <- 1 / (1 + exp(-(intercept1 - data_prediction$linearpred)))
  data_prediction$p2 <- 1 / (1 + exp(-(intercept2 - data_prediction$linearpred)))
  data_prediction$p3 <- 1 / (1 + exp(-(intercept3 - data_prediction$linearpred)))

  # calculate transition probabilities for each alcohol use category
  data_prediction$prob_cat1 <- data_prediction$p1
  data_prediction$prob_cat2 <- data_prediction$p2 - data_prediction$p1
  data_prediction$prob_cat3 <- data_prediction$p3 - data_prediction$p2
  data_prediction$prob_cat4 <- 1 - data_prediction$p3

  # calculate 'stacked' (cumulative) transition probabilities
  data_prediction$cprob_nondrinker <- data_prediction$prob_cat1
  data_prediction$cprob_lowrisk <- data_prediction$cprob_nondrinker + data_prediction$prob_cat2
  data_prediction$cprob_mediumrisk <- data_prediction$cprob_lowrisk + data_prediction$prob_cat3
  data_prediction$cprob_highrisk <- data_prediction$cprob_mediumrisk + data_prediction$prob_cat4

  # cprob_highrisk is the last probability, implying its cumulative needs to be 1
  data_prediction$cprob_highrisk <- 1

  # sample a random value from a uniform distribution
  data_prediction$random <- stats::runif(nrow(data_prediction))

  # assign alcohol transition by comparing random value to stacked cumulative transition probabilities
  data_prediction$newcat <-
    dplyr::case_when(
      data_prediction$random <= data_prediction$cprob_nondrinker ~ "Non-drinker",
      data_prediction$random >  data_prediction$cprob_nondrinker & data_prediction$random <= data_prediction$cprob_lowrisk ~ "Low risk",
      data_prediction$random >  data_prediction$cprob_lowrisk & data_prediction$random <= data_prediction$cprob_mediumrisk ~ "Medium risk",
      data_prediction$random >  data_prediction$cprob_mediumrisk ~ "High risk",
      TRUE ~ NA_character_  # Explicit catch-all
    )

  data_prediction$totransitioncont <-
    dplyr::case_when(
      data_prediction$newcat == data_prediction$alc_cat  ~ 0,
      data_prediction$newcat == "Non-drinker" ~ 0,
      data_prediction$newcat != data_prediction$alc_cat  ~ 1,
      TRUE ~ NA  # Explicit catch-all
    )

  data$alc_cat <- data_prediction$newcat
  data$totransitioncont <- data_prediction$totransitioncont

  return(data)
}

# What this functions does:
# Creates age categories using cut() with breaks [0,24,64,100] -> ["18-24", "25-64", "65+"]
# Modifies education for ages 18-24 with "College" -> "SomeC"
# Creates dummy variables for: Sex (Women), Age categories (age25-64, age65), Race (black, hispanic, other), Education level (low, med), Former drinker status, Abstainer status, Current alcohol categories (cat0-cat3), alc_scaled
# Calculates linear prediction using coefficients from the model
# Calculates cumulative probabilities for ordinal regression
# Samples using uniform random to assign new categories
# Assigns totransitioncont: 0 if no change, 1 if changed category (except if changed to "Non-drinker")

# Main outputs:
# Modified data$alc_cat column with new categories
# Added data$totransitioncont column (0 or 1)
# Returns the full data frame
