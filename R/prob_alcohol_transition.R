#' @title Transitions probabilities between alcohol use categories
#' @description Extracts and computes probabilities to transition between alcohol use categories from
#' calibrated ordinal regression model for each population subgroup
#' @param data a data frame containing the synthetic population, with at least columns \code{alc_cat}, \code{alc_gpd}, \code{formerdrinker}, \code{sex}, \code{age}, \code{education}, and \code{race}
#' @param model a data frame with parameters from alcohol transition ordinal regression model
#' @return a data frame containing transition probabilities for each population subgroup and by current alcohol use category
#' @keywords microsimulation, alcohol
#' @export
prob_alcohol_transition <- function(data, model) {
  print("get probabilities to stop drinking")

  # create a data set with the required variables dummy coded
  data_prediction <- data %>%
    dplyr::mutate(
      agecat = cut(
        age,
        breaks = c(0, 24, 64, 100),
        labels = c("18-24", "25-64", "65+")
      ),
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

  # use coefficients from regression model to compute linear prediction equation
  coefs <- model %>% dplyr::select(name, Value) %>%
    tidyr::pivot_wider(names_from = name, values_from = Value)

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


  # calculate transition probabilities for each category
  data_prediction <- data_prediction %>%
    dplyr::mutate(
      prob_nondrinker = p1,
      prob_low = p2 - p1,
      prob_med = p3 - p2,
      prob_high = 1 - p3
    ) %>%
    dplyr::group_by(alc_cat, sex, agecat, race, education) %>%
    dplyr::summarise(
      prob_nondrinker = mean(prob_nondrinker),
      prob_low = mean(prob_low),
      prob_med = mean(prob_med),
      prob_high = mean(prob_high)
    )

  return(data_prediction)
}
