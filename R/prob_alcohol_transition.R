#' @title Transitions probabilities between alcohol use categories
#' @description Extracts and computes probabilities to transition between alcohol use categories from
#' calibrated ordinal regression model for each population subgroup
#' @param data a data frame containing the synthetic population, with at least columns \code{alc_cat}, \code{alc_gpd}, \code{formerdrinker}, \code{sex}, \code{age}, \code{education}, and \code{race}
#' @param model a data frame with parameters from alcohol transition ordinal regression model
#' @return a data frame containing transition probabilities for each population subgroup and by current alcohol use category
#' @importFrom dplyr %>% mutate select group_by summarise .data
#' @importFrom tidyr pivot_wider
#' @importFrom stats sd
#' @keywords microsimulation, alcohol
#' @export
prob_alcohol_transition <- function(data, model) {

  # create a data set with the required variables dummy coded
  data_prediction <- data %>%
    dplyr::mutate(
      agecat = cut(
        .data$age,
        breaks = c(0, 24, 64, 100),
        labels = c("18-24", "25-64", "65+")
      ),
      abstainer = ifelse(.data$alc_cat == "Non-drinker" & .data$formerdrinker == FALSE, TRUE, FALSE)
    ) %>%
    dplyr::select(.data$agecat, .data$sex, .data$race, .data$education, .data$alc_gpd, .data$alc_cat, .data$formerdrinker) %>%
    dplyr::mutate(
      Women = ifelse(.data$sex == "f", 1, 0),
      age2564 = ifelse(.data$agecat == "25-64", 1, 0),
      age65 = ifelse(.data$agecat == "65+", 1, 0),
      raceblack = ifelse(.data$race == "Black", 1, 0),
      racehispanic = ifelse(.data$race == "Hispanic", 1, 0),
      raceother = ifelse(.data$race == "Others", 1, 0),
      edulow = ifelse(.data$education == "LEHS", 1, 0),
      edumed = ifelse(.data$education == "SomeC", 1, 0),
      formerdrinker = ifelse(.data$formerdrinker == TRUE, TRUE, FALSE),
      abstainer = ifelse(.data$alc_gpd == 0, TRUE, FALSE),
      cat1 = ifelse(.data$alc_cat == "Low risk", 1, 0),
      cat2 = ifelse(.data$alc_cat == "Medium risk", 1, 0),
      cat3 = ifelse(.data$alc_cat == "High risk", 1, 0),
      cat0 = ifelse(.data$alc_cat == "Non-drinker", 1, 0),
      alc_scaled = (.data$alc_gpd - mean(.data$alc_gpd)) / stats::sd(.data$alc_gpd)
    )

  # use coefficients from regression model to compute linear prediction equation
  coefs <- model %>% dplyr::select(.data$name, .data$Value) %>%
    tidyr::pivot_wider(names_from = .data$name, values_from = .data$Value)

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
      prob_nondrinker = .data$p1,
      prob_low = .data$p2 - .data$p1,
      prob_med = .data$p3 - .data$p2,
      prob_high = 1 - .data$p3
    ) %>%
    dplyr::group_by(.data$alc_cat, .data$sex, .data$agecat, .data$race, .data$education) %>%
    dplyr::summarise(
      prob_nondrinker = mean(.data$prob_nondrinker),
      prob_low = mean(.data$prob_low),
      prob_med = mean(.data$prob_med),
      prob_high = mean(.data$prob_high)
    )

  return(data_prediction)
}
