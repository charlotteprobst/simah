#' @title Update heavy episodic drinking (HED) status
#' @description Assign HED status based on a supplied Machine Learning (ML-) Model by subgroup.
#' @param data a data frame containing the synthetic population, with at least columns \code{ID}, \code{age}, \code{sex}, \code{race},
#'      \code{education}, \code{education_detailed}, \code{drinkingstatus}, \code{alc_cat}, \code{alc_gpd}, \code{formerdrinker}
#' @param hed_model1 ML-driven model for young men
#' @param hed_model2 ML-driven model everyone except young and old men
#' @param hed_model3 ML-driven model for old men
#' @return synthetic population, with an updated HED variable
#' @keywords heavy episodic drinking, HED, machine learning
#' @export
update_hed <- function(data, hed_model1, hed_model2, hed_model3){

  # == Current model: XGBoost SIMPLE MODEL (HED ~ alc_gpd + age + sex + race + education) ==

  # store IDs with grams per day between 1 and 60 grams per day
  idx <- with(data, drinkingstatus == TRUE & alc_gpd >= 1 & alc_gpd < 60)

  # subset data set with variables for prediction
  newdata <- data[idx, c("alc_gpd", "age", "sex", "education", "race"), drop = FALSE]

  newdata$education <- as.factor(newdata$education)
  newdata$race <- as.factor(newdata$race)
  newdata$sex <- as.factor(newdata$sex)

  # prepare data for each population subset
  # young men
  newdata_youngmen <- newdata %>%
    dplyr::filter(age < 35, sex == "m")

  newdata_youngmen$sex <- NULL

  # predict on everyone other than young men, but going to only apply to women
  newdata_else <- newdata %>%
    dplyr::filter(!(sex == "m" & age < 35))

  # old men
  newdata_oldmen <- newdata %>%
    dplyr::filter(age >= 35 & sex == "m")

  newdata_oldmen$sex <- NULL

  # make predictions for young men
  pred1 <- stats::predict(hed_model1, newdata = newdata_youngmen, type = "response")
  pred_bin1 <- ifelse(pred1 > 0.5, TRUE, FALSE)

  # make predictions for ALL women
  pred2 <- stats::predict(hed_model2, newdata = newdata_else, type = "response")
  pred_bin2 <- ifelse(pred2 > 0.5, TRUE, FALSE)

  # make predictions for old men
  pred3 <- stats::predict(hed_model3, newdata = newdata_oldmen, type = "response")
  pred_bin3 <- ifelse(pred3 > 0.5, TRUE, FALSE)

  # filtering predictions from model2 to just women
  newdata_else$pred_bin2 <- pred_bin2

  pred_bin2 <- newdata_else %>%
    dplyr::filter(sex == "f")

  pred_bin2 <- pred_bin2$pred_bin2
  
  # assign predictions
  newdata$pred_bin <- NA

  women_idx <- newdata$sex == "f"
  youngmen_idx <- newdata$age < 35 & newdata$sex == "m"
  oldmen_idx <- newdata$age >= 35 & newdata$sex == "m"

  newdata$pred_bin[youngmen_idx] <- pred_bin1
  newdata$pred_bin[women_idx] <- pred_bin2
  newdata$pred_bin[oldmen_idx] <- pred_bin3

  # update stored IDs only
  data$hed_binary <- NA
  data$hed_binary[idx] <- newdata$pred_bin

  # assign 0 to those < 1 grams per day and 1 to those >= 60 grams per day
  data <-
    data %>%
    dplyr::mutate(
      hed_binary = dplyr::case_when(
        drinkingstatus == FALSE ~ FALSE,
        drinkingstatus == TRUE & alc_gpd <  1  ~ FALSE,
        drinkingstatus == TRUE & alc_gpd >= 60 ~ TRUE,
        drinkingstatus == TRUE ~ hed_binary,
        TRUE ~ FALSE
      )
    )

  return(data)
}
