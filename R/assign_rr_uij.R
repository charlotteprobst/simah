#' @title Relative risk (RR) of other unintentional injuries (UIJ)
#' @description Calculates the relative risk of other unintentional injuries for each
#' individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population, with columns \code{alc_gpd}, \code{hed_binary} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_UIJ} values
#' @keywords other unintentional injuries, UIJ, RR
#' @export
assign_rr_uij <- function(data, risk_param) {

  # assign risk function parameters for other unintentional injuries
  B_UIJ1 <- as.numeric(risk_param["B_UIJ1"])
  B_UIJ2 <- as.numeric(risk_param["B_UIJ2"])
  UIJ_FORMERDRINKER <- as.numeric(risk_param["UIJ_FORMERDRINKER"])

  # calculate relative risk for other unintentional injuries
  data <- data %>%
    dplyr::mutate(
      RR_UIJ = dplyr::case_when(
        alc_gpd == 0 ~ 1,
        alc_gpd <  60 & hed_binary == FALSE ~ exp(B_UIJ1 * alc_gpd),
        alc_gpd <  60 & hed_binary == TRUE  ~ exp(B_UIJ1 * alc_gpd + B_UIJ2),
        alc_gpd >= 60 & alc_gpd < 150       ~ exp(B_UIJ1 * alc_gpd + B_UIJ2),
        alc_gpd >= 150 ~ exp(B_UIJ1 * 150 + B_UIJ2),
        TRUE ~ NA  # Explicit catch-all
      ),
      RR_UIJ = ifelse(formerdrinker == TRUE , exp(UIJ_FORMERDRINKER), RR_UIJ)
      )

  return(data)
}
