#' @title Relative Risk (RR) of motor vehicle injuries (MVACC)
#' @description Calculates the relative risk of motor vehicle injuries for each individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population, with columns \code{alc_gpd}, \code{hed_binary} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases.
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_MVACC} values.
#' @keywords motor vehicle injuries, MVACC, RR
#' @export
assign_rr_mvacc <- function(data, risk_param) {

  # assign risk function parameters for MVACC
  B_MVACC1 <- as.numeric(risk_param["B_MVACC1"])
  B_MVACC2 <- as.numeric(risk_param["B_MVACC2"])
  MVACC_FORMERDRINKER <- as.numeric(risk_param["MVACC_FORMERDRINKER"])

  # calculate relative risk for MVACC
  data <- data %>%
    dplyr::mutate(
      RR_MVACC = dplyr::case_when(
        alc_gpd == 0 ~ 1,
        alc_gpd <  60 & hed_binary == FALSE ~ exp(B_MVACC1 * alc_gpd),
        alc_gpd <  60 & hed_binary == TRUE  ~ exp(B_MVACC1 * alc_gpd + B_MVACC2),
        alc_gpd >= 60 & alc_gpd < 150   ~ exp(B_MVACC1 * alc_gpd + B_MVACC2),
        alc_gpd >= 150                  ~ exp(B_MVACC1 * 150 + B_MVACC2),
        TRUE ~ NA  # Explicit catch-all
      ),
      RR_MVACC = ifelse(formerdrinker == TRUE, exp(MVACC_FORMERDRINKER), RR_MVACC)
    )

  return(data)
}
