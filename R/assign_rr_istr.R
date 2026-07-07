#' @title Relative Risk (RR) of ischaemic stroke (ISTR)
#' @description Calculates the relative risk of ischaemic stroke for each individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population, with columns \code{alc_gpd} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases.
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_ISTR} values.
#' @keywords ischaemic stroke, ISTR, RR
#' @export
assign_rr_istr <- function(data, risk_param) {

  # assign risk function parameters for ISTR specifically
  B_ISTR1 <- as.numeric(risk_param["B_ISTR1"])
  B_ISTR2 <- as.numeric(risk_param["B_ISTR2"])
  B_ISTR3 <- as.numeric(risk_param["B_ISTR3"])
  B_ISTR4 <- as.numeric(risk_param["B_ISTR4"])
  ISTR_FORMERDRINKER <- as.numeric(risk_param["ISTR_FORMERDRINKER"])

  # calculate relative risk for ISTR
  data <- data %>%
    dplyr::mutate(
      RR_ISTR = dplyr::case_when(
        alc_gpd == 0 ~ 1,
        alc_gpd <  12 ~ exp(B_ISTR1),
        alc_gpd <= 24 ~ exp(B_ISTR2),
        alc_gpd <= 48 ~ exp(B_ISTR3),
        alc_gpd >  48 ~ exp(B_ISTR4),
        TRUE ~ NA  # Explicit catch-all
      ),
      RR_ISTR = ifelse(formerdrinker == TRUE, exp(ISTR_FORMERDRINKER), RR_ISTR)
    )

  return(data)
}
