#' @title Relative Risk (RR) of hypertensive heart disease (HYPHD)
#' @description Calculates the relative risk of hypertensive heart disease for each individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population, with columns \code{alc_gpd} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases.
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_HYPHD} values.
#' @keywords hypertensive heart disease, HYPHD, RR
#' @export
assign_rr_hyphd <- function(data, risk_param) {

  # assign risk function parameters for HYPHD
  B_HYPHD_MEN <- as.numeric(risk_param["B_HYPHD_MEN"])
  B_HYPHD_WOMEN <- as.numeric(risk_param["B_HYPHD_WOMEN"])
  HYPHD_FORMERDRINKER <- as.numeric(risk_param["HYPHD_FORMERDRINKER"])
  # calculate relative risk for HYPHD

  gpd_ref_m <- 150
  gpd_ref_f <- 150

  data <- data %>%
    dplyr::mutate(
      RR_HYPHD = dplyr::case_when(
        sex == "m" & alc_gpd <  gpd_ref_m ~ exp(0 + B_HYPHD_MEN   * alc_gpd),
        sex == "m" & alc_gpd >= gpd_ref_m ~ exp(0 + B_HYPHD_MEN   * gpd_ref_m),
        sex == "f" & alc_gpd <  gpd_ref_f ~ exp(0 + B_HYPHD_WOMEN * alc_gpd),
        sex == "f" & alc_gpd >= gpd_ref_f ~ exp(0 + B_HYPHD_WOMEN * gpd_ref_f),
        TRUE ~ NA  # Explicit catch-all
      ),
      RR_HYPHD = ifelse(formerdrinker == TRUE, exp(HYPHD_FORMERDRINKER), RR_HYPHD)
    )
  return(data)
}
