#' @title Relative Risk (RR) of alcohol use disorder (AUD)
#' @description Calculates the relative risk of AUD for each individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population, with columns \code{sex}, \code{alc_gpd} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases.
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_AUD} values.
#' @keywords AUD, alcohol use disorder, RR
#' @export
assign_rr_aud <- function(data, risk_param) {

  # assign risk function parameters for AUD
  B_AUD1_MEN <- as.numeric(risk_param["B_AUD1_MEN"])
  B_AUD1_WOMEN <- as.numeric(risk_param["B_AUD1_WOMEN"])
  AUD_FORMERDRINKER_MEN <- as.numeric(risk_param["AUD_FORMERDRINKER_MEN"])
  AUD_FORMERDRINKER_WOMEN <- as.numeric(risk_param["AUD_FORMERDRINKER_WOMEN"])

  gpd_ref_m <- 122.51
  gpd_ref_f <- 114.12

  # calculate relative risk for AUD
  data <- data %>%
    dplyr::mutate(
      RR_AUD = dplyr::case_when(
        sex == "m" & alc_gpd <  gpd_ref_m ~ exp(0 + B_AUD1_MEN   * alc_gpd),
        sex == "f" & alc_gpd <  gpd_ref_f ~ exp(0 + B_AUD1_WOMEN * alc_gpd),
        sex == "m" & alc_gpd >= gpd_ref_m ~ exp(0 + B_AUD1_MEN   * gpd_ref_m),
        sex == "f" & alc_gpd >= gpd_ref_f ~ exp(0 + B_AUD1_WOMEN * gpd_ref_f),
        TRUE ~ NA  # Explicit catch-all
      ),
      RR_AUD = dplyr::case_when(
        sex == "m" & formerdrinker == 1 ~ exp(AUD_FORMERDRINKER_MEN),
        sex == "f" & formerdrinker == 1 ~ exp(AUD_FORMERDRINKER_WOMEN),
        TRUE ~ RR_AUD
      )
    )
  return(data)
}
