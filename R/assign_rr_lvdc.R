#' @title Relative risk (RR) of liver cirrhosis through "main pathway"
#' @description Calculates the relative risk of liver cirrhosis through the main pathway for each
#' individual in the synthetic population, based on the estimated risk function.
#' The "main pathway" refers to all liver disease and cirrhosis that is causally linked to alcohol use except for liver cirrhosis caused by hepatitis C,
#' which is modelled through the hepatitis pathway.
#' @param data a data frame containing the synthetic population, with columns \code{sex}, \code{alc_gpd} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_LVDC} values
#' @keywords liver cirrhosis, LVDC, main pathway, RR
#' @export
assign_rr_lvdc <- function(data, risk_param) {

  # assign risk function parameters for liver cirrhosis through main pathway
  B_LVDC1_MEN <- as.numeric(risk_param["B_LVDC1_MEN"])
  B_LVDC2_MEN <- as.numeric(risk_param["B_LVDC2_MEN"])
  B_LVDC1_WOMEN <- as.numeric(risk_param["B_LVDC1_WOMEN"])
  B_LVDC2_WOMEN <- as.numeric(risk_param["B_LVDC2_WOMEN"])
  LVDC_FORMERDRINKER <- as.numeric(risk_param["LVDC_FORMERDRINKER"])

  gpd_ref_m <- 179.44
  gpd_ref_f <- 94.15

  # calculate relative risk for liver cirrhosis through main pathway
  data <- data %>%
    dplyr::mutate(
      RR_LVDC = dplyr::case_when(
        sex == "m" & alc_gpd <  gpd_ref_m ~ exp(0 + B_LVDC1_MEN   * alc_gpd + B_LVDC2_MEN   * (alc_gpd ^ 2)),
        sex == "f" & alc_gpd <  gpd_ref_f ~ exp(0 + B_LVDC1_WOMEN * alc_gpd + B_LVDC2_WOMEN * (alc_gpd ^ 2)),
        sex == "m" & alc_gpd >= gpd_ref_m ~ exp(0 + B_LVDC1_MEN   * gpd_ref_m   +  B_LVDC2_MEN  * (gpd_ref_m ^ 2)),
        sex == "f" & alc_gpd >= gpd_ref_f ~ exp(0 + B_LVDC1_WOMEN * gpd_ref_f   + B_LVDC2_WOMEN * (gpd_ref_f ^ 2)),
        TRUE ~ NA  # Explicit catch-all
      ),
      RR_LVDC = ifelse(formerdrinker == 1, exp(LVDC_FORMERDRINKER), RR_LVDC)
    )

  return(data)
}
