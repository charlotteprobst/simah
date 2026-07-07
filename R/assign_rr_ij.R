#' @title Relative Risk (RR) of intentional injuries (IJ)
#' @description Calculates the relative risk of IJ for each individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population, with columns \code{sex}, \code{alc_gpd} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases.
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_IJ} values.
#' @keywords suicide, intentional injuries, RR
#' @export
assign_rr_ij <- function(data, risk_param) {

  # assign risk function parameters for intentional injuries
  B_IJ_MEN <- as.numeric(risk_param["B_IJ_MEN"])
  B_IJ_WOMEN <- as.numeric(risk_param["B_IJ_WOMEN"])
  IJ_FORMERDRINKER_MEN <- as.numeric(risk_param["IJ_FORMERDRINKER_MEN"])
  IJ_FORMERDRINKER_WOMEN <- as.numeric(risk_param["IJ_FORMERDRINKER_WOMEN"])
  # calculate relative risk for intentional injuries

  gpd_ref_m <- 100
  gpd_ref_f <- 50

  data <- data %>%
    dplyr::mutate(
      RR_IJ = dplyr::case_when(
        sex == "m" & alc_gpd <  gpd_ref_m ~ exp(0 + B_IJ_MEN   * alc_gpd),
        sex == "f" & alc_gpd <  gpd_ref_f ~ exp(0 + B_IJ_WOMEN * alc_gpd),
        sex == "m" & alc_gpd >= gpd_ref_m ~ exp(0 + B_IJ_MEN   * gpd_ref_m),
        sex == "f" & alc_gpd >= gpd_ref_f ~ exp(0 + B_IJ_WOMEN * gpd_ref_f),
        TRUE ~ NA  # Explicit catch-all
      ),
      RR_IJ = dplyr::case_when(
        sex == "m" & formerdrinker == 1 ~ exp(IJ_FORMERDRINKER_MEN),
        sex == "f" & formerdrinker == 1 ~ exp(IJ_FORMERDRINKER_WOMEN),
        TRUE ~ RR_IJ
      )
    )
  return(data)
}
