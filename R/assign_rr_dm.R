#' @title Relative risk (RR) of diabetes mellitus type 2
#' @description Calculates the relative risk of diabetes mellitus type 2 for each
#' individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population, with columns \code{sex}, \code{alc_gpd} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_DM} values
#' @keywords diabetes mellitus, DM, RR
#' @export
assign_rr_dm <- function(data, risk_param) {

  # assign risk function parameters for diabetes mellitus type 2
  B_DM_MEN <- as.numeric(risk_param["B_DM_MEN"])
  B_DM1_WOMEN <- as.numeric(risk_param["B_DM1_WOMEN"])
  B_DM2_WOMEN <- as.numeric(risk_param["B_DM2_WOMEN"])
  B_DM3_WOMEN <- as.numeric(risk_param["B_DM3_WOMEN"])
  DM_FORMERDRINKER_MEN <- as.numeric(risk_param["DM_FORMERDRINKER_MEN"])
  DM_FORMERDRINKER_WOMEN <- as.numeric(risk_param["DM_FORMERDRINKER_WOMEN"])

  female_func <- function(alc_gpd) {
    mexp <-
      0 + B_DM1_WOMEN * alc_gpd
        + B_DM2_WOMEN * (pmax((alc_gpd - 1) / 13.36865, 0) ^ 3 + ((22.3 - 1) * pmax((alc_gpd - 49.9) / 13.36865, 0) ^ 3 - (49.9 - 1) * (pmax((alc_gpd - 22.3) / 13.36865, 0) ^ 3))  / (49.9 - 22.3))
        + B_DM3_WOMEN * (pmax((alc_gpd - 9.4) / 13.36865, 0) ^ 3 + ((22.3 - 9.4) * pmax((alc_gpd - 49.9) / 13.36865, 0) ^ 3 - (49.9 - 9.4) * (pmax((alc_gpd - 22.3) / 13.36865, 0) ^ 3))  / (49.9 - 22.3))
    return(mexp)
  }

  gpd_ref_m <- 100
  gpd_ref_f <- 100

  # calculate relative risk for diabetes mellitus type 2
  data <- data %>%
    dplyr::mutate(
      RR_DM = dplyr::case_when(
        sex == "m" & alc_gpd <  gpd_ref_m ~ exp(0 + B_DM_MEN * alc_gpd),
        sex == "m" & alc_gpd >= gpd_ref_m ~ exp(0 + B_DM_MEN * gpd_ref_m),
        sex == "f" & alc_gpd <  gpd_ref_f ~ exp(female_func(alc_gpd)),
        sex == "f" & alc_gpd >= gpd_ref_f ~ exp(female_func(gpd_ref_f)),
        TRUE ~ NA  # Explicit catch-all
      ),
      RR_DM = dplyr::case_when(
        sex == "m" & formerdrinker == TRUE ~ exp(DM_FORMERDRINKER_MEN),
        sex == "f" & formerdrinker == TRUE ~ exp(DM_FORMERDRINKER_WOMEN),
        TRUE ~ RR_DM
      ),
      RR_DM = pmin(RR_DM, 30)
    )
  return(data)
}
