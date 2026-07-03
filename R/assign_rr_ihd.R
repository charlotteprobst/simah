#' @title Relative risk (RR) of ischemic heart disease (IHD)
#' @description Calculates the relative risk of ischemic heart disease for each
#' individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population, with columns \code{alc_gpd} and \code{formerdrinker}
#' @param risk_param a data frame containing the risk function parameters for all diseases
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_IHD} values
#' @keywords ischemic heart disease, ischaemic heart disease, IHD, RR
#' @export
assign_rr_ihd <- function(data, risk_param) {

  # assign risk function parameters for ischemic heart disease
  B_IHD1_MEN <- as.numeric(risk_param["B_IHD1_MEN"])
  B_IHD2_MEN <- as.numeric(risk_param["B_IHD2_MEN"])
  B_IHD3_MEN <- as.numeric(risk_param["B_IHD3_MEN"])
  B_IHD4_MEN <- as.numeric(risk_param["B_IHD4_MEN"])
  B_IHD5_MEN <- as.numeric(risk_param["B_IHD5_MEN"])
  B_IHD1_WOMEN <- as.numeric(risk_param["B_IHD1_WOMEN"])
  B_IHD2_WOMEN <- as.numeric(risk_param["B_IHD2_WOMEN"])
  B_IHD3_WOMEN <- as.numeric(risk_param["B_IHD3_WOMEN"])
  B_IHD4_WOMEN <- as.numeric(risk_param["B_IHD4_WOMEN"])
  B_IHD5_WOMEN <- as.numeric(risk_param["B_IHD5_WOMEN"])
  IHD_FORMERDRINKER_MEN <- as.numeric(risk_param["IHD_FORMERDRINKER_MEN"])
  IHD_FORMERDRINKER_WOMEN <- as.numeric(risk_param["IHD_FORMERDRINKER_WOMEN"])

  # calculate relative risk for ischemic heart disease
  data <- data %>%
    dplyr::mutate(
      RR_IHD = dplyr::case_when(
        ## abstainers
        alc_gpd == 0 ~ 1,

        ## men
        sex == "m" & alc_gpd < 1.3                     ~ exp(B_IHD1_MEN),
        sex == "m" & alc_gpd >= 1.3 & alc_gpd < 25     ~ exp(B_IHD2_MEN),
        sex == "m" & alc_gpd >= 25   & alc_gpd < 45    ~ exp(B_IHD3_MEN),
        sex == "m" & alc_gpd >= 45   & alc_gpd < 65    ~ exp(B_IHD4_MEN),
        sex == "m" & alc_gpd >= 65                     ~ exp(B_IHD5_MEN),

        ## women
        sex == "f" & alc_gpd < 1.3                     ~ exp(B_IHD1_WOMEN),
        sex == "f" & alc_gpd >= 1.3 & alc_gpd < 25     ~ exp(B_IHD2_WOMEN),
        sex == "f" & alc_gpd >= 25   & alc_gpd < 45    ~ exp(B_IHD3_WOMEN),
        sex == "f" & alc_gpd >= 45   & alc_gpd < 65    ~ exp(B_IHD4_WOMEN),
        sex == "f" & alc_gpd >= 65                     ~ exp(B_IHD5_WOMEN),

        ## anything else
        TRUE                                           ~ NA
      ),

      ## override for former drinkers
      RR_IHD = dplyr::case_when(
        formerdrinker == TRUE & sex == "m" ~ exp(IHD_FORMERDRINKER_MEN),
        formerdrinker == TRUE & sex == "f" ~ exp(IHD_FORMERDRINKER_WOMEN),
        TRUE ~ RR_IHD
      )
    )

  return(data)
}
