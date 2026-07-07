#' @title Relative Risk (RR) of liver cirrhosis through hepatitis pathway
#' @description Calculates the relative risk of liver cirrhosis through the hepatitis pathway for each individual in the synthetic population, based on the estimated risk function.
#' @param data a data frame containing the synthetic population with column \code{alc_gpd}
#' @param risk_param a data frame containing the risk function parameters for all diseases.
#' @return a data frame identical to \code{data} with an additional column containing \code{RR_HLVDC} values.
#' @keywords cirrhosis hepatitis pathway, RR
#' @export
assign_rr_hlvdc <- function(data, risk_param) {

  # assign risk function parameters for liver cirrhosis through hepatitis pathway specifically
  B_HLVDC1 <- as.numeric(risk_param["B_HLVDC1"])
  B_HLVDC2 <- as.numeric(risk_param["B_HLVDC2"])
  # calculate relative risk
  gpd_ref <- 146.3
  data$RR_HLVDC <- ifelse(
    data$alc_gpd < gpd_ref,
    exp(0 + B_HLVDC1 * data$alc_gpd + B_HLVDC2 * (data$alc_gpd ^ 2)),
    exp(0 + B_HLVDC1 * gpd_ref + B_HLVDC2 * (gpd_ref^2))
  )

  return(data)
}
