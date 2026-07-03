#' @title Assigns Relative Risk (RR) of diseases to a data frame
#' @description Calls the RR functions depending on the existing diseases in disease
#' @param data a data frame containing the synthetic population
#' @param diseases vector of strings that contains the names of the diseases
#' @param risk_param a data frame containing the risk function parameters for all diseases.
#' @return a data frame identical to \code{data} with an additional columns, one per disease
#' @keywords alcohol use disorder
#' @export
assign_rr <- function(data, diseases, risk_param) {

  if(is.null(diseases)) {
    return(data)
  }

  # assign disease-specific alcohol-related mortality risk
  if ("HLVDC" %in% diseases == TRUE) {
    data <- assign_rr_hlvdc(data, risk_param)
  }
  if ("LVDC" %in% diseases == TRUE) {
    data <- assign_rr_lvdc(data, risk_param)
  }
  if ("AUD" %in% diseases == TRUE) {
      data <- assign_rr_aud(data, risk_param)
  }
  if ("IJ" %in% diseases == TRUE) {
    data <- assign_rr_ij(data, risk_param)
  }
  if ("DM" %in% diseases == TRUE) {
    data <- assign_rr_dm(data, risk_param)
  }
  if ("IHD" %in% diseases == TRUE) {
    data <- assign_rr_ihd(data, risk_param)
  }
  if ("ISTR" %in% diseases == TRUE) {
    data <- assign_rr_istr(data, risk_param)
  }
  if ("HYPHD" %in% diseases == TRUE) {
    data <- assign_rr_hyphd(data, risk_param)
  }
  if ("MVACC" %in% diseases == TRUE) {
    data <- assign_rr_mvacc(data, risk_param)
  }
  if ("UIJ" %in% diseases == TRUE) {
    data <- assign_rr_uij(data, risk_param)
  }

  return(data)
}
