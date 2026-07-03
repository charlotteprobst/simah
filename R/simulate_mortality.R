#' @title Assign risks for alcohol-related causes of death and simulates mortality
#' @description Assigns an individual risk for each alcohol-related cause of death that is explicitly modelled
#' (i.e., specified in \code{diseases}). Using random sampling from a uniform distribution, individuals are staged to die
#' for a given cause of death if their assigned random value falls within the respective risk thresholds. For individuals
#' staged to die, years of lost is calculated.
#' @param data a data frame containing the synthetic population, with variables for disease-specific relative
#'             risk and mortality rates for all alcohol-related causes of death specified in \code{diseases}
#' @param diseases a vector of specific causes of death that are modelled explicitly in relation to alcohol use
#' @return the synthetic population with additional variables that stage individuals to die from explicitly modelled causes of death
#' @keywords microsimulation, mortality, cause of death, risk
#' @export
simulate_mortality <- function(data, diseases) {

  if (rlang::is_empty(diseases)) {  # Returns TRUE if length is 0 or if NULL
    msg <- "The diseses vector is empty. No risks will be assigned to individuals in the population."
    log_verbosity(msg, level = 1, type = "warn")
    return(data)
  }

  data <- as.data.frame(data)

  # assign total individual risk by calculating and adding up risk for each cause of death in diseases vector
  for (i in 1:length(diseases)) {
    disease <- diseases[i]
    RR_expr <- rlang::sym(paste0('RR_', rlang::quo_name(disease)))
    rate_expr <- rlang::sym(paste0('rate_', rlang::quo_name(disease)))
    risk_expr <- rlang::sym(paste0('risk_', rlang::quo_name(disease)))
    if (i == 1) {
      # assign zero risk for AUD to lifetime abstainers
      if (disease == "AUD") {
        data <- data %>%
          dplyr::mutate(
            !!risk_expr := dplyr::if_else(drinkingstatus == FALSE & formerdrinker == FALSE,FALSE,
                                          !!RR_expr * !!rate_expr
            )
          )
      } else {
        data <- data %>%
          # calculate disease-specific risk by multiplying assigned relative risk with subgroup-specific rate
          dplyr::mutate(!!risk_expr := !!RR_expr * !!rate_expr)
      }
    } else if (i > 1) {
      # extract name of preceding (cumulative) individual risk
      prev_risk_expr <- rlang::sym(paste0("risk_", rlang::quo_name(diseases[i - 1])))
      # assign zero risk for AUD to lifetime abstainers
      if (disease == "AUD") {
        data <- data %>%
          dplyr::mutate(
            !!risk_expr := dplyr::if_else(drinkingstatus == FALSE & formerdrinker == FALSE,FALSE,
                                          !!RR_expr * !!rate_expr+!!prev_risk_expr
            )
          )
      } else {
        data <- data %>%
          dplyr::mutate(
            # calculate disease-specific risk by multiplying assigned relative risk with subgroup-specific rate
            !!risk_expr := !!RR_expr * !!rate_expr,
            # add preceding (cumulative) individual risk
            !!risk_expr := !!risk_expr+!!prev_risk_expr
          )
      }
    }

    # check maximum individual cumulative risk (should not exceed 1)
    max_risk <- max(data[, paste(risk_expr)])

    if(max_risk > 1) {
      if(any(data[[risk_expr]] > 1, na.rm = TRUE)) {
        warn_msg <- paste(risk_expr, "exceeds 1!")
        log_verbosity(warn_msg, level = 1, type = "warn")
      }
    }
  }
  # sample a random value from a uniform distribution
  # note: this process ensures that individuals are allocated to different causes of death proportionally to their risk
  data$prob <- stats::runif(nrow(data))

  for (i in 1:length(diseases)) {
    disease <- diseases[i]
    risk_expr <- rlang::sym(paste0('risk_', rlang::quo_name(disease)))
    mort_expr <- rlang::sym(paste0('mort_', rlang::quo_name(disease)))
    yll_expr <- rlang::sym(paste0('yll_', rlang::quo_name(disease)))
    if (i == 1) {
      data <- data %>%
        # stage individual death by comparing random value to stacked cause-specific mortality risks
        dplyr::mutate(
          !!mort_expr := ifelse(!!risk_expr > prob, 1, 0)
        )
    } else {
      # extract name of preceding (cumulative) individual risk
      prev_risk_expr <- rlang::sym(paste0("risk_", rlang::quo_name(diseases[i - 1])))
      data <- data %>%
        dplyr::mutate(
          # stage individual death by comparing random value to stacked cause-specific mortality risks
          !!mort_expr := ifelse(!!risk_expr > prob & !!prev_risk_expr < prob, 1, 0)
        )
    }
    data <- data %>%
      dplyr::mutate(
        # calculate years of life lost (YLL) if individual is aged below 75 years
        !!yll_expr := ifelse(!!mort_expr == 1 & age < 75, 75 - age, 0)
      )
  }

  # remove prob column from data frame
  data$prob <- NULL

  return(data)
}

# What this functions does:
# 1. Takes two parameters: data (data frame with synthetic population) and diseases (vector of cause names)
# 2. Assigns individual risks for each alcohol-related cause of death
# 3. Uses random sampling (runif) to determine who dies from which cause
# 4. Calculates years of life lost (YLL) for those who die before age 75
# 5. Returns the modified data frame with additional columns: risk_<disease>, mort_<disease>, yll_<disease> for each disease

