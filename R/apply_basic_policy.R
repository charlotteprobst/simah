#' @title Apply basic policy
#' @description Simulates changes in alcohol consumption, in grams per day. This applies a perturbation to the
#' existing alcohol grams per day based on elasticity statistical parameters and correlation.
#' @param data a data frame containing the synthetic population, with at least columns \code{alc_cat}, \code{alc_gpd}, \code{ID}
#' @param cons_elasticity a vector containing mean own-price consumption elasticities for beer, wine, spirits
#' @param cons_elasticity_se a vector containing standard errors corresponding to consumption elasticities for beer, wine, spirits
#' @param r_sim_obs correlation between baseline consumption and individual response to price change, numeric
#' @return a data frame identical to \code{data} but with updated \code{alc_gpd} values.
#' @keywords tax policy
#' @export
apply_basic_policy <- function(data, cons_elasticity, cons_elasticity_se, r_sim_obs) {

  filtered_pop <- data %>% dplyr::filter(alc_cat != "Non-drinker")
  if(nrow(filtered_pop) == 0) {
    msg <- "The population data frame only consists of Non-drikers. Tax policy is not being processed."
    log_verbosity(msg, level = 1, type = "warn")
    return(data)
  }

  # linear association
  newGPD <- filtered_pop %>%
    dplyr::mutate(
      percentreduction = faux::rnorm_pre(
        log(alc_gpd) ^ 2,
        mu = cons_elasticity,
        sd = cons_elasticity_se,
        r = r_sim_obs,
        empirical = T
      ),
      newGPD = alc_gpd + (alc_gpd * percentreduction)
    ) %>%
    dplyr::select(ID, percentreduction, newGPD)

  # merge simulated percent reduction into data
  data_out <- merge(data, newGPD, by = "ID", all.x = T) %>%
    dplyr::mutate(alc_gpd = ifelse(alc_gpd != 0, newGPD, 0)) %>%
    dplyr::select(-c(newGPD, percentreduction))

  # Ensure that new grams per day are not sampled below zero
  data_out <- data_out %>% dplyr::mutate(alc_gpd = ifelse(alc_gpd < 0, 0, alc_gpd))

  return(data_out)
}
