#' @title Apply taxation policy
#' @description Simulates changes drinking participation and alcohol consumption, in grams
#' per day, following a pricing change determined by supplied pricing policy parameters.
#' @param data a data frame containing the synthetic population, with at least columns \code{alc_cat}, \code{alc_gpd}, \code{ID}, \code{sex}, \code{age}, \code{education}, and \code{race}
#' @param scenario a vector containing price changes for beer, wine, spirits
#' @param cons_elasticity a vector containing mean own-price consumption elasticities for beer, wine, spirits
#' @param cons_elasticity_se a vector containing standard errors corresponding to consumption elasticities for beer, wine, spirits
#' @param r_sim_obs correlation between baseline consumption and individual response to price change, numeric
#' @return a data frame identical to \code{data} but with updated \code{alc_gpd} values.
#' @keywords tax policy
#' @export
apply_tax_policy <- function(data, scenario, cons_elasticity, cons_elasticity_se, r_sim_obs) {

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
      newGPD = alc_gpd + (alc_gpd * percentreduction * scenario)
    ) %>%
    dplyr::select(ID, percentreduction, newGPD)

  # merge simulated percent reduction into data
  data_out <- merge(data, newGPD, by = "ID", all.x = T) %>%
    dplyr::mutate(alc_gpd = ifelse(alc_gpd != 0, newGPD, 0)) %>%
    dplyr::select(-c(newGPD, percentreduction))

  return(data_out)
}
