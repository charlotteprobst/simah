#' @title Apply taxation and pricing policies
#' @description Simulates changes drinking participation and alcohol consumption, in grams
#' per day, following a pricing change determined by supplied pricing policy parameters.
#' @param data a data frame containing the synthetic population, with at least columns \code{alc_cat}, \code{alc_gpd}, \code{ID}, \code{sex}, \code{age}, \code{education}, and \code{race}
#' @param scenario a vector containing price changes for beer, wine, spirits
#' @param setting indicates which main or sensitivity analysis is being modelled, currently either "max", "standard", or "min"
#' @param participation indicates whether drinking participation is being modelled, 0 or 1
#' @param part_elasticity participation elasticity, numeric
#' @param prob_alcohol_transitions a data frame containing transition probabilities that were calculated in previous step
#' @param cons_elasticity a vector containing mean own-price consumption elasticities for beer, wine, spirits
#' @param cons_elasticity_se a vector containing standard errors corresponding to consumption elasticities for beer, wine, spirits
#' @param r_sim_obs correlation between baseline consumption and individual response to price change, numeric
#' @param input_dir directory where NESARC beverage preference data is stored
#' @return a data frame identical to \code{data} but with updated \code{alc_gpd} values.
#' @keywords tax policy
#' @export
apply_tax_policy <- function(data, scenario, setting,
                             participation, part_elasticity, prob_alcohol_transitions,
                             cons_elasticity, cons_elasticity_se,
                             r_sim_obs,
                             input_dir = DataDirectory) {
  # ==== BEVERAGE PREFERENCE ====

  # assign beverage preference
  data <- assign_beverage_preferences(data, input_dir)

  # ==== PARTICIPATION ELASTICITY ====

  # apply participation elasticity
  if (participation == 0) {
    print("no policy effect on drinking participation")
  }

  if (participation == 1) {
    if (setting != 0) {
      print("applying participation elasticities")
    }

    # determine weighted number of drinkers to become non-drinkers
    temp <- prob_alcohol_transitions %>% dplyr::filter(alc_cat != "Non-drinker") %>%
      dplyr::select(c(
        "sex",
        "agecat",
        "race",
        "education",
        "alc_cat",
        "prob_nondrinker"
      ))

    temp2 <- data %>%
      dplyr::mutate(agecat = cut(
        age,
        breaks = c(0, 24, 64, 100),
        labels = c("18-24", "25-64", "65+")
      )) %>%
      dplyr::filter(alc_cat != "Non-drinker") %>% dplyr::left_join(.,
                                                                   temp,
                                                                   by = c("sex", "agecat", "race", "education", "alc_cat"))

    # get weighted scenario parameter
    wscenario <- temp2 %>%
      tidyr::pivot_longer(
        cols = c("beergpd", "winegpd", "liqgpd"),
        names_to = "bev",
        values_to = "bevgpd"
      ) %>%
      dplyr::group_by(bev, alc_cat) %>%
      dplyr::summarise(bevgpd = mean(bevgpd)) %>%
      dplyr::group_by(alc_cat) %>%
      dplyr::mutate(
        prop = bevgpd / sum(bevgpd),
        scenario = ifelse(
          bev %like% "beer",
          scenario[1],
          ifelse(
            bev %like% "wine",
            scenario[2],
            ifelse(bev %like% "liq", scenario[3], NA)
          )
        )
      ) %>%
      dplyr::summarise(wscenario = sum(prop * scenario))

    temp3 <- temp2 %>% dplyr::group_by(alc_cat) %>%
      # obtain average transition probability to become non-drinker by alcohol category
      dplyr::summarise(prob_quit = mean(prob_nondrinker), n = dplyr::n()) %>%
      # join with weighted scenario parameter
      dplyr::left_join(., wscenario) %>%
      # obtain number of drinkers to become non-drinkers, weighted by average transition probability
      dplyr::mutate(
        prop_alc_cat = n / sum(n),
        # get proportion of people within each alcohol category
        prop_alc = prop_alc_cat * prob_quit,
        # multiple proportion of people within each alcohol category by alc TPs
        ratio = prop_alc / min(prop_alc),
        # split up participation elasticity by calculated ratio based on population and TP by alchol category
        prop_change = (-1 * part_elasticity * wscenario) / sum(ratio) * ratio,
        # multiple this group-specific elasticity by total sample size
        tochange_by_alc_cat = round(prop_change * sum(n), 0)
      )

    # sample IDs to become non-drinkers
    tochange <- c(
      sample(x = data[data$alc_cat == "Low risk", ]$ID, size = temp3[temp3$alc_cat == "Low risk", ]$tochange_by_alc_cat),
      sample(x = data[data$alc_cat == "Medium risk", ]$ID, size = temp3[temp3$alc_cat == "Medium risk", ]$tochange_by_alc_cat),
      sample(x = data[data$alc_cat == "High risk", ]$ID, size = temp3[temp3$alc_cat == "High risk", ]$tochange_by_alc_cat)
    )

    # change alcohol category to former drinker
    data <- data %>% dplyr::mutate(
      alc_cat = ifelse(ID %in% tochange, "Non-drinker", alc_cat),
      alc_gpd = ifelse(alc_cat == "Non-drinker", 0, alc_gpd),
      drinkingstatus = ifelse(alc_cat == "Non-drinker", 0, 1)
    )
  }

  # ==== CONSUMPTION ELASTICITY====

  # generic taxation
  if (policy_int == "tax") {
    if (setting != 0) {
      print("applying generic consumption elasticities")
    }

    # linear association
    newGPD <- data %>% dplyr::filter(alc_cat != "Non-drinker") %>%
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
    data <- merge(data, newGPD, by = "ID", all.x = T) %>%
      dplyr::mutate(alc_gpd = ifelse(alc_gpd != 0, newGPD, 0)) %>%
      dplyr::select(-c(newGPD, percentreduction))

  }

  # beverage-specific taxation
  if (policy_int == "price") {
    if (setting != 0) {
      print("applying beverage-specific consumption elasticities")
    }

    # get individual-level percent reduction for price policies
    newGPD <- data %>% dplyr::filter(alc_cat != "Non-drinker") %>%
      dplyr::mutate(
        beer_percentreduction = faux::rnorm_pre(
          log(beergpd) ^ 2,
          mu = cons_elasticity[1],
          sd = cons_elasticity_se[1],
          r = r_sim_obs,
          empirical = T
        ),
        beer_percentreduction = ifelse(beer_percentreduction < -1, -1, beer_percentreduction),
        beer_newGPD = beergpd + (beergpd * beer_percentreduction *
                                   scenario[1]),
        wine_percentreduction = faux::rnorm_pre(
          log(winegpd) ^ 2,
          mu = cons_elasticity[2],
          sd = cons_elasticity_se[2],
          r = r_sim_obs,
          empirical = T
        ),
        wine_percentreduction = ifelse(wine_percentreduction < -1, -1, wine_percentreduction),
        wine_newGPD = winegpd + (winegpd * wine_percentreduction *
                                   scenario[2]),
        liq_percentreduction = faux::rnorm_pre(
          log(liqgpd) ^ 2,
          mu = cons_elasticity[3],
          sd = cons_elasticity_se[3],
          r = r_sim_obs,
          empirical = T
        ),
        liq_percentreduction = ifelse(liq_percentreduction < -1, -1, liq_percentreduction),
        liq_newGPD = liqgpd + (liqgpd * liq_percentreduction * scenario[3]),
        newGPD = beer_newGPD + wine_newGPD + liq_newGPD
      ) %>%
      dplyr::select(ID, newGPD)

    # merge simulated percent reduction into data
    data <- merge(data, newGPD, by = "ID", all.x = T) %>%
      dplyr::mutate(
        alc_gpd = ifelse(alc_gpd != 0, newGPD, 0),
        alc_gpd = ifelse(alc_gpd > 200, 200, alc_gpd)
      ) %>%
      dplyr::select(-c(newGPD, beergpd, winegpd, liqgpd))

  }

  return(data)
}
