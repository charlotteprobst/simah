#' @title Assign alcohol grams per day to individuals previously transitioned
#' @description Assign a new \code{alc_gpd} to individuals that have changed their alcohol
#' use category in the previous step. Individuals are allocated sampled values from the beta distributions
#' corresponding to their subgroup (fitted to BRFSS data specific to sociodemographic characteristics and
#' alcohol use category).
#' @param data a data frame containing the synthetic population
#' @param catcontmodel a data frame containing the parameters of the beta distributions
#' @return a data frame identical to \code{data} with updated \code{alc_gpd}
#' @keywords microsimulation, continuous, grams per day
#' @export
allocate_gramsperday_sampled <- function(data, catcontmodel) {
  # prepare data set with the required variables to apply beta distributions
  prepdata <- data %>%
    dplyr::filter(alc_cat != "Non-drinker" & totransitioncont == 1) %>%
    dplyr::mutate(
      age_var = age,
      sex_recode = ifelse(sex == "m", "Male", "Female"),
      agecat = cut(
        age,
        breaks = c(0, 24, 64, 100),
        labels = c("18-24", "25-64", "65+")
      ),
      education = ifelse(agecat == "18-24" & education == "College", "SomeC", education),
      education_summary = education,
      race_eth = ifelse(race == "Others", "Other", race),
      group = paste(
        alc_cat,
        education_summary,
        agecat,
        race_eth,
        sex_recode,
        sep = "_"
      )
    )

  prepdata <- dplyr::left_join(prepdata, catcontmodel, by = c("group"))

  # function to assign grams per day using beta distribution parameters
  samplegpd <- function(data) {
    shape1 <- unique(data$shape1)
    shape2 <- unique(data$shape2)
    min <- unique(data$min)
    max <- unique(data$max)
    raw <- stats::rbeta(nrow(data), shape1, shape2)
    newgpd <- ((max - min + 10e-10) * raw) + (min - 10e-9)
    # order individuals by grams per day to retain the same ranking in new vs. old grams per day
    data <- data[order(data$alc_gpd), ]
    newgpd <- sort(newgpd)
    data$newgpd <- newgpd
    data$newgpd <- ifelse(data$newgpd > 200, 200, data$newgpd)
    return(data)
  }

  # apply function to sample grams per day
  prepdata <- prepdata %>% dplyr::group_by(group) %>% dplyr::do(samplegpd(.)) %>%
    dplyr::mutate(alc_gpd = newgpd) %>% dplyr::ungroup() %>%
    dplyr::select(ID, newgpd)

  # join sampled values to synthetic population
  data <- dplyr::left_join(data, prepdata, by = c("ID"))

  # assign new grams per day to individuals that have changed their alcohol use category in the previous step
  data$newgpd <-
    dplyr::case_when(
      data$alc_cat != "Non-drinker" & data$totransitioncont == 1 ~ data$newgpd,
      data$alc_cat != "Non-drinker" & data$totransitioncont == 0 ~ data$alc_gpd,
      data$alc_cat == "Non-drinker" ~ 0,
      TRUE ~ NA  # Explicit catch-all
    )

  data$alc_gpd <- data$newgpd
  data$newgpd <- NULL
  data$totransitioncont <- NULL

  return(data)
}

# This function:
# 1. Filters individuals where alc_cat != "Non-drinker" & totransitioncont == 1
# 2. Creates group variables with specific categorization: sex_recode: "m" → "Male", "f" -> "Female", agecat: [0,24,64,100] -> ["18-24", "25-64", "65+"], education conversion for 18-24 with "College" -> "SomeC", race_eth: "Others" -> "Other", group = paste(alc_cat, education_summary, agecat, race_eth, sex_recode, sep="_")
# 3. Joins with catcontmodel by group
# 4. Samples from beta distribution with parameters (shape1, shape2, min, max)
# 5. Ranks individuals by old alc_gpd and assigns sorted new values
# 6. Caps values > 200 to 200
# 7. Assigns new values only to those with totransitioncont == 1; others keep old alc_gpd
# 8. Removes intermediate columns newgpd and totransitioncont
