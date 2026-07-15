#' @title Probability-proportional-to-size sampling without replacement (ppswor)
#' @description
#' ppswor is referred to as \eqn{\pi}ps shares some similarities with stochastic universal sampling (SUS),
#' that is, it maintain a low selection variance (or good spread), while ensuring that n unique individuals
#' are chosen. In SUS, if an individual has an exceptionally high fitness score, then it can happen that it
#' gets picked more than once, implying that the total number of unique individuals is less than the desirable
#' number n. ppswor addresses this by identifying individuals with a dominant fitness, and these are
#' automatically selected. This is done in a systematic way until there are no dominant individuals in the
#' existing pool. See examples in https://dickbrus.github.io/SpatialSamplingwithR/pps.html
#' @param fitness a vector containing the fitness of the individuals in the population
#' @param n the desired number of individuals to be selected
#' @return a vector with the IDs of the individuals to be removed
#' @export
ppswor <- function(fitness, n) {
  pi <- sampling::inclusionprobabilities(fitness, n)
  cumsumpi <- c(0, cumsum(pi))
  start <- runif(1, min = 0, max = 1)
  sys <- 0:(n- 1) +  start
  units <- findInterval(sys, cumsumpi)
  return(units)
}
