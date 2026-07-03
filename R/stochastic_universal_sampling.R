#' @title Stochastic universal sampling
#' @description
#' This function performs selection by using the Stochastic Universal Sampling (SUS) approach
#' @param fitness a vector containing the fitness of the individuals in the population
#' @param nselect the desired number of individuals to be selected
#' @return a vector with the IDs of the individuals to be removed
#' @export
#' @author
#' The original Matlab implementation of the SUS was written by Hartmut Pohlheim and
#' Carlos Fonseca. The R implementation was written by David Zhao, and this is an
#' adapted version for the simah package.

stochastic_universal_sampling <- function(fitness, nselect) {
  dsize <- NROW(fitness)

  cumfit <- cumsum(fitness)
  trials <- cumfit[dsize]/nselect * (runif(1) + 0:(nselect - 1))
  selectedIds <- rep(0, nselect)
  for(i in 1:nselect) {
    for(j in 1:dsize) {
      if (trials[i] < cumfit[j]) {
        selectedIds[i] <- j
        break
      }
    }
  }
  return(selectedIds)
}
