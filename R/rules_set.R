# functions to evaluate whether simulation passes test.
# The test being are the simulations withing the bounds of historic observations

#' Assess if model passes 1st biomass rule
#'
#' Check to ensure all species population are above a historic minimum
#' This minimum is the minimum of survey data and multispecies model output
#'
#' @param biomass matrix. (nSpecies x nYrs) average biomass (avByr variable)
#' @param nYrsFishing scalar. Number of fishing years simulated
#' @param historicBounds matrix. (nSpecies x ??) from lazy data \code{darwinData}
#' @param simulationRules list. adapted from lazy data  \code{darwinRules}
#'
#' @return List
#' \item{lowerBoolean}{boolean vector (nSpecies). Denotes which species populations were above historic lower bounds.}
#' \item{pass}{boolean value. Indicates whether all species passed the test}
#'
#' @importFrom magrittr %>%
#'
#' @export

# check to see if all within biomass bounds after a number of years without fishing
rule1_biomass <- function(biomass,nYrsFishing,historicBounds,simulationRules) {
  if (simulationRules$biomassRule == F){
    return(pass <- F)
  }

  nSpecies <- length(unique(biomass$Species))
  nYrs <- dim(biomass)[1]/nSpecies
  nYrsNofishing <- nYrs-nYrsFishing

  # calculate the mean of the biomass in the last n years of no fishing
  noFishingBiomass <- biomass %>%
    dplyr::filter(Year > nYrsNofishing-simulationRules$lastNPoints & Year <= nYrsNofishing) %>%
    dplyr::group_by(Species) %>%
    dplyr::summarize(mean=mean(Biomass))
  #noFishingBiomass <- rowMeans(biomass[,(nYrsNofishing-simulationRules$lastNPoints+1):nYrsNofishing])

  # find the minuimum allowable level for this species
  threshold1lower <- pmin(historicBounds$min_survey,historicBounds$CurtiThesis_minBio,na.rm=T)

  rule1lower <- noFishingBiomass$mean > threshold1lower

  if (!all(rule1lower)) {
    pass <- F
  } else {
    pass <- T
  }

  return(rule1 <- list(pass = pass,lowerBoolean = rule1lower))

}

#' Assess if model passes 2nd biomass rule.
#'
#' After a period of fishing pressure the species population are checked to ensure they fall withing historic bounds
#'
#' @param biomass matrix. (nSpecies x nYrs) average biomass (avByr variable)
#' @param historicBounds matrix. (nSpecies x ??) from lazy data \code{darwinData}
#' @param simulationRules list. adapted from lazy data  \code{darwinRules}
#'
#' @return List
#' \item{lowerBoolean}{boolean vector (nSpecies). Denotes which species populations were above historic lower bounds.}
#' \item{upperBoolean}{boolean vector (nSpecies). Denotes which species populations were below historic upper bounds.}
#' \item{pass}{boolean value. Indicates whether all species passed the test}

#' @export

rule2_biomass <- function(biomass,historicBounds,simulationRules) {
  if (simulationRules$biomassRule == F){
    return(pass <- F)
  }

  nSpecies <- length(unique(biomass$Species))
  nYrs <- dim(biomass)[1]/nSpecies

  fishingBiomass <- biomass %>%
    dplyr::filter(Year > nYrs-simulationRules$lastNPoints & Year <= nYrs) %>%
    dplyr::group_by(Species) %>%
    dplyr::summarize(mean=mean(Biomass))

  #fishingBiomass <- rowMeans(biomass[,(nYrs-simulationRules$lastNPoints+1):nYrs])

  # established the upper and lower bounds based on data in darwinData
  threshold2lower <- simulationRules$biomassBounds[1]*pmin(historicBounds$min_survey,historicBounds$CurtiThesis_minBio,na.rm=T)
  threshold2upper <- simulationRules$biomassBounds[2]*pmax(historicBounds$max_survey,historicBounds$CurtiThesis_maxBio,na.rm=T)

  # evaluates the rule
  rule2upper <- (fishingBiomass$mean < threshold2upper)
  rule2lower <-  (fishingBiomass$mean > threshold2lower)


  if (rule2lower%*%rule2upper != nSpecies) {
    pass <- F
  } else {
    pass <- T
  }

  return(rule2 <- list(pass = pass,lowerBoolean = rule2lower,upperBoolean = rule2upper))

}

#' Assess if model passes catch rule.
#'
#' After a period of fishing pressure the catch of each species are checked to ensure they fall withing historic bounds
#'
#' @param biomass matrix. (nSpecies x nYrs) average biomass (avByr variable)
#' @param historicBounds matrix. (nSpecies x ??) from lazy data \code{darwinData}
#' @param simulationRules list. adapted from lazy data  \code{darwinRules}
#'
#' @return List
#' \item{lowerBoolean}{boolean vector (nSpecies). Denotes which species populations were above historic lower bounds.}
#' \item{upperBoolean}{boolean vector (nSpecies). Denotes which species populations were below historic upper bounds.}
#' \item{pass}{boolean value. Indicates whether all species passed the test}
#'
#' @export

rule3_landings <- function(catch,historicBounds,simulationRules) {
  if (simulationRules$catchRule == F){
    return(pass <- F)
  }
  nSpecies <- length(unique(catch$Species))
  nYrs <- dim(catch)[1]/nSpecies

  fishingCatch <- catch %>%
    dplyr::filter(Year > nYrs-simulationRules$lastNPoints & Year <= nYrs) %>%
    dplyr::group_by(Species) %>%
    dplyr::summarize(mean=mean(Catch))

  #fishingCatch <- rowMeans(catch[,(nYrs-simulationRules$lastNPoints+1):nYrs])

  # established the upper and lower bounds based on data in darwinData
  threshold3lower <- simulationRules$catchBounds[1]*historicBounds$minLandings
  threshold3upper <- simulationRules$catchBounds[2]*historicBounds$maxLandings
  #evaluate the rules
  rule3upper <- (fishingCatch$mean < threshold3upper)
  rule3lower <-  (fishingCatch$mean > threshold3lower)

  if (rule3lower%*%rule3upper != nSpecies) {
    pass <- F
  } else {
    pass <- T
  }

  return(rule3 <- list(pass = pass,lowerBoolean = rule3lower,upperBoolean = rule3upper))

}
