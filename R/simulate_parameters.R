#' simulate all unknowns in the MSE
#'
#' Currently only simulate stock recruitment parameters and other food. See Method section below for more details
#'
#' @param stockRecruitParams Data set similar to lazily loaded darwinData dataset
#' @param simulationRules Set of rules set up similar to lazily loaded darwinRules dataset
#' @param SRFunctionChoice Numeric Vector. Whether the Stock recruitment function for each species should be piecewise model (hockey, generalized,=  0) or shepherd (1) or a random choice of either (Default = NULL)
#'
#' @return List
#' \item{otherFood}{simulated value of otherFood variable}
#' \item{alphaShepherd}{simulated value of alpha for the shepherd model  }
#' \item{betaShepherd}{simulated value of beta for the shepherd model}
#' \item{shapeShepherd}{value of the shape parameter. Predefined by user in darwinRules$SRType}
#' \item{alphaHockey}{simulated value of alpha (slope) for the hockey stick model}
#' \item{betaHockey}{simulated value of beta (-alpha) for the hockey stick model}
#' \item{shapeHockey}{simulated value of the breakpoint for the hockey stick model}
#' \item{alphaSegmented}{simulated value of alpha (slope) for the generalized segmented model}
#' \item{betaSegmented}{simulated value of beta (change in the slope) for the generalized segmented model}
#' \item{shapeSegmented}{simulated value of breakpoint for the generalized segmented model}
#' \item{recSigma}{Values of sigma to be used in Hydra for each species/stock recruitment function chosen}
#' \item{recType}{Denotes the form of the SR model used in Hydra. see field \code{recType} in \code{hydradata::hydraDataList} }
#'
#' @section Method:
#' Methods and \code{darwinRules} dataset are described in the vignette \code{simulation_documentation}
#' The data set \code{darwinData} is defined in \code{?darwinData} and explained futher in the vignette \code{darwinData_explained}
#'
#' @export

simulate_parameters <- function(stockRecruitParams=darwinData,simulationRules=darwinRules,SRFunctionChoice=NULL){
  nSpecies <- length(simulationRules$SRType)
  simulatedValues <- list()
  simValues <- list()

  if (is.null(SRFunctionChoice)){ # ramdon sample
    SRFunctionChoice <- sample(c(0,1),nSpecies,replace=TRUE)
  } else {
    if ((length(SRFunctionChoice) != nSpecies) | (!all(SRFunctionChoice %in% c(0,1)))){
      stop(paste0("SRFunctionChoice must be a vector (of 0's and 1's) of length = ",nSpecies))
    }
  }
  # otherfood
  otherFoodChoices <- simulationRules$otherFood
  simulatedValues$otherFood <-  sample(otherFoodChoices,1)

  shepherd <- vector(mode="numeric",length=nSpecies) # allocate for shepherd SR function

  # simulate from a shepherd SR function based on ricker shape and beverton holt shape
  simValues$Shepherd <- simulate_shepherd_SR(stockRecruitParams$Ricker,"ricker",simulationRules$RickerRangeOfAlphas,nSpecies)
  bh <- simulate_shepherd_SR(stockRecruitParams$BH,"bh",simulationRules$BHRangeOfAlphas,nSpecies)
  # assign values based on whether a species is defined as overcompensatory or not. If over compensatory do nothing otherwise use BH fit
  indBH <- simulationRules$SRType == 1 # not overcompensatory
  simValues$Shepherd$alpha[indBH] <- bh$alpha[indBH]
  simValues$Shepherd$beta[indBH] <- bh$beta[indBH]
  simValues$Shepherd$shape[indBH] <- bh$shape[indBH]

  simValues$Hockey <- simulate_hockey_SR(stockRecruitParams$hockey,stockRecruitParams$SSB,simulationRules$rangeOfBreakpoint,nSpecies)
  simValues$Segmented <- simulate_segmented_SR(stockRecruitParams$segmented,stockRecruitParams$SSB,simulationRules$rangeOfBreakpoint,nSpecies)

  # rename list fields to be consistent with HydraData
  shepherdList <- rename_variables(simValues,"Shepherd")
  segmentedList <- rename_variables(simValues,"Segmented")
  hockeyList <- rename_variables(simValues,"Hockey")

  # use the values of sigma for respective fits. If choose BH then use sigma associated with beverton holt fit
  recSigma <- vector(mode="numeric",length=nSpecies)
  indS <- (simulationRules$SRType == 2) & (SRFunctionChoice == 0)
  recSigma[indS] <- stockRecruitParams$segmented$sigma[indS]
  indR <- (simulationRules$SRType == 2) & (SRFunctionChoice == 1)
  recSigma[indR] <- stockRecruitParams$Ricker$sigma[indR]
  indH <- (simulationRules$SRType == 1) & (SRFunctionChoice == 0)
  recSigma[indH] <- stockRecruitParams$hockey$sigma[indH]
  indB <- (simulationRules$SRType == 1) & (SRFunctionChoice == 1)
  recSigma[indB] <- stockRecruitParams$BH$sigma[indB]

  # assign the approriate flag to recType. Hydra uses this to simulate from a defined SR function
  # 6 = shepher, 7 = hockey, 8 = segmented
  recType <- SRFunctionChoice * 6 # set all 1's to shepherd
  recType[(SRFunctionChoice==0) & (simulationRules$SRType == 2)] <- 8 # set some 0's to 8
  recType[(SRFunctionChoice==0) & (simulationRules$SRType == 1)] <- 7 # set some 0's to 7

  # combine fields
  simulatedValues <- c(simulatedValues,shepherdList,segmentedList,hockeyList)
  simulatedValues$recSigma <- recSigma
  simulatedValues$recType <- recType

  return(simulatedValues)
}



##################### sub functions ####################
# rename variables to match hydraData
rename_variables <- function(variableNm,fieldName){
  newName <- list()

  newName[[paste0("alpha",fieldName)]] <- variableNm[[fieldName]]$alpha
  newName[[paste0("beta",fieldName)]] <- variableNm[[fieldName]]$beta
  newName[[paste0("shape",fieldName)]] <- variableNm[[fieldName]]$shape

  return(newName)

}


#
simulate_shepherd_SR <- function(estimatedParams,form,rangeOfAlphas,nSpecies){
  params <- list()
  alpha_l <- rangeOfAlphas[1]
  alpha_u <- rangeOfAlphas[2]
  if (tolower(form) == "bh") {
    # asymptote  = alpha * beta based on shepherd parameterization. see doc
    params$alpha <- runif(nSpecies,alpha_l*estimatedParams$alpha,alpha_u*estimatedParams$alpha)
    params$beta <- estimatedParams$alpha*estimatedParams$beta/params$alpha
    params$shape <- c(1,1,1,1,1,1,1,1,1,1)

  } else if (tolower(form) == "ricker"){
    # we keep the peak at [Sp = 1/sqrt(b), Rp =a/(2sqrt(b))]
    params$alpha <- runif(nSpecies,alpha_l*estimatedParams$alpha,alpha_u*estimatedParams$alpha)
    #params$beta <- estimatedParams$beta*(estimatedParams$alpha^2)/ (params$alpha^2)
    params$beta <- estimatedParams$alpha*estimatedParams$beta/params$alpha
    params$shape <- c(2,2,2,2,2,2,2,2,2,2)
  }

  return(params)
}

simulate_hockey_SR <- function(estimatedParams,SSB,breakpointRule,nSpecies){
  params <- list()

  params$shape <- runif(nSpecies,breakpointRule[1]*SSB$min_obs_SS,breakpointRule[2]*estimatedParams$shape)
  params$alpha <- estimatedParams$alpha*estimatedParams$shape/params$shape
  params$beta <- -params$alpha
  return(params)

}
simulate_segmented_SR <- function(estimatedParams,SSB,breakpointRule,nSpecies){
  params <- list()
  betaSim <- vector(mode="numeric",length=nSpecies)

  # new breakpoint is in interval [% (50)minObsSSB , % (200)fittedShape]
  params$shape <- runif(nSpecies,breakpointRule[1]*SSB$min_obs_SS,breakpointRule[2]*estimatedParams$shape)
  params$alpha <- estimatedParams$alpha*estimatedParams$shape/params$shape

  rbeta <- estimatedParams$beta
  ralpha <- estimatedParams$alpha
  Smax <- SSB$max_obs_SS
  rbreakpoint <- estimatedParams$shape

  for(isp in 1:nSpecies) {
    rbeta <- estimatedParams$beta[isp]
    ralpha <- estimatedParams$alpha[isp]
    Smax <- SSB$max_obs_SS[isp]
    rbreakpoint <- estimatedParams$shape[isp]

    if (rbeta > 0) { # fited slope increasing after breakpoint. Can't allow this
      # we take mirror image using hockeystick model as mirror
      RSmax <- ralpha*Smax + ((-2*ralpha)-rbeta)*(Smax-rbreakpoint)
      newSlope <- (RSmax-ralpha*rbreakpoint)/(Smax-rbreakpoint)
      betaSim[isp] <- newSlope-params$alpha[isp]

    } else if ((rbeta < 0) & (ralpha > abs(rbeta))) { # beta > -alpha but beta <0. Slope still positive
      RSmax <- ralpha*Smax + (-ralpha- abs(abs(ralpha)-abs(rbeta)))*(Smax-rbreakpoint)
      newSlope <- (RSmax- params$alpha[isp]*params$shape[isp])/(Smax-params$shape[isp])
      betaSim[isp] <- newSlope-params$alpha[isp]

    } else { ## beta < -alpha ricker type
      RSmax <- ralpha*Smax + rbeta*(Smax-rbreakpoint)
      newSlope <- (RSmax- params$alpha[isp]*params$shape[isp])/(Smax-params$shape[isp])
      betaSim[isp] <- newSlope-params$alpha[isp]
    }
  }
  params$beta <- betaSim

  return(params)
}



