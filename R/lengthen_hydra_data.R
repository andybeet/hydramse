#' lengthen hydra data for additional years
#'
#'The Darwinian study utilizes Hydra but requires a longer period of time to run
#'Here we extend all o fthe variables passed to Hydra to make up the additional years
#'The first nYrs of the model run is assumed to be a period without fishing pressure
#'followed by a period (of length \code{hydraDataList$Nyrs}) of fishing.
#'
#'@param data A datafile resembling \code{hydraDataList}
#'@param nYrs The total number of years Hydra should be extended to
#'
#'@return List
#'\item{Nyrs}{Scalar. The Input value of nYrs}
#'\item{recruitmentCov}{Vector. Lengthened vector for recruitment covariate. Uses mean of current values}
#'\item{maturityCov}{Vector. Lengthened vector for maturity covariate. Uses mean of current values}
#'\item{growthCov}{Vector. Lengthened vector for growth covariate. Uses mean of current values}
#'\item{observedTemperature}{Matrix. Lengthened vector for observed temperature. Uses mean of current values}
#'\item{observedBiomass}{Matrix. Lengthened vector for observed Biomass. Uses mean of current values}
#'\item{observedCatch}{Matrix. Lengthened vector for observed Catch. Uses mean of current values}
#'\item{observedEffort}{Matrix. Lengthened vector for observed Effort. Uses 0 for fist part of time series}
#'\item{redundantRecDevs}{Matrix. Lengthened vector for observed Effort. Uses 0 for fist part of time series}
#'
#'@section Notes:
#'The 8 variables that are time dependent in the model are listed above.
#'We need to provide dummy data for the first x years during the zero fishing period
#'where we assume the model will reach some kind of equilibrium
#'@export

lengthen_hydra_data <- function(data,nYrs){
  out <- list()
  out$Nyrs <- nYrs
  # always assume Hydra is run for x years
  nYrsNofishing <- nYrs - data$Nyrs
  # the first year (1867) and last year (2016) for a 150 year run
  firstYrOfData <- head(data$observedBiomass[1,],1)
  lastYrOfData <- tail(data$observedBiomass[1,],1)
  yrs <- seq(firstYrOfData-nYrsNofishing,lastYrOfData)
  # create data for the fist x years. use mean of curren data.
  out$recruitmentCov <- matrix(c(rep(round(mean(data$recruitmentCov),2),nYrsNofishing),data$recruitmentCov),1,nYrs)
  out$maturityCov <- matrix(c(rep(round(mean(data$maturityCov),2),nYrsNofishing),data$maturityCov),1,nYrs)
  out$growthCov <- matrix(c(rep(round(mean(data$growthCov),2),nYrsNofishing),data$growthCov),1,nYrs)
  out$observedTemperature <- matrix(c(rep(mean(data$observedTemperature["temperature",]),nYrsNofishing),data$observedTemperature["temperature",]),1,nYrs)
  out$observedTemperature <- rbind(yrs,round(out$observedTemperature,2))
  rownames(out$observedTemperature) <- c("year","temperature")
  out$observedBiomass <- cbind(matrix(mean(data$observedBiomass),data$Nspecies,nYrsNofishing),data$observedBiomass[2:(data$Nspecies+1),])
  out$observedBiomass <- rbind(yrs,round(out$observedBiomass),2)
  out$observedCatch <- cbind(matrix(mean(data$observedCatch),data$Nspecies,nYrsNofishing),data$observedCatch[2:(data$Nspecies+1),])
  out$observedCatch <- rbind(yrs,round(out$observedCatch),2)

  out$observedEffort <- cbind(matrix(0,data$Nfleets,nYrsNofishing),data$observedEffort[2:(data$Nfleets+1),])
  out$observedEffort <- rbind(yrs,out$observedEffort)

  out$redundantRecDevs <- round(cbind(matrix(mean(data$redundantRecDevs),data$Nspecies,nYrsNofishing),data$redundantRecDevs),5)
  colnames(out$redundantRecDevs) <- yrs
  return(out)
}
