#' Darwinian simulation study
#'
#' Simulates parameters, runs hydra, then determins if simulated parameter set is viable. Essentially filtering out poor simulations
#' Viable sets are saved as rds files in a predetermined folder
#'
#' @param nYrs Numeric scalar. Total length of simulation. See \code{Run length} section below for details
#' @param hydraD List of current base data (as in \code{hydradata::hydradataList})
#' @param stockRecruitData List. Stock recruitment parameter estimates for differnt functional forms (\code{\link{darwinData}})
#' @param simulationRules List. Set of rules used in the simulation of parameters (\code{\link{darwinRules}})
#' @param nSims Numeric scalar. Number of parameter sets to simulate
#' @param SRFunctionChoice Numeric Vector. An argument of \code{\link{simulate_parameters}}
#' @param stochasticity Boolean scalar. TRUE if stochasiticity is desired (stock recruitment functions in Hydra)
#' @param inputOptions List. Set of input values to determin run type
#' @param pathToTPL String. Path to location of Hydra executable
#' @param hydraVersion String. Name of the Hydra executable
#' @param boolPlot. Boolean. True if plotting of biomass and catch are required. Default = FALSE
#'
#' @return List
#' \item{nSuccesses}{Number of simulated parameter sets that pass the biomass and catch criteria. These successful sets will be saved as rds files in the \code{successfulSims}
#' folder, located in the project root}
#' \item{nAttempts}{Total Number of simulated parameter sets}
#'
#' @section Run length:
#'
#' Hydra is currently set up to run for 53 years (1964-2016). This is the historic period in which fishing data is available. To extend the run length to \code{nYrs}
#' is adding a period of no fishing of length \code{nYrs}-53 years to the start of the model run. It is assumed that all populations have reached
#' an equilibrium. This equilibrium is then used as the starting point for the histric portion of the model run.
#'
#' @export

darwin <- function(nYrs,hydraD,stockRecruitData,simulationRules,nSims,SRFunctionChoice,stochasticity=F,inputOptions,pathToTPL,hydraVersion,boolPlot=F){
  # create folders for storing temporary files and saved output
  nYrsFishing <- hydradata::hydraDataList$Nyrs
  outDirForDatPin <- here::here("darwin")
  outPath <- here::here("successfulSims")
  if(!dir.exists(outDirForDatPin)){dir.create(outDirForDatPin)} # folder for dumping hydra output prior to analysis
  if(!dir.exists(outPath)){dir.create(outPath)} # folder for dumping hydra output prior to analysis

  # create extended time series info for study longer than 53 yrs.
  # We need to include a nofishing scenario prior to fishing
  noFishingData <- hydramse::lengthen_hydra_data(hydraD,nYrs)

  ic <- 0
  is <- 0
  print("Running Darwinian process ...")
  #while(ic <= nSims) {
  while(1) {
      # check for existing hydra output files then removes
    f <- list.files(outDirForDatPin,".text$")
    if (!identical(f,character(0))) {file.remove(paste0(outDirForDatPin,"/",f))}

    ic <- ic + 1
    #if (ic%%10 == 0){ print(paste(is,"successes in",ic,"attempts"))}
    if (ic%%10 == 0){ print(paste(ic,"attempts"))}

    ################### simulate set of parameters ################################
    simulatedValues <- hydramse::simulate_parameters(stockRecruitData,simulationRules,SRFunctionChoice)

    # combine with extended time series for darwinian use
    #simulatedValues <- c(simulatedValues,noFishingData)
    hydraD <- hydramse::update_hydra_data(hydraD,simulatedValues) # update with simulated values
    hydraD <- hydramse::update_hydra_data(hydraD,noFishingData) # update with simulated values

    # create dat and pin files
    hydraD$flagMSE <- 2 # reduces output from hydra run
    hydraD$recStochastic <- hydraD$recStochastic*stochasticity
    inputOptions$outDir <- outDirForDatPin
    inputOptions$scenarioFlag <-  "darwin"

    hydraD <- hydradata::create_datpin_files(inputOptions,hydraD) # creates dat and pin file

    #########################################################################
    ######################## run hydra ######################################
    #########################################################################
    datPath <- paste0(outDirForDatPin,"/",inputOptions$outputFilename,".dat")
    pinPath <- paste0(outDirForDatPin,"/",inputOptions$outputFilename,".pin")
    exePath <- paste(pathToTPL,hydraVersion,sep="/")
    hydramse::run_single_hydra(exePath=exePath,datPath=datPath,pinPath=pinPath)


    # move files
    if (Sys.info()['sysname']=="Windows") {
      shell(paste0("move *.text ",paste0(outDirForDatPin,"/")),intern=T) # move files to darwin folder
    } else if (Sys.info()['sysname']=="Linux") {
      system(paste0("mv *.text ",paste0(outDirForDatPin,"/")),intern=T) # move files to darwin folder
    }

    # Check to see if set of parameters are good
    # read in output from model and summarize it
    output <- hydramse::process_darwin_output(outDirForDatPin,speciesList=hydraD$speciesList)
    biomass <- output$biomass
    catch <- output$catch
    if (boolPlot == T) {
      hydramse::plot_darwin_output(biomass,"Biomass",nYrsFishing,simulationRules)
      hydramse::plot_darwin_output(catch,"Catch",nYrsFishing,simulationRules)
    }

    # Do these simulations satisfy the rules
    ################################## No Fishing Biomass Rule ########################
    # biomass after nYrsNofishing years - should reach equilibriums - > lowest SSB seen
    # use mean of last 10 years
    rule1 <- hydramse::rule1_biomass(biomass,nYrsFishing,stockRecruitData$historicBounds,simulationRules,hydraD$speciesList)
    if(rule1$pass == F) { next} # these parameter values are garbage. Simulate next set
    ##################################  Fishing Biomass Rule ########################
    # biomass after fishing years should fall between .5 * lowest survey and 2 * highest survey
    # use mean of last 10 years
    rule2 <- hydramse::rule2_biomass(biomass,stockRecruitData$historicBounds,simulationRules,hydraD$speciesList)
    if(rule2$pass == F) { next}
    ################################## Catch Rule ########################
    # catch falls between .5 and 2 * catch
    rule3 <- hydramse::rule3_landings(catch,stockRecruitData$historicBounds,simulationRules,hydraD$speciesList)
    if(rule3$pass == F) { next}

    is <- is+1
    darwinSet <- list(hydraD=hydraD, inputOptions=inputOptions, simulationRules=simulationRules)

    saveRDS(darwinSet,file=paste0(outPath,"/success",ic,".Rds"))
    #save(hydraD,inputOptions,simulationRules,file=paste0(outPath,"/success",ic,".Rda"))
    return()
  }

#  return(list(nSuccesses=is,nAttempts=ic))

}
