## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE,eval = FALSE----------------------------------------------
#  newData <- hydramse::lengthen_hydra_data(hydradata::hydraDataList,nYrs=150)

## ----echo=TRUE,eval =FALSE-----------------------------------------------
#  outDirForDatPin <- here::here("darwin") # folder for dumping hydra output prior to analysis
#  outPath <- here::here("successfulSims") # folder containing parameters of successful model runs
#  
#  if(!dir.exists(outDirForDatPin)){dir.create(outDirForDatPin)}
#  if(!dir.exists(outPath)){dir.create(outPath)}

## ----eval=F, echo=T------------------------------------------------------
#    ipass <- 0
#    simulationRules <- hydramse::darwinRules
#    darwinD <- hydramse::darwinData
#  
#    # simulate a set of parameters
#    simulatedValues <- hydramse::simulate_parameters(darwinD,simulationRules,SRFunctionChoice = NULL)
#    # combine with extended time series data for darwinian use
#    simulatedValues <- c(simulatedValues,newData)
#    hydraD <- hydramse::update_hydra_data(hydradata::hydraDataList,simulatedValues) # update with simulated values
#    # create dat and pin files
#    inputOptions <- hydradata::setup_default_inputs(outDir=outDirForDatPin) # gets options for hydraRuns
#    inputOptions$scenarioFlag <- "darwin"
#    hydraD$flagMSE <- 2 # reduces output from hydra run
#    hydraD$recStochastic <- hydraD$recStochastic*0 # no stochasticity used in model run
#    hydraD <- hydradata::create_datpin_files(inputOptions,hydraD) # creates dat and pin file
#  
#    ######################## run hydra ######################################
#    #specify the path the input files and the Hydra model executable
#    datPath <- paste0(outDirForDatPin,"/",inputOptions$outputFilename,".dat")
#    pinPath <- paste0(outDirForDatPin,"/",inputOptions$outputFilename,".pin")
#    exePath <- paste(pathToTPL,hydraVersion,sep="/")
#    hydramse::run_single_hydra(iseed=1,exePath=exePath,datPath=datPath,pinPath=pinPath)
#  
#    # move output file
#    if (Sys.info()['sysname']=="Windows") {
#      shell(paste0("move *.text ",paste0(outDirForDatPin,"/")),intern=T) # move files to darwin folder
#    } else if (Sys.info()['sysname']=="Linux") {
#      system(paste0("mv *.text ",paste0(outDirForDatPin,"/")),intern=T) # move files to darwin folder
#    }
#  
#    # processes output from model (1 file)
#    output <- hydramse::process_darwin_output(outDirForDatPin)
#    biomass <- output$biomass
#    catch <- output$catch
#    # Do these simulations satisfy the rules(historical equivalence)
#    ################################## No Fishing Biomass Rule ########################
#    # biomass after nYrsNofishing years - should reach equilibriums - > lowest SSB seen
#    rule1 <- hydramse::rule1_biomass(biomass,nYrsFishing,stockRecruitData$historicBounds,simulationRules)
#    if(rule1$pass == F) { next} # these parameter values are garbage. Simulate next set
#    ##################################  Fishing Biomass Rule ########################
#    # biomass after fishing years should fall between .5 * lowest survey and 2 * highest survey
#    # use mean of last 10 years
#    rule2 <- hydramse::rule2_biomass(biomass,nYrs,stockRecruitData$historicBounds,simulationRules)
#    if(rule2$pass == F) { next}
#    ################################## Catch Rule ########################
#    # catch falls between .5 and 2 * catch
#    rule3 <- hydramse::rule3_landings(catch,nYrs,stockRecruitData$historicBounds,simulationRules)
#    if(rule3$pass == F) { next}
#  
#    ipass <- ipass + 1
#    # save all data to RDA file for use in MSE
#    save(hydraD,inputOptions,simulationRules,file=paste0(outPath,"/success",ipass,".Rda"))
#  
#  

