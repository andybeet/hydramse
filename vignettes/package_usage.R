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

## ---- eval=F,echo=T------------------------------------------------------
#  hydraVersion <- "hydra_sim"

## ----eval=F, echo=T------------------------------------------------------
#    ipass <- 0
#    simulationRules <- hydramse::darwinRules
#    darwinD <- hydramse::darwinData
#  
#    # simulate a set of parameters
#    simulatedValues <- hydramse::simulate_parameters(darwinD,simulationRules,SRFunctionChoice = NULL)
#    # combine with extended time series data for darwinian use
#    hydraD <- hydramse::update_hydra_data(hydradata::hydraDataList,simulatedValues) # update with simulated values
#    hydraD <- hydramse::update_hydra_data(hydradata::hydraDataList,noFishingData) # update with simulated values
#    # create dat and pin files
#    inputOptions <- hydradata::setup_default_inputs(outDir=outDirForDatPin) # gets options for hydraRuns
#    inputOptions$scenarioFlag <- "darwin"
#    hydraD$flagMSE <- 2 # reduces output from hydra run
#    hydraD$recStochastic <- hydraD$recStochastic*0 # no stochasticity used in model run
#    hydraD <- hydradata::create_datpin_files(inputOptions,hydraD) # creates dat and pin file
#  
#    # run hydra
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

## ----eval= F,echo=T------------------------------------------------------
#    # processes output from model (1 file)
#    output <- hydramse::process_darwin_output(outDirForDatPin)
#    biomass <- output$biomass
#    catch <- output$catch
#    # Do these simulations satisfy the rules(historical equivalence)
#    # No Fishing Biomass Rule. assumes equilibrium is reached
#    rule1 <- hydramse::rule1_biomass(biomass,nYrsFishing,stockRecruitData$historicBounds,simulationRules)
#    if(rule1$pass == F) { next}
#    #  Fishing Biomass Rule. use mean of last 10 years
#    rule2 <- hydramse::rule2_biomass(biomass,nYrs,stockRecruitData$historicBounds,simulationRules)
#    if(rule2$pass == F) { next}
#    # Catch Rule
#    rule3 <- hydramse::rule3_landings(catch,nYrs,stockRecruitData$historicBounds,simulationRules)
#    if(rule3$pass == F) { next}
#  
#    ipass <- ipass + 1
#    # save all data to RDA file for use in MSE
#    darwinRes <- list(hydraDarwin=hydraD,simulatedValues=simulatedValues,simulationRules=simulationRules,inputOptions=inputOptions)
#    saveRDS(darwinRes,file=paste0(outPath,"/success",ipass,".RDS"))
#  
#  

## ----eval=F,echo=T-------------------------------------------------------
#  rootFolder <- "define_the_folder_where_output_for_all_scenario_runs_be_stored"
#  darwinRes <- readRDS(file=paste0(outPath,"/success1.RDS")) # read in the RDS
#  dataToUse <- hydramse::update_hydra_data(hydradata::hydraDataList,darwinRes$simulatedValues)# sets the simulated values to the hydra model data
#  hcrTypes=c("Fixed","Ramp") # harvest control rules types
#  hcrLevels=c("Complex","HTSpecies","LTSpecies") #harvest control rule levels
#  scenarios <- apply(expand.grid(hcrTypes,hcrLevels),1,paste,collapse="")
#  # Create harvest control rule levels
#  dataToUse <- hydradata::set_hcr(dataToUse,minMaxExploitations = c(.05,0.4),increment=0.05)
#  dataToUse$flagMSE <- 1
#  exploitationRates = round(100*dataToUse$exploitationOptions[dataToUse$Nthresholds,])
#  # create folder structure for run (given exploitation rates). This creates folders in users working directory
#  folderStructure <- hydramse::create_folder_setup(rootFolder,exploitationRates = exploitationRates)
#  scenarios <- unique(apply(as.matrix(subset(folderStructure,select=c("hcrType","hcrLevels"))),1,function(x) paste0(x,collapse = "")))
#  folderDirs <- apply(folderStructure,1,function(x) paste0(x,collapse = "")) # vector of dirs
#  outputScenarioDirs <-  here::here(rootFolder,paste0("Exploitation",folderDirs))
#  
#  for (iscenario in 1:length(outputScenarioDirs)) {
#    outputScenarioDir <- outputScenarioDirs[iscenario]
#    print(outputScenarioDir)
#    # set up scenario types to run
#    if (grepl("LT",folderStructure$hcrLevels[iscenario])) speciesFlag <- "low"
#    if (grepl("HT",folderStructure$hcrLevels[iscenario])) speciesFlag <- "high"
#    if (grepl("omplex",folderStructure$hcrLevels[iscenario])) speciesFlag <- "none"
#    scenarioInputs <- hydradata::setup_default_inputs(outDir=outputScenarioDir,scenarioFlag="assessment",temperatureFlag="mean",
#                                                      scenarioType=folderStructure$hcrType[iscenario],maxExploitationRate=as.numeric(folderStructure$exploitationRate[iscenario]),
#                                                      assessmentSpeciesFlag=speciesFlag)
#  
#    # create dat and pin files for scenario
#    dataToPrint <- hydradata::create_datpin_files(listOfParameters=scenarioInputs, dataList=dataToUse)
#  }

## ----eval=F, echo = T----------------------------------------------------
#     #nCores <- parallel::detectCores()-1
#     #cl <- parallel::makeCluster(nCores)
#  
#     nSims <- 100 # set number of simulation for each scenario
#     exePath <- paste(pathToTPL,hydraVersion,sep="/")
#    for (iscenario in 1:dim(folderStructure)[1]) {
#      datpinPath <- here::here(rootFolder,paste0("Exploitation",paste0(folderStructure[iscenario,],collapse="")))
#      datPath <- paste0(datpinPath,"/",hydraVersion,".dat")
#      pinPath <- paste0(datpinPath,"/",hydraVersion,".pin")
#      #parallel::parLapply(cl,1:nSims,hydramse::run_single_hydra,exePath=exePath,datPath=datPath,pinPath=pinPath)
#      lapply(1:nSims,hydramse::run_single_hydra,exePath=exePath,datPath=datPath,pinPath=pinPath)
#      # move files
#      if (Sys.info()['sysname']=="Windows") {
#        shell(paste0("move *.txt ",paste0(datpinPath,"/indices")),intern=T) # move files to assessment folder
#      } else if (Sys.info()['sysname']=="Linux") {
#        system(paste0("mv *.txt ",paste0(datpinPath,"/indices")),intern=T) # move files to assessment folder
#      }
#    }
#     #parallel::stopCluster(cl)
#  

## ---- eval = F,echo = T--------------------------------------------------
#  indices <- c("index_LFI_Catch","index_LFI_Biomass","index_stdev_catch","avByr","est_catch_biomass","index_status_species","index_status_guild")
#  rD <- read.csv("revenuePricePerPound2012.csv",header=TRUE)
#  otherData <- list()
#  otherData$name = rD[,1]
#  otherData$ppp = rD[,2]
#  hydramse::process_model_runs(dataToUse,indices,scenarios,rootFolder,outputScenarioDirs,otherData,outputType="indices")
#  

## ---- eval=F, echo=T-----------------------------------------------------
#   hydramse::plot_box_whiskers(here::here(),rootFolder,inputFile="species_bio_rate.rds")

