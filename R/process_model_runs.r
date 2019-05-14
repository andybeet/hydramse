#' Process model runs
#'
#' Reads in all of the model run data, processes it, and writes to an .rdata file. Details of the processing can be found in section below
#'
#'@param data List. The hydra dataList \code{hydradata::hydraDataList}
#'@param indices Character vector. The names of the indices of interest from the model run.
#'@param scenarios Character vector. The names of the scenario. Defined as a harvest control rul with a max exploitation rate
#'@param exRateVec Numeric vector. Exploitation rates used in the MSE
#'@param rootFolder root folder holding all run info
#'@param outPutScenarioDirs list of scenario directories under the rootFolder
#'@param revenueData Data frame (see \code{\link{process_single_scenario}})
#'
#'@return Nothing
#'
#'@section Processing details:
#'
#'Each scenario run ... more coming
#'
#'@export


process_model_runs <- function(data,indices,scenarios,exRateVec,rootFolder,outPutScenarioDirs,revenueData,outputType="indices"){

  #speciesNames <- c("dogfish", "skate", "herring", "cod", "haddock", "ytail_fl", "wint_fl", "mackerel", "silverhake", "goosefish")
  #guildNames  <- c("Piscivores","Planktivores","Benthivores","Elasmobranchs")
  #scenarios <- c("FixedComplex","FixedHTSpecies","FixedLTSpecies","RampComplex","RampHTSpecies","RampLTSpecies")
  #exRateVec <- c(5,10,15,20,25,30,35,40)
  #indices <- c("index_LFI_Catch","index_LFI_Biomass","index_stdev_catch","avByr","est_catch_biomass","index_status_species","index_status_guild")
  # find out how many directories
  #dirs <- list.dirs(here::here(rootFolder),recursive=FALSE,full.names=FALSE)
  #dirs <- dirs[grep("Exploitation",dirs)]


  speciesNames <- data$speciesList
  guildNames <- unique(data$guildNames)
  scenarioDirNames <- apply(as.matrix(outPutScenarioDirs),1,function(x) tail(unlist(strsplit(x,"/")),1))
  yrNames <- as.character(1:data$Nyrs)

  nRates <- length(exRateVec)
  numScens <- nRates*length(scenarios)

  scenarioNames <- character(length(numScens))
  nScenarios <- length(outPutScenarioDirs)

  for (iRun in 1:nScenarios) { # total number of scenarios ran
    print(outPutScenarioDirs[iRun])

    filesToProcess <- list.files(paste0(outPutScenarioDirs[iRun],"/",outputType))
    # this needs to be coded in c++ to speed things up
    modelOutput <- lapply(paste0(outPutScenarioDirs[iRun],"/",outputType,"/",filesToProcess),file_to_Rlist)
    listOfStuff <- process_single_scenario(modelOutput,indices,revenueData)

    # now we need to massage output and save in an RDS file
    indicesNames <- colnames(listOfStuff$dfI)
    if(iRun == 1) {
      # preallocate some memory
      dfI <- array(dim=c(data$Nyrs,length(indicesNames),nScenarios),dimnames = list(yrNames,indicesNames,scenarioDirNames))
      species_bio <- array(dim = c(data$Nyrs,nScenarios,data$Nspecies),dimnames = list(yrNames,scenarioDirNames,speciesNames))
      species_catch <- array(dim = c(data$Nyrs,nScenarios,data$Nspecies),dimnames = list(yrNames,scenarioDirNames,speciesNames))
      guild_bio <- array(dim = c(data$Nyrs,nScenarios,data$numGuilds),dimnames = list(yrNames,scenarioDirNames,guildNames))
      guild_catch <- array(dim = c(data$Nyrs,nScenarios,data$numGuilds),dimnames = list(yrNames,scenarioDirNames,guildNames))
      print("yioppee")
    }
    # indices
    dfI[,,iRun] <- listOfStuff$dfI
    species_bio[,iRun,] <- listOfStuff$species_bio
    species_catch[,iRun,] <- listOfStuff$species_catch
    guild_bio[,iRun,] <- listOfStuff$guild_bio
    guild_catch[,iRun,] <- listOfStuff$guild_catch

  }

  # save indices to RDS
  saveRDS(dfI,file=here::here(rootFolder,"indices.rds"))
  # save biomass and catch to plotting data folder
  saveRDS(species_bio,file=here::here(rootFolder,"species_bio_rate.rds"))
  saveRDS(species_catch,file=here::here(rootFolder,"species_catch_rate.rds"))
  saveRDS(guild_bio,file=here::here(rootFolder,"guild_bio_rate.rds"))
  saveRDS(guild_catch,file=here::here(rootFolder,"guild_catch_rate.rds"))


  #return(dfI)

  # ic <- 0
  # for (iscenario in 1:length(scenarios)) {
  #   rateType <- scenarios[iscenario]
  #
  #   # preallocate
  #   species_bio <- list(nRates)
  #   species_catch <- list(nRates)
  #   guild_bio <- list(nRates)
  #   guild_catch <- list(nRates)
  #   numTimesBelowThreshold <- list(nRates)
  #   numTimesSpeciesBelowThreshold <- list(nRates)
  #   species_bio_Runs <- list(nRates)
  #   species_catch_Runs <- list(nRates)
  #   guild_bio_Runs <- list(nRates)
  #   guild_catch_Runs <- list(nRates)
  #
  #
  #   for (iRate in 1:nRates) {
  #
  #     ic <- ic + 1
  #     print(paste("exploitation",rateType," = ",exRateVec[iRate]))
  #     exploitationRate <- exRateVec[iRate]
  #
  #     if (exploitationRate < 10) {
  #       filePath <- paste0(wd,"/Exploitation",rateType,"0",exRateVec[iRate],"/indices")
  #     } else {
  #       filePath <- paste0(wd,"/Exploitation",rateType,exRateVec[iRate],"/indices")
  #     }
  #
  #     scenarioNames[ic] <- paste0("Exploitation",rateType,exploitationRate)
  #     # read in all files and process the data
  #     listOfBioCatch <- main_hydra_AnalyzeAll(filePath,exploitationRate,rateType,numYearsAtEndOfRun,speciesNames,guildNames,
  #                                             indices,revenueData,wd)
  #
  #     species_bio[[iRate]] <-  listOfBioCatch$species_bio/convertToKilotons
  #     species_catch[[iRate]] <-  listOfBioCatch$species_catch/convertToKilotons
  #     guild_bio[[iRate]] <-  listOfBioCatch$guild_bio/convertToKilotons
  #     guild_catch[[iRate]] <-  listOfBioCatch$guild_catch/convertToKilotons
  #     species_bio_Runs[[iRate]] <-  listOfBioCatch$species_bio_Runs/convertToKilotons
  #     species_catch_Runs[[iRate]] <-  listOfBioCatch$species_catch_Runs/convertToKilotons
  #     guild_bio_Runs[[iRate]] <-  listOfBioCatch$guild_bio_Runs/convertToKilotons
  #     guild_catch_Runs[[iRate]] <-  listOfBioCatch$guild_catch_Runs/convertToKilotons
  #
  #     dfI <- listOfBioCatch$df
  #     dfIndices[,ic,] <- dfI
  #     numTimesBelowThreshold[[iRate]] <- t(listOfBioCatch$numGuildBelow)
  #     numTimesSpeciesBelowThreshold[[iRate]] <- t(listOfBioCatch$numSpeciesBelow)
  #   }
  #
  #
  #   nT <- dim(guild_bio[[1]])[1]
  #   nTRuns <- dim(guild_bio_Runs[[1]])[1]
  #
  #   numGuilds <- dim(guild_bio[[1]])[2]
  #   numSpecies <- dim(species_bio[[1]])[2]
  #
  #
  #
  #   # sort into sub matrices over different rates
  #
  #   # guild level
  #   guild_bio_rate <- list(numGuilds)
  #   guild_catch_rate <- list(numGuilds)
  #   nBelowThreshold <- list(numGuilds)
  #   guild_bio_rate_Runs <- list(numGuilds)
  #   guild_catch_rate_Runs <- list(numGuilds)
  #   print("##########################################################################################")
  #   print(dim(guild_bio_rate_Runs))
  #   print(length(guild_bio_Runs))
  #   print(dim(guild_bio_Runs[[1]]))
  #   print(nTRuns)
  #   print(nRates)
  #   print(numGuilds)
  #   # return(guild_bio_Runs)
  #
  #   print("##########################################################################################")
  #   for (ig in 1:numGuilds) { # 3 guilds
  #     guild_bio_rate[[ig]] <- matrix(0,nT,nRates)
  #     guild_catch_rate[[ig]] <- matrix(0,nT,nRates)
  #     nBelowThreshold[[ig]] <- matrix(0,nT,nRates)
  #     guild_bio_rate_Runs[[ig]] <- matrix(0,nTRuns,nRates)
  #     guild_catch_rate_Runs[[ig]] <- matrix(0,nTRuns,nRates)
  #     for (iRate in 1:nRates) {
  #       guild_bio_rate[[ig]][,iRate] <- guild_bio[[iRate]][,ig]
  #       guild_catch_rate[[ig]][,iRate] <- guild_catch[[iRate]][,ig]
  #       nBelowThreshold[[ig]][,iRate] <- numTimesBelowThreshold[[iRate]][,ig]
  #
  #       guild_bio_rate_Runs[[ig]][,iRate] <- guild_bio_Runs[[iRate]][,ig]
  #       guild_catch_rate_Runs[[ig]][,iRate] <- guild_catch_Runs[[iRate]][,ig]
  #     }
  #   }
  #
  #   #species level
  #
  #   # runs
  #   species_bio_rate_Runs <- matrix(0,nTRuns,nRates)
  #   species_catch_rate_Runs <- matrix(0,nTRuns,nRates)
  #   for (iRate in 1:nRates) {
  #     species_bio_rate_Runs[,iRate] <- species_bio_Runs[[iRate]]
  #     species_catch_rate_Runs[,iRate] <- species_catch_Runs[[iRate]]
  #   }
  #
  #   # yrs
  #   species_bio_rate <- list(numGuilds)
  #   species_catch_rate <- list(numGuilds)
  #   nSpBelowThreshold <- list(numGuilds)
  #
  #   for (is in 1:numSpecies) { # 3 guilds
  #     species_bio_rate[[is]] <- matrix(0,nT,nRates)
  #     species_catch_rate[[is]] <- matrix(0,nT,nRates)
  #     nSpBelowThreshold[[is]] <- matrix(0,nT,nRates)
  #     for (iRate in 1:nRates) {
  #       species_bio_rate[[is]][,iRate] <- species_bio[[iRate]][,is]
  #       species_catch_rate[[is]][,iRate] <- species_catch[[iRate]][,is]
  #       nSpBelowThreshold[[is]][,iRate] <- numTimesSpeciesBelowThreshold[[iRate]][,is]
  #     }
  #   }
  #
  #
  #   # write all summaries to xls files stored in "plottingData Folder
  #   write.table(exRateVec/100,paste0(wd,"/plottingData/",rateType,"/exploitationRates.txt"),col.names = FALSE,row.names = FALSE)
  #
  #
  #   #write variables to files for later use
  #   for (iguild in 1:numGuilds) {
  #     # yrs
  #     colnames(guild_bio_rate[[iguild]]) <- paste (rateType,as.character(exRateVec))
  #     colnames(guild_catch_rate[[iguild]]) <- paste (rateType,as.character(exRateVec))
  #     colnames(nBelowThreshold[[iguild]]) <- paste (rateType,as.character(exRateVec))
  #     xlsx::write.xlsx(guild_bio_rate[[iguild]],paste0(wd,"/plottingData/",rateType,"/guild_bio_rate.xlsx"),guildNames[iguild],append=TRUE,col.names = TRUE,row.names = FALSE)
  #     xlsx::write.xlsx(guild_catch_rate[[iguild]],paste0(wd,"/plottingData/",rateType,"/guild_catch_rate.xlsx"),guildNames[iguild],append=TRUE,col.names = TRUE,row.names = FALSE)
  #     xlsx::write.xlsx(nBelowThreshold[[iguild]],paste0(wd,"/plottingData/",rateType,"/nGuildBelowThreshold.xlsx"),guildNames[iguild],append=TRUE,col.names = TRUE,row.names = FALSE)
  #     # runs
  #     colnames(guild_bio_rate_Runs[[iguild]]) <- paste (rateType,as.character(exRateVec))
  #     colnames(guild_catch_rate_Runs[[iguild]]) <- paste (rateType,as.character(exRateVec))
  #     xlsx::write.xlsx(guild_bio_rate_Runs[[iguild]],paste0(wd,"/plottingData/",rateType,"/guild_bio_rate2.xlsx"),guildNames[iguild],append=TRUE,col.names = TRUE,row.names = FALSE)
  #     xlsx::write.xlsx(guild_catch_rate_Runs[[iguild]],paste0(wd,"/plottingData/",rateType,"/guild_catch_rate2.xlsx"),guildNames[iguild],append=TRUE,col.names = TRUE,row.names = FALSE)
  #
  #   }
  #   # yrs
  #   for (isp in 1:numSpecies) {
  #     colnames(nSpBelowThreshold[[isp]]) <- paste (rateType,as.character(exRateVec))
  #     colnames(species_bio_rate[[isp]]) <- paste (rateType,as.character(exRateVec))
  #     colnames(species_catch_rate[[isp]]) <- paste (rateType,as.character(exRateVec))
  #     xlsx::write.xlsx(species_bio_rate[[isp]],paste0(wd,"/plottingData/",rateType,"/species_bio_rate.xlsx"),speciesNames[isp],append=TRUE,col.names = TRUE,row.names = FALSE)
  #     xlsx::write.xlsx(species_catch_rate[[isp]],paste0(wd,"/plottingData/",rateType,"/species_catch_rate.xlsx"),speciesNames[isp],append=TRUE,col.names = TRUE,row.names = FALSE)
  #     xlsx::write.xlsx(nSpBelowThreshold[[isp]],paste0(wd,"/plottingData/",rateType,"/nSpeciesBelowThreshold.xlsx"),speciesNames[isp],append=TRUE,col.names = TRUE,row.names = FALSE)
  #   }
  #   # runs
  #   colnames(species_bio_rate_Runs) <- paste (rateType,as.character(exRateVec))
  #   colnames(species_catch_rate_Runs) <- paste (rateType,as.character(exRateVec))
  #   xlsx::write.xlsx(species_bio_rate_Runs,paste0(wd,"/plottingData/",rateType,"/species_bio_rate2.xlsx"),"AverageBiomassPerYear",append=TRUE,col.names = TRUE,row.names = FALSE)
  #   xlsx::write.xlsx(species_catch_rate_Runs,paste0(wd,"/plottingData/",rateType,"/species_catch_rate2.xlsx"),"AverageCatchPerYear",append=TRUE,col.names = TRUE,row.names = FALSE)
  #
  #
  #   # write speciesNames and guildNames to file
  #   write.table(speciesNames,paste0(wd,"/plottingData/",rateType,"/speciesNames.txt"),col.names = FALSE,row.names = FALSE)
  #   write.table(guildNames,paste0(wd,"/plottingData/",rateType,"/guildNames.txt"),col.names = FALSE,row.names = FALSE)
  #
  # } # end scenario
  # #print(dfIndices)
  # #### write indices
  # dimnames(dfIndices) <- list(c(),scenarioNames,c(indices,"Revenue"))
  # fName <- paste0(wd,"/indicesByScenario.xlsx")
  # for (kindex in 1:length(indices)) {
  #   xlsx::write.xlsx(dfIndices[,,kindex],fName,indices[kindex],col.names=TRUE,row.names=FALSE,append=TRUE)
  # }
  # xlsx::write.xlsx(dfIndices[,,length(indices)+1],fName,"Revenue",col.names=TRUE,row.names=FALSE,append=TRUE)
  #
  #
  #






}
