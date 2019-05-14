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
    st <- proc.time()
    print(outPutScenarioDirs[iRun])

    filesToProcess <- list.files(paste0(outPutScenarioDirs[iRun],"/",outputType))
    # this needs to be coded in c++ to speed things up
    modelOutput <- lapply(paste0(outPutScenarioDirs[iRun],"/",outputType,"/",filesToProcess),file_to_Rlist)
    listOfStuff <- process_single_scenario(modelOutput,indices,revenueData)

    # now we need to massage output and save in an RDS file
    indicesNames <- colnames(listOfStuff$dfI)
    if(iRun == 1) {
      # preallocate some memory
      dfI <- array(dim=c(data$Nyrs,nScenarios,length(indicesNames)),dimnames = list(yrNames,scenarioDirNames,indicesNames))
      species_bio <- array(dim = c(data$Nyrs,nScenarios,data$Nspecies),dimnames = list(yrNames,scenarioDirNames,speciesNames))
      species_catch <- array(dim = c(data$Nyrs,nScenarios,data$Nspecies),dimnames = list(yrNames,scenarioDirNames,speciesNames))
      guild_bio <- array(dim = c(data$Nyrs,nScenarios,data$numGuilds),dimnames = list(yrNames,scenarioDirNames,guildNames))
      guild_catch <- array(dim = c(data$Nyrs,nScenarios,data$numGuilds),dimnames = list(yrNames,scenarioDirNames,guildNames))
      print("yioppee")
    }
    # indices
    dfI[,iRun,] <- listOfStuff$dfI
    species_bio[,iRun,] <- listOfStuff$species_bio
    species_catch[,iRun,] <- listOfStuff$species_catch
    guild_bio[,iRun,] <- listOfStuff$guild_bio
    guild_catch[,iRun,] <- listOfStuff$guild_catch

    en <- proc.time()
    print((en-st)[3]/60)
  }

  # save indices to RDS
  saveRDS(dfI,file=here::here(rootFolder,"indices.rds"))
  # save biomass and catch to plotting data folder
  saveRDS(species_bio,file=here::here(rootFolder,"species_bio_rate.rds"))
  saveRDS(species_catch,file=here::here(rootFolder,"species_catch_rate.rds"))
  saveRDS(guild_bio,file=here::here(rootFolder,"guild_bio_rate.rds"))
  saveRDS(guild_catch,file=here::here(rootFolder,"guild_catch_rate.rds"))

}
