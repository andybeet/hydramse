#' Process model runs
#'
#' Reads in all of the model run data, processes it, and writes to an .rdata file. Details of the processing can be found in section below.
#' Contents of rdata file are tidy and used in plotting routines
#'
#'@param data List. The hydra dataList \code{hydradata::hydraDataList}
#'@param indices Character vector. The names of the indices of interest from the model run.
#'@param scenarios Character vector. The names of the scenario. Defined as a harvest control rul with a max exploitation rate
#'@param rootFolder root folder holding all run info
#'@param outPutScenarioDirs list of scenario directories under the rootFolder
#'@param revenueData Data frame (see \code{\link{process_single_scenario}})
#'
#'@return Nothing. RDS files are produced
#'
#'@section Processing details:
#'
#'A model run (scenario) is considered to be n simulations of the Hydra model. Stochasticity differentiates each simulation.
#'
#'Each scenario/ model run is processed as in \code{\link{process_single_scenario}}. All of the processed scenarios and the relevant indices are then exported
#'as an RDS file
#'
#'@export


process_model_runs <- function(data,indices,scenarios,rootFolder,outPutScenarioDirs,revenueData,outputType="indices"){

  speciesNames <- data$speciesList
  guildNames <- unique(data$guildNames)
  scenarioDirNames <- apply(as.matrix(outPutScenarioDirs),1,function(x) tail(unlist(strsplit(x,"/")),1))
  yrNames <- as.character(1:data$Nyrs)

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

  # converts to tidy data and saves eventually pass these as argument
  outputs <- data.frame(variableNames = c("dfI","species_bio","species_catch","guild_bio","guild_catch"),
                        fileNames= c("indices.rds","species_bio_rate.rds","species_catch_rate.rds","guild_bio_rate.rds","guild_catch_rate.rds"))
  for (iv in 1:dim(outputs)[1]) {
    data <- get(as.character(outputs$variableNames[iv]))
    data <- reshape2::melt(data)
    colnames(data) <- c("Year","ScenarioFolderName","Type","Value")
    data <- tidyr::separate(data,ScenarioFolderName,into=c("Scenario","Exploitation"),-2)
    data <- data %>% dplyr::mutate(Scenario=stringr::str_replace(Scenario,"Exploitation",""))
    saveRDS(data,file=here::here(rootFolder,outputs$fileNames[iv]))
  }

}
