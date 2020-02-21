#' Process model runs
#'
#' Reads in all of the model run data, processes it, and writes to an .rds file. Details of the processing can be found in section below.
#' Contents of rds file are tidy and used in plotting routines
#'
#'@param data List. The hydra dataList \code{hydradata::hydraDataList}
#'@param indices Character vector. The names of the indices of interest from the model run.
#'@param scenarios Character vector. The names of the scenario. Defined as a harvest control rul with a max exploitation rate
#'@param rootFolder root folder holding all run info
#'@param outPutScenarioDirs list of scenario directories under the rootFolder
#'@param revenueData Data frame (see \code{\link{process_single_scenario}})
#'@param outputType Character string. Name of folder where Hydra output files are stored ("indices","diagnostics")
#'@param nLastYrs Numeric scalar. Number of trailing years in each run to take mean over. Default = 20 (Last 20 yrs of sim)

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


process_model_runs <- function(data,indices,scenarios,rootFolder,outPutScenarioDirs,revenueData,outputType="indices",nLastYrs){

  speciesNames <- data$speciesList
  guildNames <- unique(data$guildNames)
  scenarioDirNames <- apply(as.matrix(outPutScenarioDirs),1,function(x) tail(unlist(strsplit(x,"/")),1))
  yrNames <- as.character(1:data$Nyrs)

  nScenarios <- length(outPutScenarioDirs)

  for (iRun in 1:nScenarios) { # total number of scenarios ran
   # st <- proc.time()
    message(paste0("Processing run = ",outPutScenarioDirs[iRun]))

    filesToProcess <- list.files(paste0(outPutScenarioDirs[iRun],"/",outputType))
    # this needs to be coded in c++ to speed things up
    modelOutput <- lapply(paste0(outPutScenarioDirs[iRun],"/",outputType,"/",filesToProcess),file_to_Rlist)
    nSims <- length(modelOutput)
    listOfStuff <- process_single_scenario(modelOutput,indices,revenueData,nLastYrs=nLastYrs)

    # now we need to massage output and save in an RDS file
    indicesNames <- colnames(listOfStuff$dfI)
    #print(indicesNames)
    if(iRun == 1) {
      # preallocate some memory
      simNames <- as.character(1:nSims)
      dfI <- array(dim=c(data$Nyrs,nScenarios,length(indicesNames)),dimnames = list(year = yrNames,scenario=scenarioDirNames,index=indicesNames))
      species_bio <- array(dim = c(data$Nyrs,nScenarios,data$Nspecies),dimnames = list(year = yrNames,scenario = scenarioDirNames,index = speciesNames))
      species_catch <- array(dim = c(data$Nyrs,nScenarios,data$Nspecies),dimnames = list(year = yrNames,scenario = scenarioDirNames,index = speciesNames))
      guild_bio <- array(dim = c(data$Nyrs,nScenarios,data$numGuilds),dimnames = list(year = yrNames,scenario = scenarioDirNames,index = guildNames))
      guild_catch <- array(dim = c(data$Nyrs,nScenarios,data$numGuilds),dimnames = list(year = yrNames,scenario = scenarioDirNames,index = guildNames))
      guild_catch_runs <- array(dim = c(nSims,nScenarios,data$numGuilds),dimnames = list(sim= simNames,scenario = scenarioDirNames,index = guildNames))

    }
    # indices
    dfI[,iRun,] <- listOfStuff$dfI
    species_bio[,iRun,] <- listOfStuff$species_bio
    species_catch[,iRun,] <- listOfStuff$species_catch
    guild_bio[,iRun,] <- listOfStuff$guild_bio
    guild_catch[,iRun,] <- listOfStuff$guild_catch
    guild_catch_runs[,iRun,] <- listOfStuff$guild_catch_runs

    # en <- proc.time()
    # print((en-st)[3]/60)
  }

  # converts to tidy data and saves eventually pass these as argument
  outputs <- data.frame(variableNames = c("dfI","species_bio","species_catch","guild_bio","guild_catch","guild_catch_runs"),
                        fileNames= c("indices.rds","species_bio_rate.rds","species_catch_rate.rds","guild_bio_rate.rds","guild_catch_rate.rds","guild_catch_runs.rds"))
  for (iv in 1:dim(outputs)[1]) {
    data <- get(as.character(outputs$variableNames[iv])) # return the value of data
    colHeaders <- dimnames(data)$index
    data <- plyr::adply(data,c(1,2)) # converts array to dataframe
    #data <- reshape2::melt(data)
    data <- tidyr::gather(data,key="Type",value="Value",colHeaders) # tidyfies data
    colnames(data) <- c("Year","ScenarioFolderName","Type","Value")
    # splits column into two
    data <- tidyr::separate(data,ScenarioFolderName,into=c("Scenario","Exploitation"),-2)
    data <- data %>% dplyr::mutate(Scenario=stringr::str_replace(Scenario,"Exploitation",""))
    data$Year <- as.numeric(levels(data$Year)[data$Year])
    saveRDS(data,file=here::here(rootFolder,outputs$fileNames[iv]))
  }

}
