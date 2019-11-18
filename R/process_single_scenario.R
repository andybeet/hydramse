#'Process all runs for a given scenario
#'
#'A scenario is defined a a harvest control rule with a maximum exploitation rate.
#'Each scenario is run a number of times (nSims). All of the runs for a scenario are processed.
#'Specifics of processing are expalined in section below
#'
#'@param modelOutput List (length = nSims) content of files created by running Hydra
#'@param indices Character vector. Names of the indices to process
#'@param revenueData Data frame. External revenue data (names = character vector of species names, ppp = price ($) per pound)
#'@param convertBiomassTolbs Numeric scalar. 1 metric tonne = 2204.62 lbs (default)
#'@param nLastYrs Numeric scalar. Number of trailing years in each run to take mean over. Default = 20 (Last 20 yrs of sim)
#'
#'
#'@return A list containing the following items:
#'\item{species_bio}{matrix (nYears x nSpecies) of median biomass (over nSims)}
#'\item{species_catch}{matrix (nYears x nSpecies) of median catch (over nsims)}
#'\item{guild_bio}{matrix (nYears n nGuilds) of median biomass by guild (over nsims)}
#'\item{guild_catch}{matrix (nYears x nGuilds) of median catch by guild (over nsims)}
#'\item{dfI}{data frame. Each column represents an index. Each row represent a year. see section below}
#'\item{catch_runs}{matrix (nGuilds x nruns). Mean of the last x years of each run }
#'
#'@section Processing specifics:
#'
#'\code{species_bio}, \code{species_catch}, \code{guild_bio}, \code{guild_bio} are calculated from ("avByr","est_catch_biomass","est_survey_guild_biomass","est_catch_guild_biomass")
#'by taking the median value over the nSims model runs.
#'
#'Each index in \code{indices} is processed and returned in \code{dfI}
#'
#'For indices that are not species specific (Diversity indices, Large fish indices) the median value over all nSims model runs are used at each time point
#'
#'For indices that are species dependent (avByr, est_catch_biomass). The median is calculated (over all nSims) for each species at each time point.
#'The mean of the species medians are then used to represent the returned index value
#'
#'For indices that are species specific that involve status (species or guild status) the index is calculated as the total number of times a species (or guild) falls below the minimum threshold specified in the model
#'
#'Revenue is calculated from "est_catch_biomass". Biomass is converted to lbs. The median weight for each species in each year is multiplied by the price per pound (currently 2012 price)
#'
#'@export


process_single_scenario <- function(modelOutput,indices,revenueData,convertBiomassTolbs=2204.62,nLastYrs=20){

  # extract catch, convert biomass to lbs then apply price per pound to generate total revenue
  # 1 metric tonne = 2204.62 lbs

  avByr_bio<-lapply(modelOutput, '[[', "avByr")
  catch_bio<-lapply(modelOutput, '[[', "est_catch_biomass")
  guild_biomass <- lapply(modelOutput,"[[","est_survey_guild_biomass")
  guild_catch <- lapply(modelOutput,"[[","est_catch_guild_biomass")

  numRuns <- length(modelOutput) # number of files
  numGuilds <- modelOutput[[1]]$Nguilds
  numYrs <- modelOutput[[1]]$Nyrs
  numSpecies <- length(modelOutput[[1]]$threshold_species)

  ##################################### for each year over RUNS ##################################

  # find the median Biomass and Catch at guild level and species level in each year (over runs)
  # first flatten the list to a 3d array
  avByr_bio <- simplify2array(avByr_bio) # nSpecies x nYears X nRuns
  catch_bio <- simplify2array(catch_bio)
  guild_biomass <- simplify2array(guild_biomass) # nGuilds x nYears x nRuns
  guild_catch <- simplify2array(guild_catch)
  # median over runs
  species_bio <- t(apply(avByr_bio,1:2,median)) # nYrs x nSpecies
  species_catch <- t(apply(catch_bio,1:2,median)) # nYrs x nSpecies
  bio <- t(apply(guild_biomass,1:2,median)) # nYrs x nGuilds
  catch <- t(apply(guild_catch,1:2,median)) # nYrs x nGuilds

  ##################################### for each run over last NYEARS ##############################

  guild_catch_runs <- guild_catch[,(numYrs-nLastYrs+1):numYrs,] # extract last x years
  guild_catch_runs <- t(apply(guild_catch_runs,c(1,3),mean)) # nRuns x nGuilds

  ########################## index stuff ################################################

  df <- array(numeric(0),dim=c(numYrs,length(indices))) # Revenue
  nameVec <- indices

  for (jindex in 1:length(indices )) { # loop through indices
    variableData<-lapply(modelOutput, '[[', indices[jindex])
    dimSize <- dim(variableData[[1]])[1]

    if (length(variableData[[1]])==0)  { # error check
      print(paste0(indices[jindex]," doesn't exist for this scenario"))
      next
    }

    if (is.null(dimSize)) {           # Indices that are not species specific, like index_Simpsons_Crecip
      print(paste0("Processing ", indices[jindex]))
      flattenData <- simplify2array(variableData)
      dataMedQuant <- apply(flattenData,1,quantile,probs=.5,na.rm=TRUE)
      meanIndex <- dataMedQuant

    } else if (dimSize == numSpecies){ # Species specific
      print(paste0("Processing ", indices[jindex]))
      flattenData <- simplify2array(variableData)

      if (indices[jindex] == "index_status_species") { # Species status
        meanIndex <- as.vector(apply(flattenData,2,sum)) # sums over all 10 species and run combination to get yearly value
        #numSpeciesBelow <- apply(flattenData,1:2,sum) # sums over all 100 runs for species and run combination. Total numer times
      } else {
        dataMedQuant <- apply(flattenData,1:2,quantile,probs=.5,na.rm=TRUE) ## for each species, year. takes median over 100 runs
        meanIndex <- colMeans(dataMedQuant) # mean
      }

    } else if (dimSize == numGuilds) { # guild specific
      print(paste0("Processing ", indices[jindex]))
      flattenData <- simplify2array(variableData)
      meanIndex <- as.vector(apply(flattenData,2,sum)) # sums over all 10 species and run combination to get yearly value
      #numGuildBelow <- apply(flattenData,1:2,sum) # sums over all 100 runs for species and run combination. Total numer times
    }
    df[,jindex] <- meanIndex
  }


  # now calculate revenue from est catch biomass
  flattenData <- catch_bio
  # calculate revenue and add as last column
  flattenDataRev <- flattenData*convertBiomassTolbs
  for (ispec in 1:numSpecies) { # multiplies each species lbs by price per pound
    flattenDataRev[ispec,,] <- flattenDataRev[ispec,,]*revenueData$ppp[ispec]
  }
  revData <- apply(flattenDataRev,2:3,sum) # sums over all 10 species for each year/run combination
  dataMedQuant <- apply(revData,1,quantile,probs=.5,na.rm=TRUE) # median for each year
  meanIndex <- dataMedQuant/1000000 # convert to millions of dollars

  # update dataframe
  df <- cbind(df,meanIndex)
  colnames(df) <- c(nameVec,"Revenue")



  results <- list(species_bio=species_bio, species_catch=species_catch, guild_bio=bio, guild_catch=catch,dfI=df, guild_catch_runs=guild_catch_runs)

  return(results)



}
