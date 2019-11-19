#' creates Radar plots for scenario comparison
#'
#' Multipanel plots representing scenario/Exploitation rate across several
#' variables used to measure ecosystem health
#'
#' @param filePath Character string. Path to rootfolder
#' @param rootFolder Character string. Folder containing all runs output
#' @param inputFile Character string. RDS file to process.
#' @param timePeriod List. Time frame to average over. "early" and "late" fields eg (early = 21:30)
#' @param indices Character vector. The names of the indices of interest from the model run.
#'
#'
#' @return Plots are written
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes element_text
#'
#' @export


# radar plots for the scenarios for the June PDT.
# we read in the data, find the max, min, the values to summarize the run, then plot
#setwd("C:/Users/Andrew.Beet/Documents/MyWork/Hydra/Beet-Hydra/finishedRuns/CIE_1_4_like_1_3")
plot_radar_scenarios <- function(filePath,rootFolder,inputFile,timePeriod,indicesNames){
  #library(fmsb) # radar plot
  # time frame to average over. use as the representative point for a scenario run
  # we need to use a single measure to describe the scenario run. We will use 2. one will be the mean of years 21-30, the other 41-50
  early <- timePeriod$early #c(21:30)
  late <- timePeriod$late #c(41:50)

  nIndices <- length(indicesNames)
  # create a dataframe of the min and max for all variables
  #early period
  maxminsEarly <- as.data.frame(matrix(nrow=2,ncol=length(indicesNames)))
  names(maxminsEarly) <- indicesNames
  row.names(maxminsEarly) <- c("Max","Min")
  # late period
  maxminsLate <- as.data.frame(matrix(nrow=2,ncol=length(indicesNames)))
  names(maxminsLate) <- indicesNames
  row.names(maxminsLate) <- c("Max","Min")

  # read in indices
  indices <- dplyr::as_tibble(readRDS(file = paste0(filePath,"/",rootFolder,"/",inputFile)))
  scenarioNames <- names(indices)
  nScenarios <- length(scenarioNames)
  # create a dataframe with the values for each scenario
  scenarioValuesEarly <- matrix(nrow = nScenarios, ncol = nIndices,dimnames = list(scenarioNames,indicesNames))
  scenarioValuesLate <- matrix(nrow = nScenarios, ncol = nIndices,dimnames = list(scenarioNames,indicesNames))

  # this makes the number of exceedences -ve.
  # For radar plot outer edge is "good" outcome but a large number of exceedences is ba, so switch
  spStat <- indices %>% dplyr::filter(Type =="index_status_species") %>% dplyr::select(Value)
  guildStat <- as.vector(indices %>% dplyr::filter(Type =="index_status_guild") %>% dplyr::select(Value))
  indices <- indices %>% dplyr::mutate(Value = replace(Value,Type == "index_status_species",-dplyr::pull(spStat,Value)))
  indices <- indices %>% dplyr::mutate(Value = replace(Value,Type == "index_status_guild",-dplyr::pull(guildStat,Value)))

return(indices)

}
