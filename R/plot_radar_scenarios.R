#' creates Radar plots for scenario comparison
#'
#' Multipanel plots representing scenario/Exploitation rate across several
#' variables used to measure ecosystem health.
#'
#' THIS NEEDS A REALL OVERHAUL TO GENERALIZE TO A DIFF NUMBER OF SCENARIOS EXPLOITATIONS
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
#'
#' @export


# radar plots for the scenarios for the June PDT.
# we read in the data, find the max, min, the values to summarize the run, then plot
#setwd("C:/Users/Andrew.Beet/Documents/MyWork/Hydra/Beet-Hydra/finishedRuns/CIE_1_4_like_1_3")
plot_radar_scenarios <- function(filePath,rootFolder,inputFile,timePeriod,indicesNames){

  wd <- paste0(filePath,"/",rootFolder)
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
  scenarioNames <- indices %>%
    dplyr::mutate(scenarioName=paste0(Scenario,Exploitation)) %>%
    dplyr::select(scenarioName) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  nExploitation <- length(unique(indices$Exploitation))
  nScenarioNames <- length(unique(indices$Scenario))
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

  # extract the data for the required time periods
  for (index in indicesNames) {
    # extract the values over the time period and take column means
    ed <- indices %>%
      dplyr::filter(Type == index,Year %in% early) %>%
      dplyr::group_by(Scenario,Exploitation) %>%
      dplyr::summarise(means = mean(Value)) %>%
      dplyr::pull()
    ld <- indices %>%
      dplyr::filter(Type == index,Year %in% late) %>%
      dplyr::group_by(Scenario,Exploitation) %>%
      dplyr::summarise(means = mean(Value)) %>%
      dplyr::pull()

    # store the column means and
    #also the min and max (used to bound the radar plots)
    maxminsEarly[,index] <- c(max(ed),min(ed))
    maxminsLate[,index] <- c(max(ld),min(ld))

    # we now store each scenarios indices in a matrix
    scenarioValuesEarly[,index] <- ed
    scenarioValuesLate[,index] <- ld
  }

  # titlenames used in legend
  titleNames <- cbind("Exploitation = 0.10","Exploitation =  0.15","Exploitation =  0.20","Exploitation =  0.25","Exploitation =  0.30",
                      "Max Exploitation = 0.15","Max Exploitation =  0.20","Max Exploitation = 0.25","Max Exploitation = 0.30",
                      "Max Exploitation = 0.15","Max Exploitation =  0.20","Max Exploitation = 0.25","Max Exploitation = 0.30")
  labelNames <- c("Revenue","Large Fish \nLandings","Large Fish \n Population","Stability of\n Landings","Biomass","Landings","Species\n Status","Functional Group\n Status")

  #  scenarioTypes <- c("Fixed Exploitation Rate","Functional Group Status","Species Status")
  #scenarioTypes <- c("FixedComplex ","FixedHTSpecies","FixedLTSpecies","RampComplex","RampHTSpecies","RampLTSpecies")
  scenarioTypes <- c("Fixed Rate: Functional Group ","Species (Increased Threshold)","Species ","Ramped Rate: Functional Group","Species (Increased Threshold)","Species ")

  scenarioMembers <- list()
  scenarioMembers[[1]] <- 1:8#2:7#1:8
  scenarioMembers[[2]] <- 9:16#10:15#9:16
  scenarioMembers[[3]] <- 17:24#18:23#17:24
  scenarioMembers[[4]] <- 25:32#26:31#25:32
  scenarioMembers[[5]] <- 33:40#34:39#33:40
  scenarioMembers[[6]] <- 41:48#42:47#41:48

  scenarioOrder <- c(1,3,2,4,6,5)

  #colorVec <- c("darkgreen","lightblue","darkblue","red","black","grey")
  #colorVec <- c("darkgreen","lightblue","darkblue","red","black","grey")
  #colorVec <- colorRamps::green2red(8)
  colorVec <- colorRamps::rgb.tables(nExploitation)

  figs <- c("a","b","c","d","e","f")
  #earlyyears
  png(filename = paste0(wd,"/radarScenario21-30yrs.png"),width=11, height=8.5,unit="in",res=300)
  # assemble data and create radar plot

  #  par(mfrow=c(2,3))
  m <- matrix(c(1:(nScenarioNames+1),nScenarioNames+1,nScenarioNames+1),nrow = 3,ncol = 3,byrow = TRUE)
  #m <- matrix(c(1:8,8,8),nrow = 3,ncol = 3,byrow = TRUE)
  layout(m,heights=c(.48,.48,.04))
  par(mar=c(0,2,0,2)) # margin on plot (bot,left,top,right)
  par(oma=c(0,0,0,0)) # outer margins
  par(xpd=TRUE) # absence of clipping of text to figure area

  for (iscenarioType in 1:length(scenarioTypes)) {
    titleScenario <- scenarioTypes[scenarioOrder[iscenarioType]]
    index <- scenarioMembers[[scenarioOrder[iscenarioType]]]
    dat <- scenarioValuesEarly[index,]
    dat <- rbind(maxminsEarly,dat)
    #if (length(scenarioMembers[[iscenarioType]]) == 5) {
    colorChoice <- colorVec
    #} else {
    #  colorChoice <- colorVec[-1]
    #}

    fmsb::radarchart( dat, axistype=1 ,

                      pcol=colorChoice ,
                      cglcol = "grey",pty=32, plty=1, axislabcol="grey", na.itp=FALSE,vlabels=labelNames, plwd=4,#c("Revenue",indicesNames),
                      #custom the grid
                      caxislabels=c("0%","25%","50%","75%","100%"), cglty=1,calcex=1.5,
                      #custom labels
                      vlcex=1.2) #size of label
    #,title = titleScenario)
    #    if (iscenarioType == 1) {
    #      legend(1.8,1, legend=titleNames[index], seg.len=0.5, pch=1,
    #             bty="n" ,lwd=3, y.intersp=1.8, horiz=FALSE,col=colorChoice,cex=1.4)
    #    }
    #    if (iscenarioType == 0){
    #      mtext("Control Rule Scenarios",side=3,line=2.5,cex=1.5)
    #    }
    # add title text in not standard position
    text(0,1.5,paste0("(",figs[iscenarioType],") ",titleScenario),cex=2.0)
  }
  plot(1,type="n",axes=FALSE,ylab="",xlab="")
  #legend(x="top",legend=c("0.15","0.2","0.25","0.3"),col = colorVec,lty=1,lwd=4,horiz=TRUE)
  legend(x="top",legend=c(".05",".10","0.15","0.2","0.25","0.3",".35",".4"),col = colorVec,lty=1,lwd=4,horiz=TRUE)
  #legend(x="top",legend=c(".10","0.15","0.2","0.25","0.3",".35"),col = colorVec,lty=1,lwd=4,horiz=TRUE)
  dev.off()



  ## late years
  png(filename = paste0(wd,"/radarScenario41-50yrs.png"),width=11, height=8.5,unit="in",res=300)
  # assemble data and create radar plot
  #par(mfrow=c(2,3))
  layout(m,heights=c(.48,.48,.04))
  par(mar=c(0,2,0,2)) # margin on plot (bot,left,top,right)
  par(oma=c(0,0,0,0)) # outer margins
  par(xpd=TRUE) # absence of clipping of text to figure area


  for (iscenarioType in 1:length(scenarioTypes)) {
    titleScenario <- scenarioTypes[scenarioOrder[iscenarioType]]
    index <- scenarioMembers[[scenarioOrder[iscenarioType]]]
    #titleScenario <- scenarioTypes[iscenarioType]
    #index <- scenarioMembers[[iscenarioType]]
    dat <- scenarioValuesLate[index,]
    dat <- rbind(maxminsLate,dat)

    #if (length(scenarioMembers[[iscenarioType]]) == 5) {
    colorChoice <- colorVec
    #  } else {
    #    colorChoice <- colorVec[-1]
    #  }


    fmsb::radarchart( dat, axistype=1 ,
                #custom polygon
                pcol=colorChoice ,
                cglcol = "grey",pty=32, plty=1, axislabcol="grey", na.itp=FALSE,vlabels=labelNames, plwd=4,
                #custom the grid
                caxislabels=c("0%","25%","50%","75%","100%"), cglty=1,calcex=1.5,

                #custom labels
                vlcex=1.2)
    #,title=titleScenario)

    #    if (iscenarioType == 1) {
    #     legend(1.8,1, legend=titleNames[index], seg.len=0.5, pch=1,
    #         bty="n" ,lwd=3, y.intersp=1.8, horiz=FALSE,col=colorChoice,cex=1.4)
    #    }


    #  if (iscenarioType == 0){
    #    mtext("Control Rule Scenarios",side=3,line=-.5,cex=1.6)
    #  }
    # add title text in not standard position
    #text(0,1.5,titleScenario,cex=2)
    text(0,1.5,paste0("(",figs[iscenarioType],") ",titleScenario),cex=2.0)

  }
  plot(1,type="n",axes=FALSE,ylab="",xlab="")
  #legend(x="top",legend=c("0.15","0.2","0.25","0.3"),col = colorVec,lty=1,lwd=4,horiz=TRUE)
  legend(x="top",legend=c(".05",".10","0.15","0.2","0.25","0.3",".35",".4"),col = colorVec,lty=1,lwd=4,horiz=TRUE)
  #legend(x="top",legend=c(".10","0.15","0.2","0.25","0.3",".35"),col = colorVec,lty=1,lwd=4,horiz=TRUE)
  dev.off()



}
