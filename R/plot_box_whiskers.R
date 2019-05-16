#' creates box whisker plots
#'
#' Multipanel plots representing species/guild biomass, catch, etc over differnt scenario types
#'
#' @param filePath Character string. Path to rootfolder
#' @param rootFolder Character string. Folder containing all runs output
#' @param inputFile Character string. RDS file to process.
#'
#' @return Plots are written
#' @importFrom magrittr %>%
#'
#' @export

plot_box_whiskers <- function(filePath,rootFolder,inputFile) {

  # plotting values
  cexAxisVal <-  2
  subTitleSize <- 2.5
  titleSize <- 2
  xyLabelsize <- 3
  mtextSizey <- 2
  mtextSizeTitle <- 3
  chooseQuantile <- c(.1,.9)

  # load in all of the data to plot
  data <- readRDS(file = paste0(filePath,"/",rootFolder,"/",inputFile))
  scenarioType <- unique(data$Scenario)
  for (iscenarioTypes in 1:length(scenarioType)) {
    currentScenario <- scenarioType[iscenarioTypes]
    print(paste0("processing plots for Scenario = ",currentScenario))

    scenarioData <- data %>% dplyr::filter(Scenario==currentScenario)
    print(dim(scenarioData))


    # speciesNames <- dimnames(data)[3] # grabs all species names
    # ind <- grepl(currentScenario,unlist(dimnames(data)[2])) # finds scenarios that match the current one
    # scenarioData <- data[,ind,] #then extracts data
    # # splits the column names to get the list of exploitation rates
    # exploitationRates <- unlist(lapply(unlist(dimnames(scenarioData)[2]),function(x) tail(unlist(strsplit(x,split=currentScenario)),1)))




  }
}

#     # find which files represent guild and which species level
#     guildFiles <- grep("guild",tolower(xlsxfileNames))
#     speciesFiles <- grep("species",tolower(xlsxfileNames))
#     numSpecies <- length(speciesNames$names)
#     numGuilds <- length(guildNames$names)
#
#     scenarioStatsSpeciesList <- list()
#
#     # loop over all species level files and plot : nThreshold, bio, catch
#     for (ifile in 1:length(speciesFiles)) {
#       dataToPlot <- list() # list of dataframes
#       filePath <- paste0(path,"/",xlsxfileNames[speciesFiles[ifile]])
#       print(paste0("reading in file: ",xlsxfileNames[speciesFiles[ifile]]))
#       # make all data into a list for plotting
#       for (isp in 1:numSpecies) {
#         dataTo <- xlsx::read.xlsx(filePath,sheetName=speciesNames$names[isp])
#         dataToPlot[[isp]] <- dataTo
#       }
#
#       if (dim(dataToPlot[[1]])[1] == numRuns) {
#         numDiv <- numYrs
#       } else if (dim(dataToPlot[[1]])[1] == numYrs) {
#         numDiv <- numRuns
#       } else {
#         stop("what are the dimensions of the data?: plot_boxwhiskers")
#       }
#
#       # function to summarize data, median, sd, uq, lq
#       scenarioStats <- sub_scenarioStats(dataToPlot,"species",xlsxfileNames[speciesFiles[ifile]])
#       scenarioStatsSpeciesList[[ifile]] <- scenarioStats
#
#
#             # now plot data read in Species
#       if ((grepl("threshold",xlsxfileNames[speciesFiles[ifile]],ignore.case=TRUE ))) {
#         # speice level - proportion of exceedance
#         png(filename=paste0(path,"/BoxPlot_SpeciesExceedance.png"),width=900,height=900,units="px")
#
#         par(mfrow=c(4,3))
#         par(mar=c(2,2,3,2)+0.1)
#         par(oma=c(2,4,4,0))
#         # % exceedance. number of yrs exceed out of 53
#         for (ig in 1:numSpecies) {
#           d <- as.matrix(dataToPlot[[ig]])/numDiv
#           specNames <- speciesNames$names[ig]
#           BeetBoxplot(d,chooseQuantile,specNames,exploitationRates,cexAxisVal,titleSize,c(0,1))
#
#         }
#
#         mtext("Proportion Overfished", outer=T, side=2,line=1.5,cex=mtextSizey)
#
#         dev.off()
#
#       } else if ((grepl("bio",xlsxfileNames[speciesFiles[ifile]],ignore.case=TRUE )))  {
#          # speices level - biomass
#         png(filename=paste0(path,"/BoxPlot_SpeciesBiomass.png"),width=900,height=900,units="px")
#         par(mfrow=c(4,3))
#         par(mar=c(2,2,3,2)+0.1)
#         par(oma=c(2,4,4,0))
#
#         # biomass
#         for (ig in 1:numSpecies) { #,ylim=c(0,maxSpeciesBio)
#           d <- as.matrix(dataToPlot[[ig]])
#           specNames <- speciesNames$names[ig]
#           BeetBoxplot(d,chooseQuantile,specNames,exploitationRates,cexAxisVal,titleSize,range(d))
#
#           #boxplot.matrix(as.matrix(dataToPlot[[ig]]),use.cols=TRUE,range=0,names=exploitationRates,main=speciesNames$names[ig],cex.axis=cexAxisVal,cex.main=titleSize)
#           if (ig == 1) {
#             mtext("Biomass",line=-2,adj=.1,cex=1.5)
#           }
#         }
#
#         mtext("Kilotons", outer=T, side=2, line=1.5,cex=mtextSizey)
#         dev.off()
#
#       } else if ((grepl("catch",xlsxfileNames[speciesFiles[ifile]],ignore.case=TRUE )))  {
#         # speices level - catch
#         png(filename=paste0(path,"/BoxPlot_SpeciesCatch.png"),width=900,height=900,units="px")
#         par(mfrow=c(4,3))
#         par(mar=c(2,2,3,2)+0.1)
#         par(oma=c(2,4,4,0))
#
#         # catch
#         for (ig in 1:numSpecies) { #ylim=c(0,maxSpeciesCatch),
#         #  boxplot.matrix(as.matrix(dataToPlot[[ig]]),use.cols=TRUE,range=0,names=exploitationRates,main=speciesNames$names[ig],cex.axis=cexAxisVal,cex.main=titleSize)
#           d <- as.matrix(dataToPlot[[ig]])
#           specNames <- speciesNames$names[ig]
#           BeetBoxplot(d,chooseQuantile,specNames,exploitationRates,cexAxisVal,titleSize,range(d))
#
#
#           if (ig == 1) {
#             mtext("Catch",line=-2,adj=.1,cex=1.5)
#           }
#         }
#
#         mtext("Kilotons", outer=T, side=2,line=1.5,cex=mtextSizey)
#         dev.off()
#       } else {
#         stop("What's going on!!!")
#       }
#     }
#
#     scenarioStatsGuildList <- list()
#
#
#     # now loop over all guild level stuff to plot
#     # loop over all species level files and plot : nThreshold, bio, catch
#       # now plot guild result/data
#       # guild level - biomass (divide by largest value)
#       png(filename=paste0(path,"/BoxPlot_FunctionalGroup.png"),width=900,height=900,units="px")
#
#       par(mfrow=c(3,4))
#       par(mar=c(2,2,3,2)+0.1)
#       par(oma=c(2,4,4,0))
#
#     for (ifile in 1:length(guildFiles)) {
#       print(paste0("reading in file: ",xlsxfileNames[guildFiles[ifile]]))
#       dataToPlot <- list()
#       filePath <- paste0(path,"/",xlsxfileNames[guildFiles[ifile]])
#       if (grepl("bio",xlsxfileNames[guildFiles[ifile]],ignore.case=TRUE)) {titleText <- "Biomass"}
#       if (grepl("catch",xlsxfileNames[guildFiles[ifile]],ignore.case=TRUE)) {titleText <- "Catch"}
#       if (grepl("Threshold",xlsxfileNames[guildFiles[ifile]],ignore.case=TRUE)) {titleText <- "Proportion Overfished"}
#
#
#       # make all data into a list for plotting
#       for (isp in 1:numGuilds) {
#         dataTo <- xlsx::read.xlsx(filePath,sheetName=guildNames$names[isp])
#         dataToPlot[[isp]] <- dataTo
#       }
#
#       # function to summarize data, median, sd, uq, lq
#       scenarioStats <- sub_scenarioStats(dataToPlot,"guild",xlsxfileNames[guildFiles[ifile]])
#       scenarioStatsGuildList[[ifile]] <- scenarioStats
#
#       # guild level Biomass and catch
#       if (titleText !="Proportion Overfished") {
#
#       for (ig in 1:numGuilds) {
#
#         d <- as.matrix(dataToPlot[[ig]])
#         specNames <- speciesNames$names[ig]
#         BeetBoxplot(d,chooseQuantile,"",exploitationRates,cexAxisVal,titleSize,range(d))
#
#        # boxplot.matrix(as.matrix(dataToPlot[[ig]]),use.cols=TRUE,range=0,names=exploitationRates,cex.axis=cexAxisVal)
#         if (ig == 1) {
#           mtext(titleText,line=-2,adj=.1,cex=1.5)
#         }
#         if (ifile == 1) {title(main = guildNames$names[ig],cex.main=titleSize)}
#       }
#
#       } else {
#       # % exceedance. number of yrs exceeded out of 53
#       for (ig in 1:numGuilds) {
#         d <- as.matrix(dataToPlot[[ig]])/numDiv
#         specNames <- speciesNames$names[ig]
#         BeetBoxplot(d,chooseQuantile,"",exploitationRates,cexAxisVal,titleSize,c(0,1))
#
#         #boxplot.matrix(as.matrix(dataToPlot[[ig]])/numDiv,use.cols=TRUE,range=0,ylim=c(0,1),names=exploitationRates,cex.axis=cexAxisVal)
#         if (ig == 1) {
#           mtext("Proportion Overfished",line=-2,adj=.1,cex=1.5)
#         }
#       }
#       mtext("Kilotons", outer=T, side=2,line=1.5,cex=mtextSizey)
#       dev.off()
#       }
#     } # end guild
#
#     # now we need to concatenate species lists over variables and guild lists over variables
#       guildList <- vector("list", numGuilds)
#       for (ig in 1:numGuilds) {
#         for (ifi in 1:length(guildFiles)) {
#            guildList[[ig]] <- rbind(guildList[[ig]],scenarioStatsGuildList[[ifi]][[ig]])
#         }
#       }
#       guildListScenario[[iscenarioTypes]] <- guildList
#       speciesList <- vector("list", numSpecies)
#       for (isp in 1:numSpecies) {
#         for (ifi in 1:length(speciesFiles)) {
#           speciesList[[isp]] <- rbind(speciesList[[isp]],scenarioStatsSpeciesList[[ifi]][[isp]])
#         }
#       }
#       speciesListScenario[[iscenarioTypes]] <- speciesList
#
#   } # end scenario
#
#   # now we need to concatenate all scenarios and write to file.
#   # reuse lists
#   guildList <- vector("list", numGuilds)
#   for (ig in 1:numGuilds) {
#     for (ifi in 1:length(scenarioType)) {
#       guildList[[ig]] <- cbind(guildList[[ig]],guildListScenario[[ifi]][[ig]])
#     }
#   }
#   speciesList <- vector("list", numSpecies)
#   for (isp in 1:numSpecies) {
#     for (ifi in 1:length(scenarioType)) {
#       speciesList[[isp]] <- cbind(speciesList[[isp]],speciesListScenario[[ifi]][[isp]])
#     }
#   }
#
#   # write to a file
#   for (isp in 1:numSpecies) {
#     xlsx::write.xlsx(speciesList[[isp]],paste0(wd,"/ScenarioStats.xlsx"),sheetName=speciesNames$names[isp],col.names=TRUE,row.names=TRUE,append=TRUE)
#   }
#   for (ig in 1:numGuilds) {
#     xlsx::write.xlsx(guildList[[ig]],paste0(wd,"/ScenarioStats.xlsx"),sheetName=guildNames$names[ig],col.names=TRUE,row.names=TRUE,append=TRUE)
#   }
#
# } # function end
#
#
# #########     creates stats for scenarios
#
# sub_scenarioStats <- function(dataList,aggregateType,variableName) {
#
#   # data golds all of the data on a given sheet in one of the excel files. This will be a series of median values, one for each run
#   # under each exploitation scenario (Fixed, Ramp, etc). Each sheet will be for a species or guild. Each file is of a variable
#
#   scenarioStats <- list()
#   nObjects <- length(dataList)
#   for (i in 1:nObjects) {
#     median <- apply(dataList[[i]],2,median)
#     sd <- apply(dataList[[i]],2,sd)
#     lq <- apply(dataList[[i]],2,quantile,probs=.25)
#     uq <- apply(dataList[[i]],2,quantile,probs=.75)
#     scenarioStats[[i]] <- rbind(median,sd,lq,uq)
#     rnames <- row.names( scenarioStats[[i]])
#     row.names(scenarioStats[[i]]) <- paste0(variableName,"_",rnames)
#
#   }
#
#
#   return(scenarioStats)
# }
#
# BeetBoxplot <- function(d,chooseQuantile,speciesNames,exploitationRates,cexAxisVal,titleSize,dataYLim) {
#
#   percentiles <- apply(d,2,quantile,probs=chooseQuantile)
#   #print(percentiles)
#   out <- boxplot.matrix(d,use.cols=TRUE,main=speciesNames,names=exploitationRates,range=0,ylim=dataYLim,cex.axis=cexAxisVal,cex.main=titleSize)
#   minWhisker <- out$stats[1,]
#   maxWhisker <- out$stats[5,]
#   for (iscen in 1:dim(d)[2]){
#     # whites out the whisker above the new % ile
#     segments(x0=iscen,y0=percentiles[2,iscen],x1=iscen,y1=maxWhisker[iscen],col="white")
#     segments(x0=iscen-.2,y0=maxWhisker[iscen],x1=iscen+.2,y1=maxWhisker[iscen],col="white")
#     # whites out the whisker below the new % ile
#     segments(x0=iscen,y0=percentiles[1,iscen],x1=iscen,y1=minWhisker[iscen],col="white")
#     segments(x0=iscen-.2,y0=minWhisker[iscen],x1=iscen+.2,y1=minWhisker[iscen],col="white")
#     # draws in new segement
#     segments(x0=iscen-.2,y0=percentiles[2,iscen],x1=iscen+.2,y1=percentiles[2,iscen],col="black")
#     segments(x0=iscen-.2,y0=percentiles[1,iscen],x1=iscen+.2,y1=percentiles[1,iscen],col="black")
#   }
#
#
# }
