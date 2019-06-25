#' plot output from darwin simulation runs
#'
#' Used to explore output (catch and biomass) from simulation study to visualize issues
#'
#'@param data tidy data frame (Species, Year, type). type = Biomass or Catch.
#'@param yVar Character string. Name of Variable to be plotted ("Biomass" or "Catch")
#'@param nYrsFishing Numeric scalar. Number of fishing years
#'@param simulationRules List. List of rules used in simulation process (\code{\link{darwinRules}})
#'
#'
#'@export

plot_darwin_output <- function(data,yVar,nYrsFishing,simulationRules) {

  nSpecies <- length(unique(data$Species))
  nYrs <- dim(data)[1]/nSpecies
  nYrsNofishing <- nYrs - nYrsFishing
  dataType <- tail(names(data),1)

  #plot biomass

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(mapping = ggplot2::aes_(x=as.name("Year"),y=as.name(yVar)),na.rm=T) +
    ggplot2::facet_wrap(~Species,nrow=4,ncol=3,scales="free_y")

  print(p)
  # for (ispecies in 1:nSpecies) {
  #   plot(biomass[ispecies,],type="l",col="black",main=startingRules$species[ispecies],ylim=c(0,max(biomass[ispecies,])),xlab="time",ylab="metric")
  #   lines(c(1,length(biomass[ispecies,])),rep(0.5*min(startingRules$min_survey[ispecies],startingRules$CurtiThesis_minBio[ispecies],na.rm=T),2),col="red")
  #   lines(c(1,length(biomass[ispecies,])),rep(2*max(startingRules$max_survey[ispecies],startingRules$CurtiThesis_maxBio[ispecies],na.rm=T),2),col="green")
  #   lines(c(nYrsNofishing,nYrsNofishing),c(min(biomass[ispecies,]),max(biomass[ispecies,])),col = "black",lty=2)
  # }
  # mtext("Biomass",side=3,outer=TRUE)
  #
  # #plot catch
  # #dev.set(catchPlot)
  # par(mfrow=c(3,4))
  # par(mar=c(2,2,3,2)+0.1)
  # par(oma=c(2,4,4,0))
  # for (ispecies in 1:nSpecies) {
  #
  #   plot(catch[ispecies,],type="l",col="black",main=startingRules$species[ispecies],xlab="time",ylab="metric",ylim=c(0,max(catch[ispecies,],na.rm=T)))
  #   lines(c(1,length(catch[ispecies,])),rep(startingRules$minLandings[ispecies],2),col="red")
  #   lines(c(1,length(catch[ispecies,])),rep(startingRules$maxLandings[ispecies],2),col="green")
  #   title()
  #   #     lines(c(100,100),c(min(biomass[ispecies,]),max(biomass[ispecies,])),col = "black",lty=2)
  #
  # }
  # mtext("Catch",side=3,outer=TRUE)



}







#
#
# plot_biomassCatch <- function(biomass,catch,nSpecies,nYrsNofishing,startingRules) {
#
#   #plot biomass
#   #dev.set(biomassPlot)
#   par(mfrow=c(3,4))
#   par(mar=c(2,2,3,2)+0.1)
#   par(oma=c(2,4,4,0))
#   # print(dim(biomass))
#
#
#   for (ispecies in 1:nSpecies) {
#     plot(biomass[ispecies,],type="l",col="black",main=startingRules$species[ispecies],ylim=c(0,max(biomass[ispecies,])),xlab="time",ylab="metric")
#     lines(c(1,length(biomass[ispecies,])),rep(0.5*min(startingRules$min_survey[ispecies],startingRules$CurtiThesis_minBio[ispecies],na.rm=T),2),col="red")
#     lines(c(1,length(biomass[ispecies,])),rep(2*max(startingRules$max_survey[ispecies],startingRules$CurtiThesis_maxBio[ispecies],na.rm=T),2),col="green")
#     lines(c(nYrsNofishing,nYrsNofishing),c(min(biomass[ispecies,]),max(biomass[ispecies,])),col = "black",lty=2)
#   }
#   mtext("Biomass",side=3,outer=TRUE)
#
#   #plot catch
#   #dev.set(catchPlot)
#   par(mfrow=c(3,4))
#   par(mar=c(2,2,3,2)+0.1)
#   par(oma=c(2,4,4,0))
#   for (ispecies in 1:nSpecies) {
#
#     plot(catch[ispecies,],type="l",col="black",main=startingRules$species[ispecies],xlab="time",ylab="metric",ylim=c(0,max(catch[ispecies,],na.rm=T)))
#     lines(c(1,length(catch[ispecies,])),rep(startingRules$minLandings[ispecies],2),col="red")
#     lines(c(1,length(catch[ispecies,])),rep(startingRules$maxLandings[ispecies],2),col="green")
#     title()
#     #     lines(c(100,100),c(min(biomass[ispecies,]),max(biomass[ispecies,])),col = "black",lty=2)
#
#   }
#   mtext("Catch",side=3,outer=TRUE)
#
#
#
# }
#
