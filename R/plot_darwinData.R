#'Plots Stock recruitment model
#'
#'Uses parameter values found in lazy loaded data \code{darwinRules}. These models are considered starting points for simulation study
#'FINISH LATER
#'
#'@param data List. lazy data \code{darwinData} is default
#'
#'@importFrom ggplot2 aes
#'@export

plot_darwinData <- function(data=darwinData){
  # create time series then make a tidy dataset for ggploting
  minSSB <- data$SSBBounds$min_obs_SS
  maxSSB <- data$SSBBounds$max_obs_SS
  nSpecies <- length(maxSSB)

  # hockey
  x <-  c(rep(0,nSpecies), data$hockey$shape, maxSSB)
  y <- data$hockey$alpha * x
  ind <- x > data$hockey$shape
  y[ind] <- data$hockey$alpha * data$hockey$shape
  species <- rep(data$hockey$species,3)
  d <- data.frame(species=species,x=x, y=y)
  d$model <- "hockey"


  # segmented

  ggplot2::ggplot(d) +
    ggplot2::geom_line(mapping = aes(x=x,y=y)) +
    ggplot2::facet_wrap(~species,nrow=4,ncol=3,scales="free")



}
