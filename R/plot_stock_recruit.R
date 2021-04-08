#'Plots Stock recruitment model
#'
#'Plots SR models found in lazy loaded data \code{darwinRules}.
#'Overlays with simulated stock recruitment models (optional)
#'
#'@param data List. lazy data \code{darwinData} is default
#'@param stockRecruitData NOT FINISHED
#'
#'@importFrom ggplot2 aes
#'@export

plot_stock_recruit <- function(data=darwinData,stockRecruitData=NULL){
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
  d$source <- "original"

  if (!is.null(stockRecruitData)){
    # simulated hockey
    alpha <- stockRecruitData$alphaHockey
    beta <- stockRecruitData$betaHockey
    breakpoint <- stockRecruitData$shapeHockey
    x <-  c(rep(0,nSpecies), breakpoint, maxSSB)
    y <- alpha * x
    ind <- x > breakpoint
    y[ind] <- alpha * breakpoint
    simd <- data.frame(species=species,x=x, y=y)
    simd$model <- "hockey"
    simd$source <- "simulated"

    d <- rbind(d,simd)
  }

  ggplot2::ggplot(d) +
    ggplot2::geom_line(mapping = aes(x=x,y=y,color=source)) +
    ggplot2::facet_wrap(~species,nrow=4,ncol=3,scales="free")

}
