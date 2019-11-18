#' creates Line plots of median value with se
#'
#' Multipanel plots representing species/guild biomass, catch, etc over differnt scenario types
#'
#' @param filePath Character string. Path to rootfolder
#' @param rootFolder Character string. Folder containing all runs output
#' @param inputFile Character string. RDS file to process.
#' @param dataType Character string. Used for yaxis label
#' @param scaling binary scalar. Scale to kilotons (T) or not (F)
#' @param outFilename Character string. Name of file to be saved. Default = inputFile.png
#'
#' @return Plots are written
#' @importFrom magrittr %>%
#' @importFrom ggplot2 aes element_text
#'
#' @export

plot_line_median_sd <- function(filePath,rootFolder,inputFile,dataType,scaling = T,outFilename=NULL) {

  # plotting values
  chooseQuantile <- c(.1,.9)

  if (scaling == TRUE){scalingFactor <- 1000}
  else{scalingFactor <- 1}

  if (is.null(outFilename))  {
    sp <- unlist(strsplit(inputFile,".rds"))
    outFilename <- paste0("Line_",sp[1],".png")
    print(outFilename)
  }

  # load in all of the data to plot
  data <- readRDS(file = paste0(filePath,"/",rootFolder,"/",inputFile))
  scenarioType <- unique(data$Scenario)
  for (iscenarioTypes in 1:length(scenarioType)) {
    currentScenario <- scenarioType[iscenarioTypes]
    print(paste0("processing plots for Scenario = ",currentScenario))

    scenarioData <- data %>% dplyr::filter(Scenario==currentScenario)

    p <- ggplot2::ggplot(data = scenarioData) +
      ggplot2::geom_line(mapping = aes(x = Exploitation, y=median(Value/scalingFactor))) +
      ggplot2::facet_wrap(~Type,nrow=4,ncol=3,scales="free_y") +
      ggplot2::scale_x_discrete(labels= as.character(as.numeric(unique(scenarioData$Exploitation))/100))+
      ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggplot2::ylab(dataType)


    ggplot2::ggsave(paste0(filePath,"/",rootFolder,"/plottingData/",currentScenario,"/",outFilename),p,
                    device="png",width = 6, height = 6, units = "in", dpi="print")

  }
}

