#' plots production functions
#'
#' multipanel plots the production function for each guild /scenario combination
#'
#' @param filePath Character string. Path to rootfolder
#' @param rootFolder Character string. Folder containing all runs output
#' @param dataType Character string. Used for yaxis label
#' @param scaling binary scalar. Scale to kilotons (T) or not (F)
#' @param nYrs numeric scalar. number of trailing years to use for calculations. Default = 20 (Last 20 yrs of sim)
#'
#' @return Figures are written to rootFolder (1 figure per Guild)
#'
#'
#' @export



#hydramse::plot_production_function_guild(here::here(),rootFolder,inputFile="species_bio_rate.rds",dataType="Biomass")

plot_production_function_guild <- function(filePath,rootFolder,dataType,scaling = T,nLastYrs=20) {

  chooseQuantile <- c(.25,.75)

  if (scaling == TRUE){scalingFactor <- 1000}
  else{scalingFactor <- 1}

  # load in all of the data to plot
  if (tolower(dataType) == "biomass"){
    data <- dplyr::as_tibble(readRDS(file = paste0(filePath,"/",rootFolder,"/guild_bio_rate.rds")))
    outFilename <- paste0("AnnualBiomassLast",nLastYrs,"_")
  } else if (tolower(dataType) == "catch") {
    data <- dplyr::as_tibble(readRDS(file = paste0(filePath,"/",rootFolder,"/guild_catch_rate.rds")))
    outFilename <- paste0("productionFunctionLast",nLastYrs,"_")
  } else {
    stop(paste0("Not coded for dataType = ",dataType,". Only Biomass or Catch"))
  }

  guildNames <- unique(data$Type)

  for (aguild in guildNames) {
    guildData <- data %>% dplyr::filter(Type == aguild & Year > (max(data$Year)-nLastYrs))
    guildData <- guildData %>%
      dplyr::group_by(Scenario,Exploitation) %>%
      dplyr::mutate(meanVal = mean(Value/scalingFactor)) %>%
      dplyr::select(Scenario,Exploitation,meanVal) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()

    p <- ggplot2::ggplot(data = guildData) +
      ggplot2::geom_line(mapping = aes(x = as.numeric(Exploitation), y=meanVal)) +
      ggplot2::facet_wrap(~as.factor(Scenario),nrow=2,ncol=3) +
      # ggplot2::scale_x_discrete(labels= as.character(as.numeric(unique(scenarioData$Exploitation))/100))+
      # ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggplot2::ylab(dataType) +
      ggplot2::ggtitle(aguild)


    ggplot2::ggsave(paste0(filePath,"/",rootFolder,"/",outFilename,aguild,".png"),p,
                     device="png",width = 6, height = 6, units = "in", dpi="print")
  }



}
