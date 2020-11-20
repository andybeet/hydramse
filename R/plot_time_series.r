#'Plots times eries data
#'
#'Plots landings, discards, survey data (lazy loaded data \code{timeSeries}).
#'
#'@param dataset character. Either "discards", "landings", or "survey"
#'
#'@importFrom ggplot2 aes
#'@importFrom magrittr "%>%"
#'
#'@export

plot_time_series <- function(dataset){

  data <- hydramse::timeSeries %>% dplyr::filter(TYPE == dataset)

  data$NAMEORDER <- factor(data$SPECIES,levels=c("SPINY DOGFISH","WINTER SKATE","ATLANTIC HERRING","ATLANTIC COD","HADDOCK","YELLOWTAIL FLOUNDER","WINTER FLOUNDER","ATLANTIC MACKEREL","SILVER HAKE","GOOSEFISH"))

  ggplot2::ggplot(data=data) +
    ggplot2::geom_line(mapping = aes(x=YEAR,y=value)) +
    ggplot2::facet_wrap(~NAMEORDER,nrow=4,ncol=3,scales="free") +
    ggplot2::ggtitle(dataset)

}
