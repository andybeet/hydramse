#' Time Series data used to constrain Darwin model runs within historic bounds
#'
#' Time series of landings, discards and abundance.
#'
#' @format A Data frame
#' \describe{
#'   \item{YEAR}{Year of data point}
#'   \item{SPECIES}{Common name of species (10 species are modeled in Hydra) }
#'   \item{TYPE}{The type of data. "landings", "survey", "discards"}
#'   \item{VALUE}{The value of the \code{TYPE}, in metric tons}
#'}
#'
#'@seealso To visualize this data please run \code{plot_time_series}
#'
#'@source
"timeSeries"
