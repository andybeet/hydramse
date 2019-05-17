#' Data used to constrain Darwin model runs within historic bounds
#'
#'Estimates of Stock recruitment model parameters for several different model types and ranges of SSB, landing estimates. All based on historic data
#'Each item has a very similar format. Each item has the same number of rows. Each row representing a species in the model. Further details can be found in the vignette (darwinRules_explained.rmd)
#'
#' @format A list containing 6 elements
#' \describe{
#'   \item{hockey}{Data frame. Parameter estimates from hockey stick model}
#'   \item{segmented}{Data frame. Parameter estimates from the generalized hockey stick model}
#'   \item{BH}{Data frame. Parameter estimates from Beverton Holt model (Shepherd model with shape parameter = 1)}
#'   \item{Ricker}{Data frame. Parameter estimates from Ricker (type) model (Shepherd model with shape parameter = 2)}
#'   \item{SSBBounds}{Data frame. min and max "observed" SSB for each species}
#'   \item{historicBounds}{Data frame. min and max "observed" landings and survey data for each species}
#'}
#'
#'@seealso To visualize this data please run \code{plot_sr}
#'
#'@source Parameter estimates for stock recuitment models were obtained by A. Beet. Estimates of Landings/catch ans survey bionass was compiled by C. Peretti
"darwinData"
