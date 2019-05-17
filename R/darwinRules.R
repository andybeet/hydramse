#' Data used to constrain Darwin model runs within historic bounds
#'
#'Estimates of Stock recruitment model parameters for several different model types and ranges of SSB, landing estimates. All based on historic data
#'Each item has a very similar format. Each item has the same number of rows. Each row representing a species in the model. Further details can be found in the vignette (darwinRules_explained.rmd)
#'
#' @format A list containing 6 elements
#' \describe{
#'   \item{hockey}{}
#'   \item{segmented}{}
#'   \item{BH}{}
#'   \item{Ricker}{}
#'   \item{SSBBounds}{Matrix. }
#'   \item{historicBounds}{Numeric Vector (Nthresholds): The values of exploitation at each threshold (Only used in step function Ramp). This is set in \code{\link{create_datpin_files}}. See \code{flagLinearRamp}}
#'}
#'
#'@seealso To visualize this data please run \code{plot_sr}
#'
#'@source Parameter estimates for stock recuitment models were obtained by A. Beet. Estimates of Landings/catch ans survey bionass was compiled by C. Peretti
"darwinRules"
