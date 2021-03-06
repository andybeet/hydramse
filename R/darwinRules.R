#' Data rules used in Darwin simulation study
#'
#' Rules used to validae the use of simulated sets of parameters. Once a set of parameters has been simulated and used in a Hydra model run, the output
#' is analysed to ensure the model resembles historic levels.
#'
#'
#' @format A list containing 6 elements
#' \describe{
#'   \item{catchRule}{Boolean. Decision to implement Catch rule. default = T}
#'   \item{biomassRule}{Boolean. Decision to implement Biomas rules. Default = T}
#'   \item{rangeOfBreakpoint}{Numeric vector. Upper and lower bounds for the relative position of the breakpoint. See vignettes for details.}
#'   \item{lastNPoints}{Numeric scalar. Specifies the number of trailing values (years) used to compare biomass and catch model estimates with historic values}
#'   \item{biomassBounds}{Numeric vector. Upper and lower bounds represnting proportions of min and max observed biomass used in validation rules}
#'   \item{catchBounds}{Numeric vector. Upper and lower bounds represnting proportions of min and max observed landings used in validation rules}
#'   \item{RickerRangeOfAlphas}{Numeric vector. Proportions (upper and lower) of the \code{alpha} parameter for the Ricker type stock recruitment function}
#'   \item{BHRangeOfAlphas}{Numeric vector. Proportions (upper and lower) of the \code{alpha} parameter for the Beverton holt stock recruitment function}
#'   \item{SRType}{Numeric vector. Determins overcompensatory behavior (species specific). Values of 1 (compensatory) or 2 (overcompensatory)}
#'   \item{otherFood}{Numeric vector. Discrete set of other food values used to simulate from}
#'}
#'
#'@seealso To understand how this data is used, vieq the vignette, "Darwinian Process - simulation methods"
#'
"darwinRules"
