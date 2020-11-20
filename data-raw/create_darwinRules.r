
  # sets up default rules for the darwin simulation study and exports as lazy data
create_darwinRules <- function(){
    # rule choices made by scientist
  darwinRules <- list()
    darwinRules$catchRule <- T
    darwinRules$biomassRule <-  T
    darwinRules$rangeOfBreakpoint <- c(0.5,1.5) # 0.5* distance from estimated point and origin to 1.5*distance from estimated point
    darwinRules$lastNPoints <- 10
    darwinRules$biomassBounds <- c(.5,2)
    darwinRules$catchBounds <- c(1,1)
    darwinRules$RickerRangeOfAlphas <- c(.2,5)
    darwinRules$BHRangeOfAlphas <- c(.2,5)
    darwinRules$SRType <- c(2,2,1,2,1,1,1,1,2,2) # 1 = asymptote , 2 = overcompensation
    darwinRules$otherFood <- seq(10000,60000,5000)

    devtools::use_data(darwinRules,overwrite = TRUE)

}


