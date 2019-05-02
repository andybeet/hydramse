#'Executes Hydra ADMB code
#'
#'Runs the Hydra code
#'
#'@param n Numeric scalar. The seed used for the model run (Default: n = 1)
#'@param exePath Character string. The path to the executable
#'@param datPath Character string. The path to the dat file
#'@param pinPath Character string. The path to the pin file
#'
#'@return Nothing is returned. Output is written to a file for processing
#'
#'
#'
#'@export


run_single_hydra <- function(n=1,exePath,datPath,pinPath) {

  # form string to execute
  runmod<-paste(exePath,"-sim",n," -ind",datPath," -ainp ",pinPath)  # creates string to be run in shell
  # execute string capture output as R object
  system(runmod,intern=T)


  #system("rm variance fmin.log eigv.rpt") #clean up
}
