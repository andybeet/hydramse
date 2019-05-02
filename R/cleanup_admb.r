#'Cleans up environment after ADMB calls
#'
#'Removes left over files from a failed run and temporary files created by ADMB
#'
#'@param folderName Character string: Name of the folder to be deleted (Default = NULL)
#'
#'@return Nothing
#'
#'@section Files Removed:
#'
#'All files of type csv, txt, out, text are removed.
#'Temporary files "variance", "fmin.log", "eigv.rpt"
#'
#'
#'@export

cleanup_admb <- function(folderName = NULL){
  system("rm variance fmin.log eigv.rpt") #clean up
  system("rm *.csv") # exports for kraken
  system("rm *.txt") # exports for indices
  system("rm *.out") # exports for diagnostics
  system("rm *.text") # exports for darwinian
  if (!is.null(folderName)) {
    unlink(folderName,recursive = TRUE)
  }

}
