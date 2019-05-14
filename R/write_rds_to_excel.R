#'Write RDS files to excel
#'
#'Converts output in RDS files to XLSX files
#'
#'@param filePath Character string. The path to the directory containing the files to process
#'@param filenames Character string. A vecor of filenames to convert to excel
#'
#'@return Excel files are created in folder \code{filePath}
#'
#'@importFrom xlsx write.xlsx
#'@export


write_rds_to_excel <- function(filePath,filenames) {
  for (fname in filenames) {
    fn <- head(unlist(strsplit(fname,"\\.")),1)
    fileContents <- readRDS(paste0(filePath,"/",fname))

    nobj <- dim(fileContents)[3]
    for (isheet in 1:nobj) {
      write.xlsx(fileContents[,,isheet],file=paste0(filePath,"/",fn,".xlsx"),sheetName=unlist(dimnames(fileContents)[3])[isheet],row.names=F,col.names=T,append=T)
    }
  }
}
