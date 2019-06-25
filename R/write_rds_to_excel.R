#'Write RDS files to excel
#'
#'Converts output in RDS files to XLSX files
#'
#'@param filePath Character string. The path to the directory containing the files to process
#'@param filenames Character string. A vecor of filenames to convert to excel
#'
#'@return Excel files are created in folder \code{filePath}
#'
#'@export


write_rds_to_excel <- function(filePath,filenames) {

  OUT <- openxlsx::createWorkbook()
  # split filename, read in , write out 3rd dimention to a a separate sheet in excel
  for (fname in filenames) {
    fn <- head(unlist(strsplit(fname,"\\.")),1)
    fileContents <- readRDS(paste0(filePath,"/",fname))

    objectNames <- as.character(unique(fileContents$Type))

    for (isheet in 1:length(objectNames)) {
      openxlsx::addWorksheet(OUT,objectNames[isheet])
      outData <- fileContents %>% dplyr::filter(Type == objectNames[isheet])
      openxlsx::writeData(OUT,sheet = objectNames[isheet],x=outData,rowNames=F,colNames=T)
      #xlsx::write.xlsx(outData,file=paste0(filePath,"/",fn,".xlsx"),sheetName=objectNames[isheet],row.names=F,col.names=T,append=T)
    }
  }
  openxlsx::saveWorkbook(OUT,file=paste0(filePath,"/",fn,".xlsx"),overwrite = T)
}
