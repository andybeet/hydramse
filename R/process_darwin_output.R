#' processes output from Darwin run
#'
#' Reads in hydra output from the specified darwin run
#'
#' @param filePath Character string. Path to where output files exist
#'
#' @return A List:
#' \item{biomass}{matrix (nSpecies x nYrs). Average biomass (avByr) for each species in each year}
#' \item{catch}{matrix (nSpecies x nYrs). Average catch (est_catch_biomass) for each species in each year}
#'
#'
#' @export


process_darwin_output<- function(filePath){
  fname<-list.files(path=filePath,pattern="\\.text$")

  if (length(fname)== 0) {
    stop(paste0("There are no output files to process. Make sure they have been copied to the folder -",filePath))
  } else if (length(fname) > 1) {
    stop(paste0("Only 1 file should have been written"))
  }
  # create a list of names. full path
  f <- paste0(filePath,"/",fname)
  # read in parts of file pertaining to catch and biomass
  fIn <- readLines(f)
  # num years
  lineNum <- grep("Nyrs",fIn)
  nYrs <- as.numeric(fIn[lineNum+1])
  # num species
  lineNum <- grep("guildMembers",fIn)
  nSpecies <- length(strsplit(fIn[lineNum+1]," ")[[1]]) - 1

  #biomass
  lineNum <- grep("avByr",fIn)
  biomass <- matrix(data=NA,nrow=nSpecies,ncol=nYrs+1)
  for (iline in 1:nSpecies) {
    biomass[iline,] <- (as.numeric(strsplit(fIn[lineNum+iline]," ")[[1]]))
  }
  biomass <- biomass[,-1] # remove first column

  #catch
  lineNum <- grep("est_catch_biomass",fIn)
  catch <- matrix(data=NA,nrow=nSpecies,ncol=nYrs+1)
  for (iline in 1:nSpecies) {
    catch[iline,] <- (as.numeric(strsplit(fIn[lineNum+iline]," ")[[1]]))
  }
  catch <- catch[,-1] #remove 1st column

  return(list(catch=catch,biomass=biomass) )

}

