#' processes output from Darwin run
#'
#' Reads in hydra output from the specified darwin run
#'
#' @param filePath Character string. Path to where output files exist
#' @param speciesList Character string. List of species in order
#'
#' @return A List:
#' \item{biomass}{tidy data frame.(nSpecies * nYrs x 2). Average biomass (avByr) for each species in each year}
#' \item{catch}{tidy data frame (nSpecies * nYrs x 2). Average catch (est_catch_biomass) for each species in each year}
#'
#'
#' @export


process_darwin_output<- function(filePath,speciesList){
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
  rownames(biomass) <- speciesList
  colnames(biomass) <- c(1:nYrs)
  biomass <- tibble::rownames_to_column(as.data.frame(biomass))
  biomass <- tidyr::gather(biomass,Year,Biomass, -rowname)
  biomass$Year <- as.numeric(biomass$Year)
  names(biomass)[1] <- "Species"

  #catch
  lineNum <- grep("est_catch_biomass",fIn)
  catch <- matrix(data=NA,nrow=nSpecies,ncol=nYrs+1)
  for (iline in 1:nSpecies) {
    catch[iline,] <- (as.numeric(strsplit(fIn[lineNum+iline]," ")[[1]]))
  }
  catch <- catch[,-1] #remove 1st column
  rownames(catch) <- speciesList
  colnames(catch) <- c(1:nYrs)
  catch <- tibble::rownames_to_column(as.data.frame(catch))
  catch <- tidyr::gather(catch,Year,Catch, -rowname)
  catch$Year <- as.numeric(catch$Year)
  names(catch)[1] <- "Species"

  return(list(catch=catch,biomass=biomass))
}

