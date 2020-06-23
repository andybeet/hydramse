#'create folders structure to hold model output.
#'
#'The folder structure is dependent on the number and type of scenarios selected in addition to the number of fleets. Every combination of fleet/exploitation is considered. If there are 3 fleets and 8 exploitation levels there will be 8x8x8 = 512 folders. Within each scenario are subfolder to store model output: dat and pin files and/or plots.
#'
#'@param rootFolder Character string: name of folder to hold all output from a single set of parameters
#'@param fleetsNames Character vector: Names of the fleets in the model
#'@param exploitationRates Numeric matrix: Values for the exploitation rates for each fleet. Eg. Column 1 would be the values of exploitation rates from min to max for fllet 1, column 2 for fleet 2, etc.
#'@param create Boolean. To create folders on machine (TRUE), to retrun structure only (F)
#'
#'@return A Character matrix (3 columns):
#'
#'\item{folderStructure}{Column 1 = hcrType, Column 2 = hcrLevels, column 3 = exploitationRate}
#'
#'
#'
#'@export


create_effort_folder_setup <- function(rootFolder,fleetNames,exploitationRates,create=T) {

  # checks to see if folders are set up for an assessment/MSE
  if (create) {
    if (!dir.exists(rootFolder)) {
      dir.create(rootFolder)
    } else {
      stop(paste0("Root folder ",rootFolder, " already exists"))
    }

    # create kraken folder
    if (!dir.exists(paste0(rootFolder,"/krakenInputs/"))) {
      dir.create(paste0(rootFolder,"/krakenInputs/"),recursive = TRUE)
    }
    if (!dir.exists(paste0(rootFolder,"/crashed/"))) {
      dir.create(paste0(rootFolder,"/crashed/"),recursive = TRUE)
    }
  }

  nFleets <- length(fleetNames)

  exploitationRates <- as.list(as.data.frame(exploitationRates))
  folderCombinations <- expand.grid(exploitationRates)

  #string <- paste0("expand.grid(",paste0(rep("exploitationRates",nFleets),collapse = ","),")")
  #folderCombinations <- eval(parse(text=string))
  # combinations
  folderStructure <- NULL
  # create all folders
  for (ifolder in 1:nrow(folderCombinations)) {
    rates <- as.numeric(folderCombinations[ifolder,])
#    rates <- paste0("_",sprintf("%02d",rates))
    rates <- paste0("_",rates)
    folderName <- paste0(fleetNames,rates,collapse="_")
    folderStructure <- rbind(folderStructure,folderName)

    if (create) {
      if (!dir.exists(paste0(rootFolder,"/",folderName))) {
        dir.create(paste0(rootFolder,"/",folderName),recursive = TRUE)
      }
      # inside these folders create subfolders to hold model output files
      if (!dir.exists(paste0(rootFolder,"/",folderName,"/indices"))) {
        dir.create(paste0(rootFolder,"/",folderName,"/indices"),recursive = TRUE)
      }
      if (!dir.exists(paste0(rootFolder,"/",folderName,"/diagnostics"))) {
        dir.create(paste0(rootFolder,"/",folderName,"/diagnostics"),recursive = TRUE)
      }
    }

  }

  return(folderStructure)

}
