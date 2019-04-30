#'create folders structure to hold model output.
#'
#'The folder structure is dependent on the number and type of scenarios selected. Within each scenario are subfolder to store model output
#'dat and pin files and/or plots. Additional folders are also created ofr summary statistics and plots.
#'
#'@param rootFolder Character string: name of folder to hold all output from a single set of parameters
#'@param hcrTypes Character vector: Names for the harvest control rule types (Default = c("Fixed, "Ramp"))
#'@param hcrLevels Character vector: Names for the levels that the harvest control rules are applied to. (Default = c("Complex","HTSpecies","LTSpecies"))
#'@param exploitationRates Numeric vector: Values for the maximum exploitation rate in any scenario.
#'
#'@return A Character matrix (3 columns):
#'
#'\item{folderStructure}{Column 1 = hcrType, Column 2 = hcrLevels, column 3 = exploitationRate}
#'
#'
#'
#'@export


create_folder_setup <- function(rootFolder,hcrTypes=c("Fixed","Ramp"),hcrLevels=c("Complex","HTSpecies","LTSpecies"),exploitationRates) {

  # checks to see if folders are set up for an assessment/MSE
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
  folderStructure <- NULL
  # create plottingData folder to hold summary stats and plots withing scenarioType subfolders
  for (iType in 1:length(hcrTypes)) {
    for (iRule in 1:length(hcrLevels)) {
      plottingFolderName <- paste0(hcrTypes[iType],hcrLevels[iRule])

      # creates the folders inside the plotting data folder
      if (!dir.exists(paste0(rootFolder,"/plottingData/",plottingFolderName))) {
        dir.create(paste0(rootFolder,"/plottingData/",plottingFolderName),recursive = TRUE)
      }
      v <- c(hcrTypes[iType],hcrLevels[iRule])
      # creates individual folders for each exploitation/scenario combo
      for (iRate in 1:length(exploitationRates)) {
        if (exploitationRates[iRate] < 10) {
          folderName <- paste0("Exploitation",hcrTypes[iType],hcrLevels[iRule],"0",exploitationRates[iRate])
          folderStructure <- rbind(folderStructure,c(v,paste0("0",exploitationRates[iRate])))
        } else {
          folderName <- paste0("Exploitation",hcrTypes[iType],hcrLevels[iRule],exploitationRates[iRate])
          folderStructure <- rbind(folderStructure,c(v,exploitationRates[iRate]))

        }
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
  }
  colnames(folderStructure) <- c("hcrType","hcrLevels","exploitationRate")

  return(as.data.frame(folderStructure,stringsAsFactors=F))




}
