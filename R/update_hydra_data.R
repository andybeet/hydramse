#' update base Hydra data
#'
#' update Hydra data with values stored in updates
#'
#' @param hydraD List of current base data (as in \code{hydradata::hydradataList})
#' @param updates List containing data with fieldnames identical to those in \code{hydradata::hydradataList} used to update the base data
#'
#' @return List
#' \item{updatedData}{List of \code{hydraD} with updated values}
#'
#' @export

update_hydra_data <- function(hydraD,updates){

  fieldNames <- names(updates)
  if (!all(fieldNames %in% names(hydraD))) {
    stop("Fields exist in update data that don't exist in main data")
  }

  # replace all fields from updates to hydraD
  for (iname in 1:length(fieldNames)) {
    colNms <- names(hydraD[fieldNames[iname]][[1]])
    hydraD[fieldNames[iname]] <- updates[fieldNames[iname]]
    names(hydraD[fieldNames[iname]][[1]]) <- colNms
  }

  return(hydraD)
}
