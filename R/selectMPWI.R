#' Maximum Posterior Weighted Information (MPWI) Item Selection
#'
#' This function is utilized by the GRM engine to select the next item using the MPWI
#' item selection method. It is called within the engine, but can be used independently, if so desired.
#'
#' @param matrixInfo This is the information matrix for the item bank calulated using the matrixInfoGRM() function.
#' The information matrix should have been called by the GRM engine prior to item selection. But if used alone,
#' this will need to be applied.
#' @param currentProbGrid The probability grid must be supplied to weight the item information matrix.
#' It may be the normal density (e.g., if no items have been calculated), or it may be the density of the current EAP score estimate.
#' @param nItems The number of items in the item bank.
#'
#' @return After running MPWI item selection, a vector is returned equal in length to the number of items in the item bank.
#' The values are item information (across all categories) weighted by the current probability grid.
#'
#' @export
#'
#' @family  GRM_CAT functions

selectMPWI <- function(matrixInfo, currentProbGrid, nItems){
  tmpInfo <- vector(length=nItems)
  tmpInfo <- colSums(matrixInfo * currentProbGrid)
  return(tmpInfo)
}
