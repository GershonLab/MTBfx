#' Calculate EAP Scores
#'
#' This function updates the current probability grid and calculates its mean and standard deviation,
#' which results in the expected a posteriori (EAP) score and one index of the reliability of that score.
#' An alternative reliability indicator would be the standard error calculated from the information matrix
#' but that is not calculated herein. The Mobile Toolbox standard is the SD of the probability grid.
#'
#' Note that this function is normally called within the GRM engine and thus not utilized on its own.
#' It updates the EAP score for one new item. To use this function alone or to conduct pattern-based scoring, one
#' would need to loop over all of the items outside of the function.
#'
#' @param currentProbGrid The probability grid must be supplied to weight the item information matrix.
#' It may be the normal density (e.g., if no items have been calculated), or  it may be the density of the current EAP score estimate.
#' @param item An index for the item which was just administered.
#' @param i_resp The chosen response category, indexed from 1.
#' @param probDiff The difference between category response probabilities, which is returned by the matrixInfoGRM function.
#' @param thetaGrid A vector of theta estimates at which the test information will be calculated.
#' The thetaGrid is not provided by default, insofar as it expected to be passed by the GRM engine
#' to the function. But if this function is intended to be used alone, a reasonable thetaGrid would
#' be a sequence from -4 to +4 incremented by 0.01 (which is the PROMIS defaults).
#'
#' @return This function returns three values. The first is the probability grid, updated after administering/scoring this item.
#' This is a vector the same length as the previous pobability grid (and should be the same length as the thetaGrid).
#' The second element is the estimated theta score using the EAP scoring method. The final element is the standard deviation
#' of the probability grid, which provides an index for the reliability of the score estimate.
#'
#' @examples
#' #
#' sdEAP <- 3
#' thetaGrid <- seq(-4, 4, length.out=121)
#' muEAP <- 0
#' currentProbGrid <- 1/(sdEAP * sqrt(2*pi))*exp((-(thetaGrid - muEAP)^2)/(2*sdEAP^2))
#'
#' # not run
#' # calcEAP(currentProbGrid = currentProbGrid, i_resp = 1, probDiff = , thetaGrid = thetaGrid)
#'
#' @export
#'
#' @family  GRM_CAT functions

calcEAP <- function(currentProbGrid,item, i_resp, probDiff, thetaGrid){
  currentProbGrid_next <- currentProbGrid * probDiff[,item,i_resp]
  estTheta <- sum(currentProbGrid_next * thetaGrid)/sum(currentProbGrid_next)
  estSD <- sqrt(sum(currentProbGrid_next * (thetaGrid - estTheta)^2)/sum(currentProbGrid_next))
  return(list(currentProbGrid=currentProbGrid_next, estTheta=estTheta, estSD=estSD))
}
