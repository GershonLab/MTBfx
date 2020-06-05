#' Calculate the Information Matrix from an Item Bank Calibrated Using the Graded Response Model
#'
#' This function is used with the MobileToolbox GRM engine for item selection. The item parameters
#' need to be in slope/threshold parameterization. Then this fucntion is called by the GRM engine
#' prior to administering items to calculate the information matrix, which can then be used
#' by the maximum posterior weighted information item selection procedure.
#'
#' @param ipar An item parameter matrix in slope/threshold format. Each item is on a separate line.
#' The column names should be "a" and "CB1" through "CB#" for
#' the slope and relevant thresholds. As is required by the GRM model, there will be K-1 thresholds.
#' Additional columns are allowed (e.g., for an item identifier or for number of categories with that item)
#' provided that the column name is not "a" and does not contain "CB" in it.
#' @param thetaGrid A vector of theta estimates at which the test information will be calculated.
#' The thetaGrid is not provided by default, insofar as it expected to be passed by the GRM engine
#' to the function. But if this function is intended to be used alone, a reasonable thetaGrid would
#' be a sequence from -4 to +4 incremented by 0.01 (which is the PROMIS defaults).
#' @param maxCat An integer for the maximum number of categories across all items in the bank.
#'
#' @return A list is returned with two elements. The first is an array with dimensions for the length of the thetaGrid,
#' the number of items, and the maximum number of response categories across the entire bank. It is named probDiff
#' and is the difference between adjacent categories on a category-by-category basis. The second element is a
#' matrix with dimensions the length of the thetaGrid by the number of items. It is called matrixInfo and contains the
#' information for each item calculated along the theta grid.
#'
#' @export
#'
#' @family  GRM_CAT functions



matrixInfoGRM <- function(ipar,thetaGrid,maxCat){
  require(tidyverse)
  probDiff <- array(0,dim=c(length(thetaGrid),nrow(ipar),maxCat))
  matrixInfo <- matrix(0,nrow=length(thetaGrid),ncol=nrow(ipar))
  CB <- ipar[,str_detect(colnames(ipar),'CB')] %>% as.data.frame()
  for(i in 1:nrow(ipar)){
    pStar <- matrix(NA,nrow=length(thetaGrid),ncol=(1+ipar$NCAT[i]))
    pStar[,1] <- 1
    pStar[,ncol(pStar)] <- 0
    for(k in 1:(ipar$NCAT[i]-1)){
      pStar[,k+1] <- 1/(1+exp(-1*ipar$a[i]*(thetaGrid - CB[i,k])))
    }
    for(k in 1:ipar$NCAT[i]){
      probDiff[,i,k] <- pStar[,k] - pStar[,(k+1)]
      tmp <- (ipar$a[i] * (pStar[,k]*(1-pStar[,k]) - pStar[,(k+1)]*(1-pStar[,(k+1)])))^2/probDiff[,i,k]
      tmp[which(is.nan(tmp))] <- 0
      matrixInfo[,i] <- matrixInfo[,i] + tmp
    }
  }
  return(list(probDiff=probDiff,matrixInfo=matrixInfo))
}
