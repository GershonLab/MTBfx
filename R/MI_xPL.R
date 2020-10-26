#' Dichotomous Max Info
#'
#' This function takes parameters from a dichotomous item bank and a target theta estimate.
#' It calculates the item information at the target theta values (using the TIF_xPL function),
#' and organizes the items from maximum to minimum information provided.
#'
#' @param iparFull A matrix with rows for items and three columns. The first column is the slope,
#' the second is the intercept (not the threshold),
#' and the third is the guessing parameter (on the original scale, not on the logit scale).
#' @param targTheta The target theta at which the item information is to be calculated and the next item chosen.
#'
#' @return This function returns a vector of integers indicating row numbers from iparFull with the maximum information provided at the target theta value.
#'
#' @export
#'
#' @family dichotomous functions

maxInfo_xPL <- function(iparFull, targTheta){
  if(tibble::is_tibble(iparFull)) iparFull <- as.data.frame(iparFull)
  ItemInfo <- TIF_xPL(iparFull, thetaGrid=targTheta)

  ItemOrder <- order(ItemInfo$itemInfo, decreasing=TRUE)

  return(ItemOrder)
}
