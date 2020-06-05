#' Dichotomous TIF
#'
#' This function calculates the test information function for an item bank.
#' It supports the Rasch/1PL, 2PL, and 3PL models provided that they are appropriately parameterized for this function.
#'
#' @param ipar A matrix with rows for items and three columns. The first column is the slope,
#' the second is the intercept (not the threshold),
#' and the third is the guessing parameter (on the original scale, not on the logit scale).
#' @param thetaGrid A vector of theta estimates at which the test information will be calculated.
#' This defaults to a vector from -4.5 to 4.5, incremented by 0.1.
#'
#' @return This function returns a list. The first element is a matrix with the item information.
#' The thetaGrid forms the rows, and the number of items forms the columns.
#' The second element of the list is a vector the same length as thetaGrid. It is the test information function estimated at each point along the thetaGrid.
#'
#' @export



xPL_TIF <- function(ipar,thetaGrid=seq(-4.5,4.5,.1)){
  nItems <- nrow(ipar)
  item.info <- matrix(rep(0, length(thetaGrid)*nItems), nrow=length(thetaGrid), ncol=nItems)
  test.info <- rep(0, length(thetaGrid))
  for(i in 1:nItems){
    if(ipar[i,3] == 0){
      P <- 1/(1+exp(-(ipar[i,2]+ipar[i,1]*thetaGrid)))
      Q <- 1 - P
      item.info[,i] <- ipar[i,1]^2 * P * Q
      test.info <- test.info + item.info[,i]
    } else {
      Pstar <- 1/(1+exp(-(ipar[i,2]+ipar[i,1]*thetaGrid)))
      P <- ipar[i,3] + (1-ipar[i,3]) * Pstar
      item.info[,i] <- ipar[i,1]^2 * P * (1 - P) * (Pstar/P)^2
      test.info <- test.info + item.info[,i]
    }
  }
  return(list(itemInfo=item.info, testInfo=test.info))
}
