#' Dichotomous TIF
#'
#' This function calculates the test information function for an item bank


xPL_TIF <- function(ipar,thetaGrid=seq(-4.5,4.5,.1)){
  n.items <- nrow(ipar)
  item.info <- matrix(rep(0, length(thetaGrid)*n.items), nrow=length(thetaGrid), ncol=n.items)
  test.info <- rep(0, length(thetaGrid))
  for(i in 1:n.items){
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
  return(list(item.info=item.info, test.info=test.info))
}
