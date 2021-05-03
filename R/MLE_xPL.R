#' Dichotmous MLE
#'
#' This function calculates the maximum likelihood estimate for dichotomous items. It is based off of the code provided in Baker and Kim 2017.
#' However, it includes modifications for slope/intercept (as opposed to slope/threshold) parameterization,
#' though it maintains original-scale guessing parameterization. It supports the Rasch/1PL, 2PL, and 3PL models provided that
#' they are appropriately parameterized for this function.
#'
#' @param ipar A matrix with rows for items and three columns. The first column is the slope,
#' the second is the intercept (not the threshold),
#' and the third is the guessing parameter (on the original scale, not on the logit scale).
#' @param u A vector of response strings, coded 0 for incorrect and 1 for correct.
#' The length of this vector must be equal to the number of rows (i.e., the number of items) in ipar.
#' @param st_th Starting theta to initialize the Newton Raphson method. Defaults to 0.
#' @param crit Convergence criteria for Newton Raphson method. Defaults to 0.001.
#' @param maxIter Maximum number of iterations for the Newton Raphson method. Defaults to 100.
#' @param minTheta Minimum permissible theta value. Defaults to -10.
#' @param maxTheta Maximum permissible theta value. Defaults to +10.
#'
#' @return Returns a data.frame with the theta estimate at convergence (or minTheta or maxTheta if convergence was not met),
#' and the standard error associated with that estimate (which is Inf if not converged),
#' and the number of iterations taken by the Newton Raphson algorithm to reach convergence (or maxIter if not converged).
#'
#' @export
#'
#' @family dichotomous functions

MLE_xPL <- function(ipar, u, st_th=0,crit=.001,maxIter = 100, minTheta=-10, maxTheta=10){
  if(tibble::is_tibble(ipar)) ipar <- as.data.frame(ipar)
  nItems <- nrow(ipar)
  if(length(u) != nItems) stop("Unequal number of items and responses")
  delta <- 10 # initialize difference at unrealistic value
  th_1 <- st_th # th and th_1 are used below in the scoring loop where th is the current score and th_1 is score on the next loop
  niter <- 0
  if(sum(u) == nItems){ # exclude undefined response patterns
    output <- data.frame(Theta=Inf,SEM=NaN,iter=0)
  } else if(sum(u) == 0){
    output <- data.frame(Theta= -Inf,SEM=NaN,iter=0)
  } else {
    while(delta > crit & niter <= maxIter){
      th <- as.numeric(th_1)
      niter <- niter+1
      P <- Q <- W <- vector() # let P equal the probability of a correct response
      div1 <- div2 <- 0 # set up the first and second derivatives
      Pstar <- Qstar <- vector() # only used for 3PL
      for(i in 1:nItems){
        if(ipar[i,3] == 0){ # for 1PL & 2PL
          P[i] <- 1/(1+exp(-(ipar[i,2]+ipar[i,1]*th)))
          Q[i] <- 1-P[i]
          W[i] <- P[i] * Q[i]
          div1 <- div1 + ipar[i,1]*u[i] - ipar[i,1]*P[i]
          div2 <- div2 - ipar[i,1]^2*W[i]
        } else {
          Pstar[i] <- 1/(1+exp(-(ipar[i,2]+ipar[i,1]*th)))
          Qstar[i] <- 1 - Pstar[i]
          P[i] <- ipar[i,3] + (1-ipar[i,3]) * Pstar[i]
          Q[i] <- (1-ipar[i,3])*Qstar[i]
          W[i] <- (Pstar[i] * Qstar[i]) / (P[i]*Q[i])
          div1 <- div1 + ipar[i,1]*(u[i] - P[i])*(Pstar[i]/P[i])
          div1 <- as.numeric(div1)
          div2 <- div2 - ipar[i,1]^2 * P[i] * Q[i] * (Pstar[i]/P[i])^2
          # following Baker et al., this is the expected value of the second derivative, not the observed value
          # as is appropriate for the 3PL model
        }
      }
      th_1 <- th - div1/div2 # get the next value for theta
      delta <- abs(th - th_1) # get the difference between current and next value of theta
      if(th_1 < minTheta) th_1 <- minTheta
      if(th_1 > maxTheta) th_1 <- maxTheta
      # cat(P,"\t",div1,"\t",div2,"\t",th_1,"\t", niter, "\t","\n")
    }
    output <- data.frame(Theta=th_1,SEM=sqrt(-1/div2),iter=niter)
    if(niter >= maxIter){
      output <- data.frame(Theta=st_th,SEM=sqrt(-1/div2),iter=niter)
    }
  }
  return(output)
}
