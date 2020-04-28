#' xPL MLE
#'
#' This function calculates the maximum likelihood estimate for dichotomous items

xPL_MLE <- function(ipar, u, st.th=0,crit=.001,maxIter = 100, minTheta=-10, maxTheta=10){
  n.items <- nrow(ipar)
  if(length(u) != n.items) stop("Unequal number of items and responses")
  delta <- 10 # initialize difference at unrealistic value
  th_1 <- st.th # th and th_1 are used below in the scoring loop where th is the current score and th_1 is score on the next loop
  niter <- 0
  if(sum(u) == n.items){ # exclude undefined response patterns
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
      for(i in 1:n.items){
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
  }
  return(output)
}
