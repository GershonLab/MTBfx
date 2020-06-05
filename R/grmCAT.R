#' MTB Graded Response Model Engine
#'
#' This function is a wrapper to simulate the graded response model engine programmed within MobileToolbox.
#' It utilizes matrixInfoGRM, selectMPWI, and calcEAP funtions for item selection and scoring.
#'
#' @param ipar
#' @param resp
#' @param minTheta
#' @param maxTheta
#' @param nQpts
#' @param prevTheta
#' @param prevSD
#' @param muEAP
#' @param sdEAP
#' @param scoreCumulativeDiffuse
#' @param start_item
#' @param minNI
#' @param maxSD
#' @param maxNI
#' @param deltaSD
#'
#' @return This function returns a list with five elements. The first two elements are scalar elements with
#' the final theta estimate and the standard deviation of the score elements.The third element is a dataframe
#' with the item administration history. The first row is the prior. The second row through the final are
#' the items administered, the chosen response category score, and the theta/sd history. The fourth and fifth
#' elements are the probability differences array and the item information matrix,
#' both of which are calculated by the the matrixInfoGRM function.
#'
#' @export
#'
#' @family GRM_CAT functions


grmCAT <- function(ipar, resp,minTheta=-6,maxTheta=6,nQpts=121,prevTheta=NULL,
                      prevSD=NULL,muEAP=0,sdEAP=3,scoreCumulativeDiffuse=TRUE,
                      start_item=0,minNI=4,maxSD=0.3,maxNI=8,deltaSD=0.01){
  nItems <- nrow(ipar)
  thetaGrid <- seq(minTheta,maxTheta,length.out=nQpts)
  maxCat <- max(ipar$NCAT)
  useprobs <- matrixInfoCalc(ipar,thetaGrid,maxCat)
  if(!scoreCumulativeDiffuse | (scoreCumulativeDiffuse & is.null(prevTheta))){
    currentProbGrid <- 1/(sdEAP * sqrt(2*pi))*exp((-(thetaGrid - muEAP)^2)/(2*sdEAP^2))
  } else {
    currentProbGrid <- 1/(sdEAP * sqrt(2*pi))*exp((-(thetaGrid - prevTheta)^2)/(2*sdEAP^2))
  }
  itemAvail <- rep(TRUE,nItems)
  nGiven <- 1
  thetaHistory <- ifelse(scoreCumulativeDiffuse & !is.null(prevTheta), prevTheta, muEAP)
  sdHistory <- sdEAP
  itemAdminHistory <- vector()
  scores <- vector()
  if(start_item == 0){
    iinfo <- selectMPWI(useprobs$matrixInfo,currentProbGrid,nItems)
    iiorder <- order(iinfo, decreasing=T)
    itemAdmin <- iiorder[itemAvail[iiorder]][1]
    scores <- as.numeric(resp[itemAdmin])
    itemAvail[itemAdmin] <- F
    itemAdminHistory <- c(itemAdminHistory, itemAdmin)
  } else {
    itemAdmin <- start_item
    scores <- as.numeric(resp[start_item])
    itemAvail[start_item] <- F
    itemAdminHistory <- c(itemAdminHistory, start_item)
  }
  EAPs <- calcEAP(currentProbGrid, itemAdminHistory[length(itemAdminHistory)],
                  scores[length(scores)], probDiff=useprobs$probDiff, thetaGrid)
  thetaHistory <- c(thetaHistory, EAPs$estTheta)
  sdHistory <- c(sdHistory, EAPs$estSD)
  currentProbGrid <- EAPs$currentProbGrid
  while(nGiven < maxNI & length(which(itemAvail)) > 0){
    nGiven <- nGiven + 1
    iinfo <- selectMPWI(useprobs$matrixInfo,currentProbGrid,nItems)
    iiorder <- order(iinfo, decreasing=T)
    itemAdmin <- iiorder[itemAvail[iiorder]][1]
    scores <- c(scores,as.numeric(resp[itemAdmin]))
    itemAvail[itemAdmin] <- F
    itemAdminHistory <- c(itemAdminHistory, itemAdmin)
    EAPs <- calcEAP(currentProbGrid, itemAdminHistory[length(itemAdminHistory)],
                    scores[length(scores)], probDiff=useprobs$probDiff, thetaGrid)
    thetaHistory <- c(thetaHistory, EAPs$estTheta)
    sdHistory <- c(sdHistory, EAPs$estSD)
    currentProbGrid <- EAPs$currentProbGrid
    if(nGiven >= minNI & (EAPs$estSD < maxSD | (sdHistory[nGiven] - sdHistory[(nGiven+1)] > 0 &
                                                sdHistory[nGiven] - sdHistory[(nGiven+1)] < deltaSD)))
      break
  }
  output <- list(finalTheta=thetaHistory[(nGiven+1)],
                 finalSD=sdHistory[(nGiven+1)],
                 scoreHistory = data.frame(itemAdminHistory=c('Prior',itemAdminHistory),itemScores=c(NA,scores),thetaHistory,sdHistory),
                 probDiff=useprobs$probDiff,
                 matrixInfo=useprobs$matrixInfo)
}
