#' MTB Graded Response Model Engine
#'
#' This function is a wrapper to simulate the graded response model engine programmed within MobileToolbox.
#' It utilizes matrixInfoCalc, selectMPWI, and calcEAP funtions for item selection and scoring.
#'
#' @param ipar An item parameter matrix in slope/threshold format. Each item is on a separate line.
#' The column names should be "a" and "CB1" through "CB#" for
#' the slope and relevant thresholds.
#' @param resp Response matrix (number of rows = number of participants and number of columns = number of items)
#' @param minTheta Minimum theta value (PROMIS default is -4)
#' @param maxTheta Maximum theta value (PROMIS default is 4)
#' @param nQpts number of quadrature points, with min minTheta and max maxTheta
#' @param prevTheta estimated theta from prior CAT administration
#' @param prevSD estimated SD from prior CAT administration
#' @param muEAP Estimated A Posterior mean. Defaults to NULL.
#' @param sdEAP Estimated A Posterior SD. Defaults to NULL.
#' @param scoreCumulativeDiffuse
#' @param start_item First item to start the CAT engine.
#' @param minNI Minimum number of items to administer. Defaults to 4.
#' @param maxSD Maximum SD. Defaults to 0.3.
#' @param maxNI Maximum number of items to administer. Defaults to 8.
#' @param deltaSD Mininum change for SD.
#'
#' @return This function returns a list with five elements. The first two elements are scalar elements with
#' the final theta estimate and the standard deviation of the score elements.The third element is a dataframe
#' with the item administration history. The first row is the prior. The second row through the final are
#' the items administered, the chosen response category score, and the theta/sd history. The fourth and fifth
#' elements are the probability differences array and the item information matrix,
#' both of which are calculated by the the matrixInfoGRM function.
#' @example
#' # define item parameter bank; number of rows is the number of columns
#' # a = item slope; CB1 to CB# = category bins/thresholds for response; NCAT = number of categories for item
#'
#' ipar <- data.frame(a = c(2.99, 3.53,  3.10),
#'                   CB1 = c(-1.18, -2.79,  -2.12),
#'                   CB2 = c(-0.499, -1.85,  -1.27),
#'                   CB3 = c(0.17, -1.19,  -0.52),
#'                   CB4 = c(0.653, -0.551,  0.036),
#'                   NCAT = 5)
#' simulate responses
#' set.seed(22416)
#' # 10 participants and their responses from the three items
#' resp <- round(matrix(runif(30, 1, 5), 20, 3))
#'
#' # run
#' grmCAT_output <- grmCAT(ipar, resp,minTheta=-6,maxTheta=6,nQpts=121,prevTheta=NULL,
#' prevSD=NULL,muEAP=0,sdEAP=3,scoreCumulativeDiffuse=TRUE,
#' start_item=0,minNI=4,maxSD=0.3,maxNI=8,deltaSD=0.01)
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
