#' MTB Graded Response Model Engine
#'
#' This function is a wrapper to simulate the graded response model engine programmed within MobileToolbox.
#' It utilizes matrixInfoGRM, selectMPWI, and calcEAP funtions for item selection and scoring.
#'
#' @param ipar An item parameter matrix in slope/threshold format. Each item is on a separate line.
#' The column names should be "a" and "CB1" through "CB#" for
#' the slope and relevant thresholds. There should also be a column called "NCAT" to indicate
#' the number of response categories available for each item.
#' @param resp Response matrix (number of rows = number of participants and number of columns = number of items)
#' @param minTheta Minimum theta value (PROMIS default is -4)
#' @param maxTheta Maximum theta value (PROMIS default is 4)
#' @param nQpts number of quadrature points, with min minTheta and max maxTheta
#' @param prevTheta estimated theta from prior CAT administration
#' @param prevSD estimated SD from prior CAT administration
#' @param muEAP Estimated A Posterior mean. Defaults to NULL.
#' @param sdEAP Estimated A Posterior SD. Defaults to NULL.
#' @param scoreCumulativeDiffuse Logical to indicate whether the theta grid should be modified prior to administering the first item based
#' on the final score estimate from the previous administration. Defaults to TRUE.
#' @param start_item First item to start the CAT engine.
#' @param minNI Minimum number of items to administer. Defaults to 4.
#' @param maxSD Maximum SD. Defaults to 0.3.
#' @param maxNI Maximum number of items to administer. Defaults to 8.
#' @param deltaSD Mininum change for SD.
#' @param bestAt This indicates how many items have to be administered before the Best Health stopping rule is checked.
#' This can override the _minNI_ parameter. NOTE: Setting this to any value greater than the _maxNI_ parameter will turn off the Best Health rule.
#' @param bestStop An indicator for whether the Best Health is the minimum (e.g., for symptom banks; indicated with “LO”)
#' or the maximum (e.g. for function banks; indicated with “HI”).
#' @param verbose A logical value indicating whether or not to print interim information related to the computer adaptive testing progress.
#' Default is FALSE.
#'
#' @return This function returns a list with five elements. The first two elements are scalar elements with
#' the final theta estimate and the standard deviation of the score elements.The third element is a dataframe
#' with the item administration history. The first row is the prior. The second row through the final are
#' the items administered, the chosen response category score, and the theta/sd history. The fourth and fifth
#' elements are the probability differences array and the item information matrix,
#' both of which are calculated by the the matrixInfoGRM function.
#'
#' @examples
#' # define item parameter bank; number of rows is the number of columns
#' # a = item slope; CB1 to CB# = category bins/thresholds for response; NCAT = number of categories for item
#'
#' ipar <- data.frame(a = c(2.99, 3.53,  3.10),
#'                   CB1 = c(-1.18, -2.79,  -2.12),
#'                   CB2 = c(-0.499, -1.85,  -1.27),
#'                   CB3 = c(0.17, -1.19,  -0.52),
#'                   CB4 = c(0.653, -0.551,  0.036),
#'                   NCAT = 5)
#' #simulate responses
#' set.seed(22416)
#' # 20 participants and their responses from the three items
#' resp <- round(matrix(stats::runif(20*3, 1, 5), 20, 3))
#'
#' # Currently grmCAT is NOT vectorized and only 1 participant can be run at a time (unless using apply)
#'
#' # run
#' grmCAT_output <- grmCAT(ipar, resp[,1],minTheta=-6,maxTheta=6,nQpts=121,prevTheta=NULL,
#' prevSD=NULL,muEAP=0,sdEAP=3,scoreCumulativeDiffuse=TRUE,
#' start_item=0,minNI=4,maxSD=0.3,maxNI=8,deltaSD=0.01)
#'
#' # set scoreCumulativeDiffuse = T and prevTheta = 1
#' grmCAT_sCD_true <- grmCAT(ipar, resp[,1],minTheta=-6,maxTheta=6,nQpts=121,prevTheta=1,
#' prevSD=NULL,muEAP=0,sdEAP=3,scoreCumulativeDiffuse=TRUE,
#' start_item=0,minNI=4,maxSD=0.3,maxNI=8,deltaSD=0.01)
#'
#' grmCAT_sCD_false <- grmCAT(ipar, resp[,1],minTheta=-6,maxTheta=6,nQpts=121,prevTheta=1,
#' prevSD=NULL,muEAP=0,sdEAP=3,scoreCumulativeDiffuse=FALSE,
#' start_item=0,minNI=4,maxSD=0.3,maxNI=8,deltaSD=0.01)
#'
#' # changing the scoreCumulativeDiffuse option does not change item selection with only three items
#' # theta estimates should generally be very close, if not identical
#'  grmCAT_sCD_true$finalTheta; grmCAT_sCD_false$finalTheta
#'
#' # or with apply
#' #' allResp <- apply(resp,1, function(y) grmCAT(ipar, resp=y))
#'
#'
#'  #### Using the PROMIS Pediatric Strength Impact item bank
#'  set.seed(542)
#' # 5 participants with random responses to 10 of the items in the bank
#' data(iparPedImp)
#' respSI <- round(matrix(stats::runif(10*5, 1, 5), 5, 10))
#' grm_out1 <- apply(respSI,1,function(y) grmCAT(ipar=iparPedImp[,2:7], resp=y,
#' minTheta=-4, maxTheta=4, nQpts=81,minNI=5,maxNI=12)) # PROMIS Peds defaults
#'
#' table(sapply(grm_out1, function(y) nrow(y$scoreHistory)-1)) # all participants got 5 items
#' table(sapply(grm_out1, function(y) y$scoreHistory$itemAdminHistory))
#'
#' @export
#'
#' @family GRM_CAT functions


grmCAT <- function(ipar, resp,minTheta=-6,maxTheta=6,nQpts=121,prevTheta=NULL,
                   prevSD=NULL,muEAP=0,sdEAP=3,scoreCumulativeDiffuse=TRUE,
                   start_item=0,minNI=4,maxSD=0.3,maxNI=8,
                   deltaSD=0.01, bestAt=NULL, bestStop='LO', verbose=F){
  # browser()
  if(!toupper(bestStop) %in% c("LO","HI")) stop("Error with Best Health stopping rule. Must be 'LO' or 'HI' to utilize; ignored if bestAt is null")
  nItems <- nrow(ipar)
  thetaGrid <- seq(minTheta,maxTheta,length.out=nQpts)
  maxCat <- max(ipar$NCAT)
  useprobs <- matrixInfoGRM(ipar,thetaGrid,maxCat)
  if(!scoreCumulativeDiffuse | (scoreCumulativeDiffuse & is.null(prevTheta))){
    currentProbGrid <- 1/(sdEAP * sqrt(2*pi))*exp((-(thetaGrid - muEAP)^2)/(2*sdEAP^2))
  } else {
    currentProbGrid <- 1/(sdEAP * sqrt(2*pi))*exp((-(thetaGrid - prevTheta)^2)/(2*sdEAP^2))
  }
  if(verbose){
    cat("adminOrder\t",0,'\tstartTheta\t',ifelse(scoreCumulativeDiffuse & !is.null(prevTheta), prevTheta, muEAP),
        "\nadminOrder\t",0,'\tStartSE\t',sdEAP,
        "\nadminOrder\t",0,'\tcurrentProbGrid\t',currentProbGrid)
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
  if(verbose){
    cat("\nadminOrder\t",as.character(nGiven),"\titemInfo\t",iinfo,
        "\nadminOrder\t",as.character(nGiven),"\tItemAdmin\t", itemAdmin,
        "\nadminOrder\t",as.character(nGiven),'\tScore\t',resp[itemAdmin],
        "\nadminOrder\t",as.character(nGiven),'\tInterimTheta\t',EAPs$estTheta,
        "\nadminOrder\t",as.character(nGiven),'\tInterimSE\t',EAPs$estSD,
        "\nadminOrder\t",as.character(nGiven),'\tcurrentProbGrid\t',EAPs$currentProbGrid)
  }
  while(nGiven < maxNI & length(which(itemAvail)) > 0){
    if(nGiven >= minNI & (EAPs$estSD < maxSD | (sdHistory[nGiven] - sdHistory[(nGiven+1)] > 0 &
                                                sdHistory[nGiven] - sdHistory[(nGiven+1)] < deltaSD)))
      break
    if(is.null(bestAt) |
       (!is.null(bestAt) && (nGiven != bestAt |
                           nGiven == bestAt & ((toupper(bestStop)=="LO" & sum(scores)!=nGiven) |
                                               (toupper(bestStop) == "HI" & sum(scores) != sum(ipar[itemAdminHistory,'NCAT'])))) )){
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
      if(verbose){
        cat("\nadminOrder\t",as.character(nGiven),"\titemInfo\t",iinfo,
            "\nadminOrder\t",as.character(nGiven),"\tItemAdmin\t", itemAdmin,
            "\nadminOrder\t",as.character(nGiven),'\tScore\t',resp[itemAdmin],
            "\nadminOrder\t",as.character(nGiven),'\tInterimTheta\t',EAPs$estTheta,
            "\nadminOrder\t",as.character(nGiven),'\tInterimSE\t',EAPs$estSD,
            "\nadminOrder\t",as.character(nGiven),'\tcurrentProbGrid\t',EAPs$currentProbGrid)
      }
    } else if(nGiven == bestAt & ((toupper(bestStop)=="LO" & sum(scores)==nGiven) |
                                (toupper(bestStop) == "HI" & sum(scores) == sum(ipar[itemAdminHistory,'NCAT'])))) break
  }
  output <- list(finalTheta=thetaHistory[(nGiven+1)],
                 finalSD=sdHistory[(nGiven+1)],
                 scoreHistory = data.frame(itemAdminHistory=c('Prior',itemAdminHistory),itemScores=c(NA,scores),thetaHistory,sdHistory),
                 probDiff=useprobs$probDiff,
                 matrixInfo=useprobs$matrixInfo)
  return(invisible(output))
}
