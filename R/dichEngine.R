#' MTB Dichotomous Engine
#'
#' This function is a wrapper to simulate the dichotomous engine programmed within MobileToolbox.
#' It utilizes MI_xPL and TIF_xPL functions for item selection and the MLE_xPL function for scoring.
#' The dichotomous engine includes functionality for using experimental items (every 5th item, provided experimental items are available),
#' exposure control with the Sympson-Hetter or Modified Davey-Parshall method,
#' excluding items administered on a previous run (for retest scenarios), and exporting interim steps to the console, if requested.
#'
#' @param iparFull A matrix with rows for items and three columns. The first column is the slope,
#' the second is the intercept (not the threshold),
#' and the third is the guessing parameter (on the original scale, not on the logit scale).
#' @param uFull A vector of response strings, coded 0 for incorrect and 1 for correct.
#' The length of this vector must be equal to the number of items in the item bank.
#' @param calib A vector of length nItems indicating whether an item is experimental (=0) or calibrated (=1).
#' Only calibrated items are utilized in scoring. Experimental (or uncalibrated) items are administered every 5 items as available.
#' @param lastAdmin A logical vector of length nItems indicating whether an item was administered (=TRUE) during the previous test session
#' or if it was unadministered (=FALSE) and thus available for the current test session. If this is not provided, it defaults to all FALSE
#' (i.e., no items were administered last time and thus all items are avialable this time).
#' @param targetProb The target probability for a correct answer. Admissible values are from 0.01 to 0.99. The optimally-targeted items is
#' at 0.50 (50% probability of correct responding). This is the default value, but many tests within MobileToolbox override this to 0.65
#' or something similar, in which case the respondent is more likely to get the correct answer.
#' @param minNI Minimum number of items to administer. Defaults to 20.
#' @param maxNI Maximum number of items to administer. Defaults to 35.
#' @param maxSE Maximum SE for variable-length CATs. Defaults to 0.4 (or approximately a reliability of 0.85)
#' @param nonML_se The standard error to report when the maximum likelihood score estimate is undefined.
#' Note that the MLE_xPL function returns Inf in these cases, and it is overridden only for reporting purposes.
#' The default value for this is 99.
#' @param stepVal In cases when the MLE score is undefined, this parameter defines how far the theta estimate should be incremented
#' (positively if all correct, negatively if all incorrect) for the next item. Default is 0.75 logits.
#' @param minTheta Minimum allowable theta. Defaults to -10.
#' @param maxTheta Maximum allowable theta. Defaults to +10.
#' @param exp.cont This is a symmetric exposure control matrix of dimensions nItems by nItems. Acceptable values are between 0 and 1.
#' If it is not provided, the default is to create a matrix of all 1's, indicating no exposure control. If all off-diagonal values
#' are equal to 1 but the diagonal is between 0 and 1, this is equal to the Sympson-Hetter method for exposure control, or what
#' has been referred to as unconditional exposure control. If the off-diagonals are also between 0 and 1, the lowest off-diagonal for an administered item
#' is multiplied by the diagonal value to get a conditional exposure control value. This is a modification of the Davey-Parshall method.
#' @param startTheta The theta value at which item selection for this person should begin. Defaults to 0.
#' @param maxCycles Maximum number of cycles for the Newton Raphson method for MLE scoring.
#' @param critScore Convergence criterion for the Newton Raphson method for MLE scoring.
#' @param seedval Seed value for the random number generator used for exposure control.
#' @param verbose A logical value indicating whether or not to print interim information related to the computer adaptive testing progress.
#' Default is FALSE.
#'
#' @return dichotomousEngine returns a list of length 7 with various information about how the engine progressed through the simulation.
#' The first element is Theta, which is the final score for this simulee.
#'
#' The second element is SEM, which is the standard error associated with the final theta for this simulee.
#'
#' The third element is the interimScores matrix. It includes information about the theta estimate and standard error used to initialize the engine
#' (row 1) and after each item.
#'
#' The fourth element is the itemConsideration dataframe. This includes information on which item was considered,
#' what the exposure control value was for that item, conditioned upon all of the items already administered, the random number sampled at that time
#' used to determine whether or not to administer that item, and a TRUE/FALSE indicator for whether the considered item was administered to the examinee
#' or removed from consideration for the remainder of this administration.
#'
#' The adminInfo element is next, which includes only the items admnistered during the consideration step. It lists the item identifier, the response
#' (correct/incorrect coded 1/0, respectively), and whether or not the item was experimental.
#'
#' The sixth element is an integer for the number of items given during this administration.
#'
#' And the final element is restating the seed value used to initialize the RNG for exposure control.
#'
#' @export
#'
#' @family dichotomous functions


dichEngine <- function(iparFull, uFull, calib, lastAdmin, targetProb=0.5, minNI=20, maxNI=35, maxSE=0.4,
                       nonML_se=99, stepVal=0.75, minTheta=-10, maxTheta=10, exp.cont, startTheta=0,
                       maxCycles=100, critScore=5e-4, seedval=12345, verbose=F){
  if(tibble::is_tibble(iparFull)) iparFull <- as.data.frame(iparFull)
  set.seed(seedval)
  nItems <- nrow(iparFull)
  if(!"a" %in% colnames(iparFull)) iparFull$a <- rep(1, nItems)
  if(!"g" %in% colnames(iparFull)) iparFull$g <- rep(0, nItems)
  iparFull <- iparFull[,c('a','d','g')]
  if(length(uFull) != nItems) stop("Input error, incomplete response vector")
  if(!all(calib %in% c(0,1))) stop("Invalid calib argument. Must be coded 0=uncalibrated 1=calibrated.")
  if(missing(lastAdmin)) lastAdmin <- rep(FALSE, nItems)                            # if no missing last administration, set all items to unadministered
  if(missing(exp.cont)) exp.cont <- matrix(1, nItems,nItems)       # if no exposure control matrix, set all items to uncontrolled
  if(nrow(exp.cont) != ncol(exp.cont) & nrow(exp.cont) >= 1) stop("Invalid exposure control matrix")

  itemAvail <- rep(TRUE, nItems)
  itemAvail[which(lastAdmin)] <- FALSE                                               # set last administered items to unavailable on this test occasions

  interimScores <- data.frame(Theta=startTheta, SE=nonML_se)                        # initialize interim scores at starting values
  nGiven <- 0

  # initialize the item consideration data frame
  # note, these vectors are independent now, but will be added together prior to ending the simulation
  test.item <- test.exp <- rnd.exp <- admin.TF <- vector()


  # initialize the item administration data frame
  # note, these vectors are independent now, but will be added together prior to ending the simulation
  admin.ItemID <- resp <- vector()

  if(all(calib==1)){
    while(nGiven < maxNI & length(which(itemAvail)) > 0){                              # this is the main CAT function
      nGiven <- nGiven+1                                                               # increase the number of items given by 1
      targTheta <- interimScores$Theta[nGiven] - log(targetProb/(1-targetProb))        # modify target theta based on probability of correct
      iiorder <- maxInfo_xPL(iparFull, targTheta)                                     # calculate item info order
      iiorder <- iiorder[itemAvail[iiorder]]                                           # remove from consideration items that are unavailable
      if(verbose) cat(as.character(nGiven), "\t","targTheta", "\t", targTheta, "\n",
                      as.character(nGiven), "\t","Max Info Order", "\t",paste0("V",iiorder), fill=TRUE)
      for(c in 1:length(iiorder)){
        test.item <- c(test.item,iiorder[c])                                           # add the current test item to the test item vector
        rnd.exp <- c(rnd.exp, stats::runif(n=1, min=0, max=1))                                # randomly draw an exposure value and add to that vector
        if(nGiven == 1){                                                               # calculate the exposure control value and add it to that vector
          test.exp <- c(test.exp, exp.cont[test.item[length(test.item)],
                                           test.item[length(test.item)]])
        } else {
          test.exp <- c(test.exp, (exp.cont[test.item[length(test.item)],
                                            test.item[length(test.item)]] *
                                     min(exp.cont[test.item[length(test.item)], admin.ItemID])))
        }
        if(verbose) cat(as.character(nGiven), "\t","testItem", "\t", paste0("V",test.item[length(test.item)]), "\n",
                        as.character(nGiven), "\t","rnd.exp", "\t", rnd.exp[length(rnd.exp)], "\n",
                        as.character(nGiven), "\t","test.exp","\t",test.exp[length(test.exp)], fill=TRUE)
        if(test.exp[length(test.exp)] >= rnd.exp[length(rnd.exp)]){                    # administer the item if passes exposure control rules
          itemAvail[test.item[length(test.item)]] <- FALSE
          admin.TF <- c(admin.TF, TRUE)
          break
        } else {
          itemAvail[test.item[length(test.item)]] <- FALSE
          admin.TF <- c(admin.TF, FALSE)
        }
      }
      admin.ItemID <- c(admin.ItemID, test.item[length(test.item)])                    # record administered item
      resp <- c(resp, uFull[admin.ItemID[length(admin.ItemID)]])
      tmp <- MLE_xPL(ipar=iparFull[admin.ItemID,], u=resp, crit=critScore, maxIter=maxCycles, minTheta=minTheta, maxTheta=maxTheta)
      if(is.infinite(tmp$Theta)){
        interimScores[(nGiven+1),] <- c(ifelse(interimScores[nGiven,'Theta'] + sign(tmp$Theta)*stepVal < minTheta, minTheta,
                                               ifelse(interimScores[nGiven,'Theta'] + sign(tmp$Theta)*stepVal > maxTheta, maxTheta,
                                                      interimScores[nGiven,'Theta'] + sign(tmp$Theta)*stepVal)),nonML_se)
      } else interimScores[(nGiven+1),] <- tmp[,c("Theta","SEM")]
      if(verbose) cat(as.character(nGiven), "\t","score","\t",resp[length(resp)], "\n",
                      as.character(nGiven), "\t","interimTheta","\t", interimScores[(nGiven+1),1], "\n",
                      as.character(nGiven), "\t","interimSE","\t", interimScores[(nGiven+1),2],fill=TRUE)
      if(nGiven >= minNI & interimScores[(nGiven+1),'SE'] < maxSE) break
    }
    output <- list(Theta=interimScores[(nGiven+1),'Theta'],
                   SEM=interimScores[(nGiven+1),'SE'],
                   interimScores=interimScores,
                   itemConsideration=data.frame(test.item,test.exp,rnd.exp,admin.TF),
                   adminInfo=data.frame(admin.ItemID,resp),
                   nGiven=nGiven,
                   seedVal=seedval)
  } else {
    ipar_Split <- split(iparFull,calib)
    u_Split <- split(uFull, calib)
    itemAvailSplit <- split(itemAvail, calib)
    exp.cont <- exp.cont[which(calib==1),which(calib==1)]
    admin_type <- vector()
    while(nGiven < maxNI & length(which(itemAvailSplit[["1"]])) > 0){
      nGiven <- nGiven + 1
      if(nGiven %% 5 != 0 | all(!itemAvailSplit[["0"]])){
        admin_type <- c(admin_type,'Calibrated')
        targTheta <- interimScores$Theta[nGiven] - log(targetProb/(1-targetProb))        # modify target theta based on probability of correct
        iiorder <- maxInfo_xPL(ipar_Split[['1']], targTheta)                             # calculate item info order
        iiorder <- iiorder[itemAvailSplit[['1']][iiorder]]                                 # remove from consideration items that are unavailable
        if(verbose) cat(as.character(nGiven), "\t","targTheta", "\t", targTheta, "\n",
                        as.character(nGiven), "\t","maxInfoOrder", "\t",paste0("C",iiorder), fill=TRUE)
        for(c in 1:length(iiorder)){
          test.item <- c(test.item,iiorder[c])                                           # add the current test item to the test item vector
          rnd.exp <- c(rnd.exp, runif(n=1, min=0, max=1))                                # randomly draw an exposure value and add to that vector
          # browser()
          if(nGiven == 1){                                                               # calculate the exposure control value and add it to that vector
            test.exp <- c(test.exp, exp.cont[test.item[length(test.item)],
                                             test.item[length(test.item)]])
          } else {
            test.exp <- c(test.exp, (exp.cont[test.item[length(test.item)],
                                              test.item[length(test.item)]] *
                                       min(exp.cont[test.item[length(test.item)],
                                                    admin.ItemID[which(admin_type=='Calibrated')]],na.rm=T)))
          }
          if(verbose) cat(as.character(nGiven), "\t","testItem", "\t", paste0("C",test.item[length(test.item)]), "\n",
                          as.character(nGiven), "\t","rnd.exp", "\t", rnd.exp[length(rnd.exp)], "\n",
                          as.character(nGiven), "\t","test.exp","\t",test.exp[length(test.exp)], fill=TRUE)
          if(test.exp[length(test.exp)] >= rnd.exp[length(rnd.exp)]){                    # administer the item if passes exposure control rules
            itemAvailSplit[['1']][test.item[length(test.item)]] <- FALSE
            admin.TF <- c(admin.TF, TRUE)
            break
          } else {
            itemAvailSplit[['1']][test.item[length(test.item)]] <- FALSE
            admin.TF <- c(admin.TF, FALSE)
          }
        }
        admin.ItemID <- c(admin.ItemID, test.item[length(test.item)])                    # record administered item
        resp <- c(resp, u_Split[['1']][admin.ItemID[length(admin.ItemID)]])
        tmp <- MLE_xPL(ipar=ipar_Split[['1']][admin.ItemID[which(admin_type == 'Calibrated')],],
                       u=resp[which(admin_type == 'Calibrated')],
                       crit=critScore, maxIter=maxCycles, minTheta=minTheta, maxTheta=maxTheta)
        if(is.infinite(tmp$Theta)){
          interimScores[(nGiven+1),] <- c(ifelse(interimScores[nGiven,'Theta'] + sign(tmp$Theta)*stepVal < minTheta, minTheta,
                                                 ifelse(interimScores[nGiven,'Theta'] + sign(tmp$Theta)*stepVal > maxTheta, maxTheta,
                                                        interimScores[nGiven,'Theta'] + sign(tmp$Theta)*stepVal)),nonML_se)
        } else interimScores[(nGiven+1),] <- tmp[,c("Theta","SEM")]
        if(verbose) cat(as.character(nGiven), "\t","score","\t",resp[length(resp)], "\n",
                        as.character(nGiven), "\t","interimTheta","\t", interimScores[(nGiven+1),1], "\n",
                        as.character(nGiven), "\t","interimSE","\t", interimScores[(nGiven+1),2],fill=TRUE)
        if(nGiven >= minNI & interimScores[(nGiven+1),'SE'] < maxSE) break
      } else {
        admin_type <- c(admin_type,'Experimental')
        targTheta <- interimScores$Theta[nGiven] - log(targetProb/(1-targetProb))        # modify target theta based on probability of correct
        iiorder <- maxInfo_xPL(ipar_Split[['0']], targTheta)                             # calculate item info order
        iiorder <- iiorder[itemAvailSplit[['0']][iiorder]]                                # remove from consideration items that are unavailable
        if(verbose) cat(as.character(nGiven), "\t","targTheta", "\t", targTheta, "\n",
                        as.character(nGiven), "\t","UNCALIBRATED_maxInfoOrder", "\t",paste0("E",iiorder), fill=TRUE)
        itemAvailSplit[['0']][iiorder[1]] <- FALSE
        admin.ItemID <- c(admin.ItemID, iiorder[1])
        resp <- c(resp, u_Split[['0']][iiorder[1]])
        interimScores[(nGiven+1),] <- interimScores[nGiven,]
        if(verbose) cat(as.character(nGiven), "\t","score","\t",resp[length(resp)], "\n",
                        as.character(nGiven), "\t","interimTheta","\t", interimScores[(nGiven+1),1], "\n",
                        as.character(nGiven), "\t","interimSE","\t", interimScores[(nGiven+1),2],fill=TRUE)
        if(nGiven >= minNI & interimScores[(nGiven+1),'SE'] < maxSE) break
      }
    }
    output <- list(Theta=interimScores[(nGiven+1),'Theta'],
                   SEM=interimScores[(nGiven+1),'SE'],
                   interimScores=interimScores,
                   itemConsideration=data.frame(test.item,test.exp,rnd.exp,admin.TF),
                   adminInfo=data.frame(admin.ItemID,resp,admin_type),
                   nGiven=nGiven,
                   seedVal=seedval)
  }


  return(output)
}
