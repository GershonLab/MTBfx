# firestar function to hack and determine where inconsistencies are

# ---------------------------------------------------------------
# randomly defining variables
# ---------------------------------------------------------------

#ni.available = Inf

# ---------------------------------------------------------------
# ---------------------------------------------------------------
# ---------------------------------------------------------------
# ---------------------------------------------------------------


 Firestar_removeglut <- function (filename.ipar = file.choose(),
                                  filename.resp = "",
                       filename.content = "", ncc = 1, filename.theta = "", model = "GRM",
                       D = 1, simulateTheta = F, popDist = "NORMAL", popPar = c(0, 1),
                       nSimulee = 1000, eapFullLength = T,
                       maxCat = 5,
                       minTheta = -3, maxTheta = 3, inc = 0.1, minNI = 4, maxNI = 12,
                       maxSE = 0.3, topN = 1,
                       exposure.control = F,
                       stopSE = 0.01,
                       continueSE = 0.03, min.SE.change = 0, extreme.response.check = "N",
                       max.extreme.response = 4, ni.available = 1000,
                       selection.method = "MPWI", interim.Theta = "EAP",
                       shrinkageCorrection = FALSE, se.method = 1, first.item.selection = 1,
                       first.at.theta = 0, first.item = 1, show.theta.audit.trail = T,
                       #plot.usage = F, plot.info = F, plot.prob = F,
                       add.final.theta = F,
                       bank.diagnosis = F, prior.dist = 1, prior.mean = 0, prior.sd = 1,
                       file.items.used = "", file.theta.history = "", file.se.history = "",
                       file.final.theta.se = "", file.other.thetas = "", file.likelihood.dist = "",
                       file.posterior.dist = "", file.matrix.info = "", file.full.length.theta = "",
                       file.selected.item.resp = "")
 {
call <- match.call()

#filename.ipar <-" PF_2.0_ipar.csv"
# item parameter filename
item.par <- read.csv(filename.ipar, sep = ",", header = FALSE,
                     col.names = c("a", paste("cb", 1:(maxCat - 1), sep = ""),
                                   "NCAT"))
# number of items
ni <- nrow(item.par)

# code about whether to perform exposure control or content balancing
 if (exposure.control)
   exposure.rate <- numeric(ni)
   content.balancing <- FALSE
 if (ncc > 1 && filename.content != "" && !(toupper(selection.method) %in%
                                            c("SEQ", "TSB"))) {
   target.content.dist <- as.numeric(read.csv(filename.content,
                                             header = FALSE, nrows = 1))
   if (all(target.content.dist == 0))
     warning("WARNING: all values in target content distribution are zero\n:content balancing not used")
   content.cat <- read.csv(filename.content, header = FALSE,
                           skip = 1)[[2]]
   if (abs(sum(target.content.dist) - 1) > 0.1)
     warning("WARNING: the sum of content proportions should add up to 1.0\n:content balancing not used")
   else if (length(target.content.dist) != ncc)
     warning("WARNING: the number of content categories (ncc) does not match the number of target proportions in the content control file\n:content balancing not used")
   else if (length(content.cat) != ni)
     warning("WARNING: the number of records in the content control file does not match the number of items in the bank\n:content balancing not used")
   else {
     if (maxNI > sum(content.cat %in% which(target.content.dist >
                                            0)))
       warning("WARNING: maxNI cannot be larger than the sum of items where target.content.dist > 0")
     overall.content.freq <- numeric(ncc)
     content.balancing <- TRUE
   }
 }
 next.content <- function() {
   available.content <- which(target.content.dist > 0 &
                                as.numeric(tapply(items.available, content.cat,
                                                  sum) > 0))
   idx <- which.max(target.content.dist[available.content] -
                      current.content.dist[available.content])
   return(available.content[idx])
 }
 update.content.dist <- function() {
   idx <- content.cat[item.selected]
   current.content.freq[idx] <<- current.content.freq[idx] +
     1
   overall.content.freq[idx] <<- overall.content.freq[idx] +
     1
   current.content.dist <<- current.content.freq/ni.given
 }
genResp <- function(n, Par) {
  if (toupper(popDist) == "NORMAL") {
    theta <- rnorm(n) * popPar[2] + popPar[1]
  }
  else if (toupper(popDist) == "UNIFORM") {
    theta <- runif(n, popPar[1], popPar[2])
  }
  else if (toupper(popDist) == "GRID") {
    theta <- rep(popPar, each = nSimulee)
  }
  else {
    stop("invalid option specified for popDist")
  }
  ni <- dim(Par)[1]
  ncat <- Par$NCAT
  nq <- length(theta)
  resp <- matrix(1, nq, ni)
  pp <- array(0, c(nq, ni, maxCat))
  if (model == "GRM") {
    for (i in 1:ni) {
      ncat <- Par[i, "NCAT"]
      a <- Par[i, "a"]
      cb <- Par[i, paste("cb", 1:(ncat - 1), sep = "")]
      if (is.unsorted(cb)) {
        stop(paste("item", i, "has disordinal category parameters\n"))
      }
      if (any(is.na(cb))) {
        stop(paste("item", i, "has missing category parameter(s)\n"))
      }
      ps <- matrix(0, nq, ncat + 1)
      ps[, 1] <- 1
      ps[, ncat + 1] <- 0
      for (k in 1:(ncat - 1)) {
        ps[, k + 1] <- 1/(1 + exp(-D * a * (theta -
                                              cb[[k]])))
      }
      for (k in 1:ncat) {
        pp[, i, k] = ps[, k] - ps[, k + 1]
      }
    }
  }
  # else if (model == "GPCM") {
  #   for (i in 1:ni) {
  #     ncat <- Par[i, "NCAT"]
  #     a <- Par[i, "a"]
  #     cb <- unlist(Par[i, paste("cb", 1:(ncat - 1),
  #                               sep = "")])
  #     cb <- c(0, cb)
  #     zz <- matrix(0, nq, ncat)
  #     sdsum <- 0
  #     den <- rep(0, nq)
  #     for (k in 1:ncat) {
  #       sdsum <- sdsum + cb[k]
  #       zz[, k] <- exp(D * a * (k * theta - sdsum))
  #       den <- den + zz[, k]
  #     }
  #     for (k in 1:ncat) {
  #       pp[, i, k] <- zz[, k]/den
  #     }
  #   }
  # }
  random <- matrix(runif(nq * ni), nq, ni)
  for (i in 1:ni) {
    ncat <- Par[i, "NCAT"]
    sump <- numeric(nq)
    for (k in 1:(ncat - 1)) {
      sump <- sump + pp[, i, k]
      resp[, i] <- ifelse(random[, i] > sump, resp[,
                                                   i] + 1, resp[, i] + 0)
    }
  }
  out <- data.frame(resp, theta)
  names(out) <- c(paste("R", 1:ni, sep = ""), "theta")
  return(out)
}
if (filename.resp != "" && simulateTheta == FALSE) {
  resp.data <- read.csv(filename.resp, sep = ",", header = FALSE,
                        col.names = paste("R", 1:ni, sep = ""))
  resp.matrix <- data.matrix(resp.data)
  true.theta <- NA
}
else if (!is.na(nSimulee) && nSimulee > 0) {
  resp.data <- genResp(nSimulee, item.par)
  true.theta <- resp.data$theta
  resp.matrix <- data.matrix(resp.data[paste("R", 1:ni,
                                             sep = "")])
}
theta <- seq(minTheta, maxTheta, inc)
nq = length(theta)
if (first.item.selection == 2 && first.at.theta >= minTheta &&
    first.at.theta <= maxTheta) {
  start.theta <- first.at.theta
}
else start.theta <- prior.mean
if (prior.dist == 1) {
  prior <- dnorm((theta - prior.mean)/prior.sd)
}
else if (prior.dist == 2) {
  prior <- exp((theta - prior.mean)/prior.sd)/(1 + exp((theta -
                                                          prior.mean)/prior.sd))^2
}
else prior <- dnorm(theta)
nExaminees <- dim(resp.data)[1]
items.used <- matrix(NA, nExaminees, maxNI)
selected.item.resp <- matrix(NA, nExaminees, maxNI)
ni.administered <- numeric(nExaminees)
theta.CAT <- rep(NA, nExaminees)
sem.CAT <- rep(NA, nExaminees)
theta.history <- matrix(NA, nExaminees, maxNI)
se.history <- matrix(NA, nExaminees, maxNI)
posterior.matrix <- matrix(NA, nExaminees, nq)
LH.matrix <- matrix(NA, nExaminees, nq)
NCAT <- item.par[, "NCAT"]
DISC <- item.par[, "a"]
CB <- item.par[paste("cb", 1:(maxCat - 1), sep = "")]
prep.prob.info <- function() {
  pp <- array(0, c(nq, ni, maxCat))
  matrix.info <- matrix(0, nq, ni)
#  if (model == "GRM") {
    for (i in 1:ni) {
      ps <- matrix(0, nq, NCAT[i] + 1)
      ps[, 1] <- 1
      ps[, NCAT[i] + 1] <- 0
      for (k in 1:(NCAT[i] - 1)) {
        ps[, k + 1] <- 1/(1 + exp(-D * DISC[i] * (theta -
                                                    CB[i, k])))
      }
      for (k in 1:NCAT[i]) {
        pp[, i, k] = ps[, k] - ps[, k + 1]
        matrix.info[, i] <- matrix.info[, i] + (D *
                                                  DISC[i] * (ps[, k] * (1 - ps[, k]) - ps[,
                                                                                          k + 1] * (1 - ps[, k + 1])))^2/pp[, i, k]
      }
    }
 # }
 # else if (model == "GPCM") {
  #  for (i in 1:ni) {
   #   cb <- unlist(CB[i, ])
   #   cb <- c(0, cb)
  #   zz <- matrix(0, nq, NCAT[i])
   #   sdsum <- 0
   #   den <- rep(0, nq)
   ##   for (k in 1:NCAT[i]) {
   #     sdsum <- sdsum + cb[k]
    #    zz[, k] <- exp(D * DISC[i] * (k * theta -
    #                                    sdsum))
    #    den <- den + zz[, k]
   #   }
   #   AX <- rep(0, nq)
   #   BX <- rep(0, nq)
   #   for (k in 1:NCAT[i]) {
   #     pp[, i, k] <- zz[, k]/den
   #     AX <- AX + k^2 * pp[, i, k]
    #    BX <- BX + k * pp[, i, k]
   #   }
    #  matrix.info[, i] <- D^2 * DISC[i]^2 * (AX -
    #                                           BX^2)
   # }
  #}
  list(pp = pp, matrix.info = matrix.info)
}
matrix.prob.info <- prep.prob.info()
pp <- matrix.prob.info$pp
matrix.info <- matrix.prob.info$matrix.info
calcFullLengthEAP <- function() {
  posterior <- matrix(rep(prior, nExaminees), nExaminees,
                      nq, byrow = TRUE)
  for (i in 1:ni) {
    resp <- matrix(resp.matrix[, i], nExaminees, 1)
    if (!all(is.na(resp))) {
      prob <- t(pp[, i, resp])
      prob[is.na(prob)] <- 1
      posterior <- posterior * prob
    }
  }
  EAP <- posterior %*% theta/rowSums(posterior)
  SEM <- sqrt(rowSums(posterior * (matrix(theta, nExaminees,
                                          nq, byrow = TRUE) - matrix(EAP, nExaminees, nq))^2)/rowSums(posterior))
  return(data.frame(theta = EAP, SE = SEM))
}
if (!(filename.theta == "") & eapFullLength == FALSE) {
  ext.theta <- read.csv(filename.theta, sep = ",", header = F,
                        col.names = "theta")
}
else {
  ext.theta <- calcFullLengthEAP()
}
calcInfo <- function(th) {
  info <- numeric(ni)
  available <- items.available
  if (content.balancing) {
    available <- items.available & (content.cat == next.content())
  }
  if (sum(available) == 0)
    return(info)
  if (model == "GRM") {
    for (i in 1:ni) {
      if (available[i] == TRUE) {
        ps <- numeric(NCAT[i] + 1)
        ps[1] <- 1
        ps[NCAT[i] + 1] <- 0
        for (k in 1:(NCAT[i] - 1)) {
          ps[k + 1] <- 1/(1 + exp(-D * DISC[i] * (th -
                                                    CB[i, k])))
        }
        prob <- numeric(NCAT[i])
        for (k in 1:NCAT[i]) {
          prob[k] <- ps[k] - ps[k + 1]
          info[i] <- info[i] + (D * DISC[i] * (ps[k] *
                                                 (1 - ps[k]) - ps[k + 1] * (1 - ps[k +
                                                                                     1])))^2/prob[k]
        }
      }
    }
  }
  # else if (model == "GPCM") {
  #   for (i in 1:ni) {
  #     if (available[i] == TRUE) {
  #       zz <- numeric(NCAT[i])
  #       sdsum <- 0
  #       den <- 0
  #       cb <- unlist(CB[i, ])
  #       cb <- c(0, cb)
  #       for (k in 1:(NCAT[i])) {
  #         sdsum <- sdsum + cb[k]
  #         zz[k] <- exp(D * DISC[i] * (k * th - sdsum))
  #         den <- den + zz[k]
  #       }
  #       AX <- 0
  #       BX <- 0
  #       prob <- numeric(NCAT[i])
  #       for (k in 1:NCAT[i]) {
  #         prob[k] <- zz[k]/den
  #         AX <- AX + k^2 * prob[k]
  #         BX <- BX + k * prob[k]
  #       }
  #       info[i] <- D^2 * DISC[i]^2 * (AX - BX^2)
  #     }
  #   }
  # }
   return(info)
}
calc.Loc.info <- function(th) {
  info <- numeric(ni)
  avg <- function(...) mean(..., na.rm = TRUE)
  loc <- apply(CB, 1, avg)
  available <- items.available
  if (content.balancing)
    available <- items.available & (content.cat == next.content())
  for (i in 1:ni) {
    if (available[i] == TRUE) {
      p <- 1/(1 + exp(-D * DISC[i] * (th - loc[i])))
      q <- 1 - p
      info[i] <- D^2 * DISC[i]^2 * p * q
    }
  }
  return(info)
}
calc.LW.info <- function(lk) {
  info <- numeric(ni)
  info <- apply(matrix.info * lk, 2, sum)
  info[items.available == FALSE] <- 0
  if (content.balancing)
    info[content.cat != next.content()] <- 0
  return(info)
}
calc.PW.info <- function(pos) {
  info <- numeric(ni)
  info <- apply(matrix.info * pos, 2, sum)
  info[items.available == FALSE] <- 0
  if (content.balancing)
    info[content.cat != next.content()] <- 0
  return(info)
}
calc.Expected.Info <- function(pos, current.theta) {
  info <- numeric(ni)
  available <- items.available
  if (content.balancing)
    available <- items.available & (content.cat == next.content())
  for (i in 1:ni) {
    if (available[i] == TRUE) {
      ncat <- NCAT[i]
      a <- DISC[i]
      cb <- unlist(CB[i, ])
      EAP.k <- numeric(ncat)
      wt <- numeric(ncat)
      for (k in 1:ncat) {
        posterior.k <- pos * pp[, i, k]
        wt[k] <- sum(posterior.k)
        EAP.k[k] <- sum(posterior.k * theta)/sum(posterior.k)
      }
      wt <- wt/sum(wt)
      if (model == "GRM") {
        ps <- numeric(ncat + 1)
        ps[1] <- 1
        ps[ncat + 1] <- 0
        for (r in 1:ncat) {
          info.r <- 0
          prob <- numeric(ncat)
          for (k in 1:(ncat - 1)) {
            ps[k + 1] <- 1/(1 + exp(-D * a * (EAP.k[r] -
                                                cb[k])))
          }
          for (k in 1:ncat) {
            prob[k] <- ps[k] - ps[k + 1]
            info.r <- info.r + (D * a * (ps[k] * (1 -
                                                    ps[k]) - ps[k + 1] * (1 - ps[k + 1])))^2/prob[k]
          }
          info[i] <- info[i] + wt[r] * info.r
        }
      }
  #     else if (model == "GPCM") {
  #       cb <- c(0, cb)
  #       for (r in 1:ncat) {
  #         prob <- numeric(ncat)
  #         zz <- numeric(ncat)
  #         sdsum <- 0
  #         den <- 0
  #         for (k in 1:ncat) {
  #           sdsum <- sdsum + cb[k]
  #           zz[k] <- exp(D * DISC[i] * (k * EAP.k[r] -
  #                                         sdsum))
  #           den <- den + zz[k]
  #         }
  #         AX <- 0
  #         BX <- 0
  #         for (k in 1:ncat) {
  #           prob[k] <- zz[k]/den
  #           AX <- AX + k^2 * prob[k]
  #           BX <- BX + k * prob[k]
  #         }
  #         info.r <- D^2 * DISC[i]^2 * (AX - BX^2)
  #         info[i] <- info[i] + wt[r] * info.r
  #       }
  #     }
  #   }
  # }
  return(info)
}
calc.Expected.Var <- function(pos, current.theta) {
  epv <- numeric(ni)
  available <- items.available
  if (content.balancing)
    available <- items.available & (content.cat == next.content())
  for (i in 1:ni) {
    if (available[i] == TRUE) {
      ncat <- NCAT[i]
      wt <- numeric(ncat)
      EAP.k <- numeric(ncat)
      for (k in 1:ncat) {
        posterior.k <- pos * pp[, i, k]
        EAP.k[k] <- sum(posterior.k * theta)/sum(posterior.k)
        wt[k] <- sum(posterior.k)
        epv[i] <- epv[i] + wt[k] * sum(posterior.k *
                                         (theta - EAP.k[k])^2)/sum(posterior.k)
      }
      epv[i] <- 1/epv[i]^2
    }
  }
  return(epv)
}
calc.MI <- function(pos) {
  MI <- numeric(ni)
  available <- items.available
  if (content.balancing)
    available <- items.available & (content.cat == next.content())
  for (i in 1:ni) {
    if (available[i] == TRUE) {
      ncat <- NCAT[i]
      p <- numeric(ncat)
      posterior.k <- matrix(NA, ncat, length(pos))
      for (k in 1:ncat) {
        posterior.k[k, ] <- pos * pp[, i, k]
        p[k] <- sum(posterior.k[k, ])
      }
      p <- p/sum(p)
      for (k in 1:ncat) {
        MI[i] <- MI[i] + sum(posterior.k[k, ] * log(posterior.k[k,
                                                                ]/(pos * p[k])))
      }
    }
  }
  return(MI)
}
calc.Predicted.PSD <- function(pos, current.theta) {
  ppsd <- rep(NA, ni)
  available <- items.available
  if (content.balancing)
    available <- items.available & (content.cat == next.content())
  for (i in 1:ni) {
    if (available[i] == TRUE) {
      ncat <- NCAT[i]
      wt <- numeric(ncat)
      EAP.k <- numeric(ncat)
      posterior.k <- matrix(NA, nq, ncat)
      for (k in 1:ncat) {
        posterior.k[, k] <- pos * pp[, i, k]
        wt[k] <- sum(pp[, i, k] * pos/sum(pos))
      }
      wt <- wt/sum(wt)
      ppsd[i] <- 0
      for (k in 1:ncat) {
        EAP.k[k] <- sum(posterior.k[, k] * theta)/sum(posterior.k[,
                                                                  k])
        ppsd[i] <- ppsd[i] + wt[k] * sqrt(sum(pos *
                                                pp[, i, k] * (theta - EAP.k[k])^2)/sum(pos *
                                                                                         pp[, i, k]))
      }
    }
  }
  return(ppsd)
}
calc.Expected.PW.Info <- function(pos, current.theta) {
  info <- numeric(ni)
  available <- items.available
  if (content.balancing)
    available <- items.available & (content.cat == next.content())
  for (i in 1:ni) {
    if (available[i] == TRUE) {
      ncat <- NCAT[i]
      wt <- numeric(ncat)
      info.i <- matrix.info[, i]
      info.k <- numeric(ncat)
      for (k in 1:ncat) {
        posterior.k <- pos * pp[, i, k]/sum(pos *
                                              pp[, i, k])
        info.k[k] <- sum(info.i * posterior.k)
        wt[k] <- sum(pos * pp[, i, k])
      }
      wt <- wt/sum(wt)
      info[i] <- sum(info.k * wt)
    }
  }
  return(info)
}
prep.info.ext.theta <- function() {
  pp <- matrix(0, nExaminees, maxCat)
  matrix.info <- matrix(0, nExaminees, ni)
  if (model == "GRM") {
    for (i in 1:ni) {
      ps <- matrix(0, nExaminees, NCAT[i] + 1)
      ps[, 1] <- 1
      ps[, NCAT[i] + 1] <- 0
      for (k in 1:(NCAT[i] - 1)) {
        ps[, k + 1] <- 1/(1 + exp(-D * DISC[i] * (ext.theta[[1]] -
                                                    CB[i, k])))
      }
      pp[, 1] <- 1 - ps[, 1]
      pp[, NCAT[i]] <- ps[, NCAT[i]]
      for (k in 1:NCAT[i]) {
        pp[, k] = ps[, k] - ps[, k + 1]
        matrix.info[, i] <- matrix.info[, i] + (D *
                                                  DISC[i] * (ps[, k] * (1 - ps[, k]) - ps[,
                                                                                          k + 1] * (1 - ps[, k + 1])))^2/pp[, k]
      }
    }
  }
  # else if (model == "GPCM") {
  #   for (i in 1:ni) {
  #     cb <- unlist(CB[i, ])
  #     cb <- c(0, cb)
  #     zz <- matrix(0, nExaminees, NCAT[i])
  #     sdsum <- 0
  #     den <- rep(0, nExaminees)
  #     for (k in 1:NCAT[i]) {
  #       sdsum <- sdsum + cb[k]
  #       zz[, k] <- exp(D * DISC[i] * (k * ext.theta[[1]] -
  #                                       sdsum))
  #       den <- den + zz[, k]
  #     }
  #     AX <- rep(0, nExaminees)
  #     BX <- rep(0, nExaminees)
  #     for (k in 1:NCAT[i]) {
  #       pp[, k] <- zz[, k]/den
  #       AX <- AX + k^2 * pp[, k]
  #       BX <- BX + k * pp[, k]
  #     }
  #     matrix.info[, i] <- D^2 * DISC[i]^2 * (AX -
  #                                              BX^2)
  #   }
  # }
  return(matrix.info)
}
select.maxInfo <- function() {
  if (exposure.control) {
    if (ni.given == 0) {
      item.selected <- info.index[sample(ni.available,
                                         1)]
      exposure.rate[item.selected] <<- exposure.rate[item.selected] +
        1
    }
    else {
      rc <- runif(ni, max = max(array.info))
      rc[items.available == FALSE] <- 0
      rel <- 1 - se.history[j, ni.given]^2
      w.array.info <- rel * (1 - exposure.rate/j) *
        array.info + (1 - rel) * rc
      item.selected <- order(w.array.info, decreasing = TRUE)[1]
      exposure.rate[item.selected] <<- exposure.rate[item.selected] +
        1
    }
  }
  else {
    if (ni.available >= topN) {
      item.selected <- info.index[sample(topN, 1)]
    }
    else if (ni.available > 0) {
      item.selected <- info.index[sample(ni.available,
                                         1)]
    }
  }
  return(item.selected)
}
calcSE <- function(examinee, ngiven, th) {
  info <- 0
  if (model == "GRM") {
    for (i in 1:ngiven) {
      itm <- items.used[examinee, i]
      ps <- numeric(NCAT[itm] + 1)
      ps[1] <- 1
      ps[NCAT[itm] + 1] <- 0
      for (k in 1:(NCAT[itm] - 1)) {
        ps[k + 1] <- 1/(1 + exp(-D * DISC[itm] * (th -
                                                    CB[itm, k])))
      }
      prob <- numeric(NCAT[itm])
      for (k in 1:NCAT[itm]) {
        prob[k] <- ps[k] - ps[k + 1]
        info <- info + (D * DISC[itm] * (ps[k] * (1 -
                                                    ps[k]) - ps[k + 1] * (1 - ps[k + 1])))^2/prob[k]
      }
    }
  }
  # else if (model == "GPCM") {
  #   info <- 0
  #   for (i in 1:ngiven) {
  #     itm <- items.used[examinee, i]
  #     cb <- unlist(CB[itm, ])
  #     cb <- c(0, cb)
  #     zz <- numeric(NCAT[itm])
  #     sdsum <- 0
  #     den <- 0
  #     for (k in 1:NCAT[itm]) {
  #       sdsum <- sdsum + cb[k]
  #       zz[k] <- exp(D * DISC[itm] * (k * th - sdsum))
  #       den <- den + zz[k]
  #     }
  #     AX <- 0
  #     BX <- 0
  #     prob <- numeric(NCAT[itm])
  #     for (k in 1:NCAT[itm]) {
  #       prob[k] <- zz[k]/den
  #       AX <- AX + k^2 * prob[k]
  #       BX <- BX + k * prob[k]
  #     }
  #     info <- info + D^2 * DISC[itm]^2 * (AX - BX^2)
  #   }
  # }
  SEM <- 1/sqrt(info)
  return(SEM)
}
calcEAP <- function(examinee, ngiven) {
  LH <- rep(1, nq)
  for (i in 1:ngiven) {
    item <- items.used[examinee, i]
    resp <- resp.matrix[examinee, item]
    prob <- pp[, item, resp]
    LH <- LH * prob
  }
  posterior <- prior * LH
  EAP <- sum(posterior * theta)/sum(posterior)
  if (se.method == 1) {
    SEM <- sqrt(sum(posterior * (theta - EAP)^2)/sum(posterior))
  }
  else if (se.method == 2) {
    SEM <- calcSE(examinee, ngiven, EAP)
  }
  if (shrinkageCorrection) {
    EAP <- EAP * (1 + SEM^2)
    if (se.method == 1) {
      SEM <- 1/(sqrt(1/SEM^2 - 1/prior.sd))
    }
    else if (se.method == 2) {
      SEM <- calcSE(examinee, ngiven, EAP)
    }
  }
  return(list(THETA = EAP, SEM = SEM, LH = LH, posterior = posterior))
}
calcMLE <- function(examinee, ngiven, maxIter = 50, crit = 1e-04) {
  EAP.estimates <- calcEAP(examinee, ngiven)
  total.raw <- 0
  max.raw <- 0
  ncat <- numeric(ngiven)
  resp <- numeric(ngiven)
  for (i in 1:ngiven) {
    item <- items.used[examinee, i]
    ncat[i] <- item.par[item, "NCAT"]
    resp[i] <- resp.matrix[examinee, item]
  }
  total.raw <- sum(resp)
  max.raw <- sum(ncat)
  if (total.raw == ngiven || total.raw == max.raw) {
    MLE <- EAP.estimates$THETA
    SEM <- EAP.estimates$SEM
  }
  else {
    change <- 1000
    nIter <- 0
    post.theta <- EAP.estimates$THETA
    if (model == "GRM") {
      while (nIter <= maxIter && change > crit) {
        pre.theta <- post.theta
        deriv1 <- 0
        deriv2 <- 0
        for (i in 1:ngiven) {
          item <- items.used[examinee, i]
          a <- item.par[item, "a"]
          cb <- item.par[item, paste("cb", 1:(ncat[i] -
                                                1), sep = "")]
          pp <- numeric(ncat[i])
          ps <- numeric(ncat[i] + 1)
          qs <- numeric(ncat[i] + 1)
          ps[1] <- 1
          for (k in 1:(ncat[i] - 1)) {
            ps[k + 1] <- 1/(1 + exp(-D * a * (pre.theta -
                                                cb[[k]])))
            qs[k + 1] <- 1 - ps[k + 1]
          }
          qs[ncat[i] + 1] <- 1
          pp[1] <- 1 - ps[1]
          pp[ncat[i]] <- ps[ncat[i]]
          for (k in 1:ncat[i]) {
            pp[k] <- ps[k] - ps[k + 1]
          }
          deriv1 <- deriv1 + D * a * ((ps[resp[i]] *
                                         qs[resp[i]] - ps[resp[i] + 1] * qs[resp[i] +
                                                                              1])/pp[resp[i]])
          deriv2 <- deriv2 + D^2 * a^2 * ((ps[resp[i]] *
                                             qs[resp[i]] * (qs[resp[i]] - ps[resp[i]]) -
                                             ps[resp[i] + 1] * qs[resp[i] + 1] * (qs[resp[i] +
                                                                                       1] - ps[resp[i] + 1]))/pp[resp[i]] -
                                            (ps[resp[i]] * qs[resp[i]] - ps[resp[i] +
                                                                              1] * qs[resp[i] + 1])^2/pp[resp[i]]^2)
        }
        SEM <- 1/sqrt(abs(deriv2))
        post.theta <- pre.theta - deriv1/deriv2
        change <- abs(post.theta - pre.theta)
        nIter <- nIter + 1
      }
    }
    # else if (model == "GPCM") {
    #   while (nIter <= maxIter && change > crit) {
    #     pre.theta <- post.theta
    #     deriv1 <- 0
    #     deriv2 <- 0
    #     for (i in 1:ngiven) {
    #       item <- items.used[examinee, i]
    #       a <- item.par[item, "a"]
    #       cb <- unlist(item.par[item, paste("cb",
    #                                         1:(ncat[i] - 1), sep = "")])
    #       cb <- c(0, cb)
    #       pp <- numeric(ncat[i])
    #       zz <- numeric(ncat[i])
    #       sdsum <- 0
    #       den <- 0
    #       for (k in 1:ncat[i]) {
    #         sdsum <- sdsum + cb[k]
    #         zz[k] <- exp(D * a * (k * pre.theta -
    #                                 sdsum))
    #         den <- den + zz[k]
    #       }
    #       AX <- 0
    #       BX <- 0
    #       for (k in 1:ncat[i]) {
    #         pp[k] <- zz[k]/den
    #         AX <- AX + k^2 * pp[k]
    #         BX <- BX + k * pp[k]
    #       }
    #       deriv1 <- deriv1 + D * a * (resp[i] - BX)
    #       deriv2 <- deriv2 + D^2 * a^2 * (AX - BX^2)
    #     }
    #     deriv2 <- -deriv2
    #     SEM <- 1/sqrt(abs(deriv2))
    #     post.theta <- pre.theta - deriv1/deriv2
    #     change <- abs(post.theta - pre.theta)
    #     nIter <- nIter + 1
    #   }
    # }
    if (post.theta < minTheta) {
      MLE <- minTheta
    }
    else if (post.theta > maxTheta) {
      MLE <- maxTheta
    }
    else {
      MLE <- post.theta
    }
  }
  return(list(THETA = MLE, SEM = SEM, LH = EAP.estimates$LH,
              posterior = EAP.estimates$posterior))
}
# plot.theta.audit.trail <- function() {
#   par(mfrow = c(2, 1))
#   plot(1:maxNI, seq(minTheta, maxTheta, length = maxNI),
#        main = paste("CAT Audit Trail - Examinee ", j, sep = ""),
#        xlab = "Items Administered", ylab = "Theta", type = "n",
#        las = 1)
#   points(1:ni.given, theta.history[j, 1:ni.given], type = "b",
#          pch = 9, col = "blue")
#   abline(h = theta.CAT[j], lty = 2, col = "red")
#   item.string <- paste(items.used[j, 1:ni.given], collapse = ",")
#   text(1, maxTheta, paste("Items: ", item.string, sep = ""),
#        cex = 0.7, adj = 0)
#   text(1, minTheta + 0.3, paste("Theta: ", round(estimates$THETA,
#                                                  digits = 2), " SE: ", round(estimates$SEM, digits = 2)),
#        cex = 0.8, adj = 0)
#   if (content.balancing)
#     text(1, maxTheta - 0.7, paste("Content Distribution:",
#                                   paste(round(current.content.dist, digits = 2),
#                                         collapse = ",")), cex = 0.7, adj = 0)
#   for (i in 1:ni.given) {
#     lines(rep(i, 2), c(theta.history[j, i] - 1.96 *
#                          se.history[j, i], theta.history[j, i] + 1.96 *
#                          se.history[j, i]))
#   }
#   resp.string <- paste(resp.matrix[j, items.used[j, 1:ni.given]],
#                        collapse = ",")
#   plot(theta, posterior.matrix[j, ], main = "Final Posterior Distribution",
#        xlab = "Theta", ylab = "Posterior", type = "l",
#        col = "blue", yaxt = "n")
#   text(minTheta, max(posterior.matrix[j, ]), paste("Responses: ",
#                                                    resp.string, sep = ""), cex = 0.7, adj = 0)
# }
# plot.item.usage <- function() {
#   par(mfrow = c(1, 1))
#   if (simulateTheta) {
#     if (toupper(popDist) == "GRID") {
#       boxplot(rowSums(!is.na(items.used)) ~ true.theta,
#               col = "skyblue", boxwex = 0.5, ylim = c(minNI,
#                                                       maxNI), names = (format(popPar, digits = 1)),
#               xlim = c(1, length(popPar)), xlab = "True Theta",
#               ylab = "Number of Items Administered")
#       for (i in minNI:maxNI) {
#         abline(h = i, lty = 3, col = "light grey")
#       }
#     }
#     else {
#       plot(true.theta, jitter(rowSums(!is.na(items.used)),
#                               amount = 0.01), ylim = c(minNI, maxNI), xlab = "True Theta",
#            ylab = "Number of Items Administered", col = 4,
#            las = 1)
#       grid()
#     }
#   }
#   else {
#     plot(theta.CAT, jitter(rowSums(!is.na(items.used)),
#                            amount = 0.01), ylim = c(minNI, maxNI), xlab = "CAT Theta",
#          ylab = "Number of Items Administered", col = 4,
#          las = 1)
#     grid()
#   }
#   pct.items.used <- numeric(ni)
#   tot.ni.used <- sum(!is.na(items.used))
#   for (i in 1:ni) {
#     pct.items.used[i] <- sum(items.used == i, na.rm = T) *
#       100/tot.ni.used
#   }
#   plot(c(1, ni), c(0, max(pct.items.used)), type = "n",
#        xlab = "Items", ylab = "Percent Used", las = 1)
#   for (i in 1:ni) {
#     lines(rep(i, 2), c(0, pct.items.used[i]), col = "blue")
#   }
# }
# plot.item.info <- function() {
#   par(mfrow = c(1, 1))
#   bank.info <- rowSums(matrix.info)
#   bank.se <- 1/sqrt(bank.info)
#   bticks <- pretty(theta, ceiling(maxTheta - minTheta))
#   rticks <- pretty(1/sqrt(bank.info))
#   scale.factor <- (max(bank.info) - min(bank.info))/(max(bank.se) -
#                                                        min(bank.se))
#   plot(theta, bank.info, type = "l", xaxt = "n", las = 2,
#        xlab = "Theta", ylab = "Total Information", lty = 1)
#   axis(4, at = rticks * scale.factor, labels = rticks,
#        tck = 0.01, lty = 2)
#   mtext("Standard Error", side = 4, line = 3)
#   axis(1, at = bticks, labels = bticks, tck = 0.01, lty = 2)
#   lines(theta, scale.factor * bank.se, lty = 3)
#   legend(min(theta), max(bank.info), legend = c("Information",
#                                                 "SE"), lty = c(1, 3), bg = "white")
#   par(mfrow = c(3, 4))
#   max.info <- max(matrix.info)
#   for (i in 1:ni) {
#     plot(theta, seq(0, max.info, length = length(theta)),
#          type = "n", xlab = "Theta", ylab = "Information",
#          main = paste("Item", i, sep = " "))
#     lines(theta, matrix.info[, i], lty = 1, col = 4)
#     theta.at.max <- theta[which(matrix.info[, i] ==
#                                   max(matrix.info[, i]))]
#     points(theta.at.max, 0, pch = "|", col = 6)
#     text(mean(c(minTheta, maxTheta)), max.info, paste("Max at Theta=",
#                                                       round(theta.at.max, dig = 1), sep = ""), cex = 0.7)
#   }
# }
# plot.item.prob <- function() {
#   par(mfrow = c(3, 4))
#   for (i in 1:ni) {
#     ncat <- NCAT[i]
#     plot(theta, seq(0, 1, length = length(theta)), type = "n",
#          xlab = "Theta", ylab = "Probability", main = paste("Item",
#                                                             i, sep = " "))
#     for (k in 1:ncat) {
#       lines(theta, pp[, i, k], lty = k, col = k)
#     }
#     legend(min(theta), 1, legend = 1:ncat, lty = 1:ncat,
#            cex = 0.5, col = 1:ncat, bg = "white")
#   }
# }
run.final.theta.estimators <- function() {
  info.wt <- sqrt(rowSums(matrix.info))
  WLH.matrix <- LH.matrix * info.wt
  find.max <- function(vec) {
    theta[which(vec == max(vec))]
  }
  theta.MAP <- apply(posterior.matrix, 1, find.max)
  theta.MLE <- apply(LH.matrix, 1, find.max)
  theta.WLE <- apply(WLH.matrix, 1, find.max)
  final.theta.estimators <- data.frame(EAP = theta.CAT,
                                       MAP = theta.MAP, MLE = theta.MLE, WLE = theta.WLE)
  return(final.theta.estimators)
}
# plot.max.info <- function() {
#   par(mfrow = c(1, 1))
#   matrix.order <- t(apply(matrix.info, 1, order, decreasing = T))
#   sorted.info <- matrix(0, nrow(matrix.order), ncol(matrix.order))
#   for (j in 1:nrow(matrix.order)) {
#     sorted.info[j, ] <- matrix.info[j, matrix.order[j,
#                                                     ]]
#   }
#   cum.info <- t(apply(sorted.info, 1, cumsum))
#   bticks <- pretty(theta, ceiling(maxTheta - minTheta))
#   plot(theta, cum.info[, 1], xlab = "Theta", ylab = "Max Attainable Information",
#        ylim = c(0, max(cum.info)), type = "n", cex.lab = 1,
#        las = 1, xaxt = "n")
#   axis(1, at = bticks, labels = bticks, tck = 1, lty = 2,
#        col = "grey")
#   box()
#   abline(h = 1/maxSE^2, col = "blue", lty = 2)
#   text(minTheta, 1/maxSE^2, paste("SE=", maxSE, sep = ""),
#        adj = c(0, 0), cex = 0.8)
#   for (i in 1:ni) {
#     color <- ifelse(i%%5 == 0, "black", "grey")
#     lines(theta, cum.info[, i], col = color)
#     text(theta[which(cum.info[, i] == max(cum.info[,
#                                                    i]))], max(cum.info[, i]), i, cex = 0.5, adj = c(0,
#                                                                                                     0), col = "red")
#   }
# }
calc.e.score <- function(th) {
  nq <- length(th)
  e.score <- matrix(0, nq, ni)
  if (model == "GRM") {
    for (i in 1:ni) {
      pp <- matrix(0, nq, NCAT[i])
      ps <- matrix(0, nq, NCAT[i] + 1)
      ps[, 1] <- 1
      ps[, NCAT[i] + 1] <- 0
      for (k in 1:(NCAT[i] - 1)) {
        ps[, k + 1] <- 1/(1 + exp(-D * DISC[i] * (th -
                                                    CB[i, k])))
      }
      pp[, 1] <- 1 - ps[, 1]
      pp[, NCAT[i]] <- ps[, NCAT[i]]
      for (k in 1:NCAT[i]) {
        pp[, k] = ps[, k] - ps[, k + 1]
        e.score[, i] <- e.score[, i] + pp[, k] * k
      }
    }
  }
  # else if (model == "GPCM") {
  #   for (i in 1:ni) {
  #     pp <- matrix(0, nq, NCAT[i])
  #     cb <- c(0, unlist(CB[i, 1:(NCAT[i] - 1)]))
  #     zz <- matrix(0, nq, NCAT[i])
  #     sdsum <- 0
  #     den <- rep(0, nq)
  #     for (k in 1:NCAT[i]) {
  #       sdsum <- sdsum + cb[k]
  #       zz[, k] <- exp(D * DISC[i] * (k * th - sdsum))
  #       den <- den + zz[, k]
  #     }
  #     for (k in 1:NCAT[i]) {
  #       pp[, k] <- zz[, k]/den
  #       e.score[, i] <- e.score[, i] + pp[, k] * k
  #     }
  #   }
  # }
  return(e.score)
}
# plot.Q3 <- function(theta) {
#   par(mfrow = c(1, 1))
#   es <- calc.e.score(theta)
#   res <- resp.matrix - es
#   Q3 <- cor(res, use = "pairwise.complete.obs")
#   plot(1:ni, 1:ni, type = "n", ylab = "Item", xlab = "Item",
#        xaxt = "n", yaxt = "n")
#   axis(1, at = 1:ni, cex = 0.5)
#   axis(2, at = 1:ni, cex = 0.5)
#   axis(3, at = 1:ni, cex = 0.5)
#   axis(4, at = 1:ni, cex = 0.5)
#   for (r in 1:ni) {
#     for (c in 1:ni) {
#       if (r > c) {
#         if (abs(Q3[r, c]) >= 0.4) {
#           points(r, c, pch = 16, col = "red")
#           abline(h = c, lty = 2, col = "dark grey")
#           abline(v = r, lty = 2, col = "dark grey")
#         }
#         else if (abs(Q3[r, c]) >= 0.3)
#           points(r, c, pch = 10)
#         else if (abs(Q3[r, c]) >= 0.2)
#           points(r, c, pch = 21)
#       }
#     }
#   }
#   legend(1, ni, c("| r | >= 0.4", "| r | >= 0.3", "| r | >= 0.2"),
#          pch = c(16, 10, 21), bg = "white", col = c("red",
#                                                     "black", "black"))
# }
check.extreme.response <- function() {
  flag <- FALSE
  if (toupper(extreme.response.check) %in% c("L", "R",
                                             "H", "E")) {
    if (ni.given == max.extreme.response) {
      resp.string <- paste0(selected.item.resp[j,
                                               1:ni.given], collapse = "")
      if (toupper(extreme.response.check) == "L") {
        if (resp.string == paste0(rep(1, ni.given),
                                  collapse = "")) {
          flag <- TRUE
        }
      }
      else if (toupper(extreme.response.check) %in%
               c("R", "H")) {
        if (resp.string == paste0(NCAT[items.used[j,
                                                  1:ni.given]], collapse = "")) {
          flag <- TRUE
        }
      }
      else {
        if (resp.string == paste0(rep(1, ni.given),
                                  collapse = "") || resp.string == paste0(NCAT[items.used[j,
                                                                                          1:ni.given]], collapse = "")) {
          flag <- TRUE
        }
      }
    }
  }
  return(flag)
}
check.SE.change <- function() {
  flag <- FALSE
  if (min.SE.change > 0 && ni.given >= minNI && ni.given >=
      2) {
    if ((se.history[j, ni.given - 1] - se.history[j,
                                                  ni.given]) < min.SE.change & (se.history[j, ni.given - 1] - se.history[j,
                                                                                                                         ni.given]) > 0) {
      flag <- TRUE
    }
  }
  return(flag)
}
if (Sys.info()["sysname"] == "Windows")
  windows(record = T)
#if (toupper(selection.method) == "MFI") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     if (first.item.selection == 4)
#       theta.current <- ext.theta$theta[j]
#     else theta.current <- start.theta
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       array.info <- calcInfo(theta.current)
#       ni.available <- sum(array.info > 0)
#       info.index <- order(array.info, decreasing = TRUE)
#       item.selected <- select.maxInfo()
#       if (ni.given == 0) {
#         if (first.item.selection == 3 && first.item >=
#             1 && first.item <= ni) {
#           if (items.available[first.item] == TRUE) {
#             item.selected <- first.item
#           }
#         }
#         else item.selected <- select.maxInfo()
#       }
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       if (toupper(interim.Theta) == "EAP") {
#         estimates <- calcEAP(j, ni.given)
#       }
#       else if (toupper(interim.Theta) == "MLE") {
#         estimates <- calcMLE(j, ni.given)
#       }
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "MLWI") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     if (first.item.selection == 4)
#       theta.current <- ext.theta$theta[j]
#     else theta.current <- start.theta
#     likelihood <- rep(1, length(theta))
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       array.info <- calc.LW.info(likelihood)
#       ni.available <- sum(array.info > 0)
#       info.index <- order(array.info, decreasing = TRUE)
#       item.selected <- select.maxInfo()
#       if (ni.given == 0) {
#         if (first.item.selection == 3 && first.item >=
#             1 && first.item <= ni) {
#           if (items.available[first.item] == TRUE) {
#             item.selected <- first.item
#           }
#         }
#         else if (first.item.selection == 2 || first.item.selection ==
#                  4) {
#           array.info <- calcInfo(theta.current)
#           info.index <- order(array.info, decreasing = TRUE)
#           item.selected <- select.maxInfo()
#         }
#       }
#       resp <- resp.matrix[j, item.selected]
#       prob <- pp[, item.selected, resp]
#       likelihood <- likelihood * prob
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       if (toupper(interim.Theta) == "EAP") {
#         estimates <- calcEAP(j, ni.given)
#       }
#       else if (toupper(interim.Theta) == "MLS") {
#         estimates <- calcMLE(j, ni.given)
#       }
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
if (toupper(selection.method) == "MPWI") {
  for (j in 1:nExaminees) {
    critMet <- FALSE
    items.available <- rep(TRUE, ni)
    items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
    if (content.balancing) {
      current.content.dist <- numeric(ncc)
      current.content.freq <- numeric(ncc)
      if (any(target.content.dist == 0))
        items.available[content.cat %in% which(target.content.dist ==
                                                 0)] <- FALSE
    }
    max.to.administer <- ifelse(sum(items.available) <=
                                  maxNI, sum(items.available), maxNI)
    ni.given <- 0
    if (first.item.selection == 4)
      theta.current <- ext.theta$theta[j]
    else theta.current <- start.theta
    posterior <- prior
    while (critMet == FALSE && ni.given < max.to.administer) {
      array.info <- calc.PW.info(posterior)
      ni.available <- sum(array.info > 0)
      info.index <- order(array.info, decreasing = TRUE)
      item.selected <- select.maxInfo()
      if (ni.given == 0) {
        if (first.item.selection == 3 && first.item >=
            1 && first.item <= ni) {
          if (items.available[first.item] == TRUE) {
            item.selected <- first.item
          }
        }
        else if (first.item.selection == 2 || first.item.selection ==
                 4) {
          array.info <- calcInfo(theta.current)
          info.index <- order(array.info, decreasing = TRUE)
          item.selected <- select.maxInfo()
        }
      }
      resp <- resp.matrix[j, item.selected]
      prob <- pp[, item.selected, resp]
      posterior <- posterior * prob
      ni.given <- ni.given + 1
      items.used[j, ni.given] <- item.selected
      if (content.balancing)
        update.content.dist()
      items.available[item.selected] <- FALSE
      selected.item.resp[j, ni.given] <- resp.matrix[j,
                                                     item.selected]
      estimates <- calcEAP(j, ni.given)
      theta.history[j, ni.given] <- estimates$THETA
      se.history[j, ni.given] <- estimates$SEM
      theta.current <- estimates$THETA
      if (ni.given >= max.to.administer || (estimates$SEM <=
                                            maxSE && ni.given >= minNI) || check.extreme.response() ||
          check.SE.change()) {
        critMet <- TRUE
        theta.CAT[j] <- estimates$THETA
        sem.CAT[j] <- estimates$SEM
        LH.matrix[j, ] <- estimates$LH
        posterior.matrix[j, ] <- estimates$posterior
        ni.administered[j] <- ni.given
      }
    }
    if (show.theta.audit.trail)
      plot.theta.audit.trail()
  }
}
#if (toupper(selection.method) == "MEI") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     if (first.item.selection == 4)
#       theta.current <- ext.theta$theta[j]
#     else theta.current <- start.theta
#     posterior <- prior
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       array.info <- calc.Expected.Info(posterior,
#                                        theta.current)
#       ni.available <- sum(array.info > 0)
#       info.index <- order(array.info, decreasing = TRUE)
#       item.selected <- select.maxInfo()
#       if (ni.given == 0) {
#         if (first.item.selection == 3 && first.item >=
#             1 && first.item <= ni) {
#           if (items.available[first.item] == TRUE) {
#             item.selected <- first.item
#           }
#         }
#         else if (first.item.selection == 2 || first.item.selection ==
#                  4) {
#           array.info <- calcInfo(theta.current)
#           info.index <- order(array.info, decreasing = TRUE)
#           item.selected <- select.maxInfo()
#         }
#       }
#       resp <- resp.matrix[j, item.selected]
#       prob <- pp[, item.selected, resp]
#       posterior <- posterior * prob
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "MEPV") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     if (first.item.selection == 4)
#       theta.current <- ext.theta$theta[j]
#     else theta.current <- start.theta
#     posterior <- prior
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       array.info <- calc.Expected.Var(posterior, theta.current)
#       ni.available <- sum(array.info > 0)
#       info.index <- order(array.info, decreasing = TRUE)
#       item.selected <- select.maxInfo()
#       if (ni.given == 0) {
#         if (first.item.selection == 3 && first.item >=
#             1 && first.item <= ni) {
#           if (items.available[first.item] == TRUE) {
#             item.selected <- first.item
#           }
#         }
#         else if (first.item.selection == 2 || first.item.selection ==
#                  4) {
#           array.info <- calcInfo(theta.current)
#           info.index <- order(array.info, decreasing = TRUE)
#           item.selected <- select.maxInfo()
#         }
#       }
#       resp <- resp.matrix[j, item.selected]
#       prob <- pp[, item.selected, resp]
#       posterior <- posterior * prob
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "MEPWI") {
#  for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     if (first.item.selection == 4)
#       theta.current <- ext.theta$theta[j]
#     else theta.current <- start.theta
#     posterior <- prior
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       array.info <- calc.Expected.PW.Info(posterior,
#                                           theta.current)
#       ni.available <- sum(array.info > 0)
#       info.index <- order(array.info, decreasing = TRUE)
#       item.selected <- select.maxInfo()
#       if (ni.given == 0) {
#         if (first.item.selection == 3 && first.item >=
#             1 && first.item <= ni) {
#           if (items.available[first.item] == TRUE) {
#             item.selected <- first.item
#           }
#         }
#         else if (first.item.selection == 2 || first.item.selection ==
#                  4) {
#           array.info <- calcInfo(theta.current)
#           info.index <- order(array.info, decreasing = TRUE)
#           item.selected <- select.maxInfo()
#         }
#       }
#       resp <- resp.matrix[j, item.selected]
#       prob <- pp[, item.selected, resp]
#       posterior <- posterior * prob
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "RND") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       random <- runif(ni)
#       random[!items.available] <- 0
#       if (content.balancing)
#         random[content.cat != next.content()] <- 0
#       item.selected <- order(random, decreasing = TRUE)[1]
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "KET") {
#   info.table <- prep.info.ext.theta()
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       array.info <- info.table[j, ]
#       array.info[!items.available] <- 0
#       if (content.balancing)
#         array.info[content.cat != next.content()] <- 0
#       item.selected <- order(array.info, decreasing = TRUE)[1]
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "LOC") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     if (first.item.selection == 4)
#       theta.current <- ext.theta$theta[j]
#     else theta.current <- start.theta
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       array.info <- calc.Loc.info(theta.current)
#       ni.available <- sum(array.info > 0)
#       info.index <- order(array.info, decreasing = TRUE)
#       item.selected <- select.maxInfo()
#       if (ni.given == 0) {
#         if (first.item.selection == 3 && first.item >=
#             1 && first.item <= ni) {
#           if (items.available[first.item] == TRUE) {
#             item.selected <- first.item
#           }
#         }
#         else if (first.item.selection == 2 || first.item.selection ==
#                  4) {
#           array.info <- calcInfo(theta.current)
#           info.index <- order(array.info, decreasing = TRUE)
#           item.selected <- select.maxInfo()
#         }
#       }
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "SEQ") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     item.order <- 1:ni
#     if (sum(items.available) < ni)
#       item.order <- item.order[-which(items.available ==
#                                         FALSE)]
#     while (critMet == FALSE && ni.given < length(item.order)) {
#       item.selected <- item.order[ni.given + 1]
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "TSB") {
#   items.available <- rep(TRUE, ni)
#   array.info <- calc.PW.info(prior)
#   info.index <- order(array.info, decreasing = TRUE)
#   locator.item <- info.index[1]
#   ncat <- NCAT[locator.item]
#   sequence.matrix <- matrix(NA, ncat, ni)
#   posterior.k <- matrix(rep(prior, ncat), ncat, length(prior),
#                         byrow = T)
#   for (k in 1:ncat) {
#     posterior.k[k, ] <- prior * pp[, locator.item, k]
#     array.info <- calc.PW.info(posterior.k[k, ])
#     info.index <- order(array.info, decreasing = TRUE)
#     info.index <- info.index[-which(info.index == locator.item)]
#     sequence.matrix[k, ] <- c(locator.item, info.index)
#   }
#   par(mfrow = c(1, 1))
#   plot(1:15, seq(1, (ncat + 4), length = 15), xaxt = "n",
#        yaxt = "n", type = "n", xlab = "", ylab = "")
#   text(2, round(median(1:ncat)) + 2, paste("Locator: ",
#                                            locator.item, sep = ""))
#   for (k in 1:ncat) {
#     text(5, ncat + 3 - k, paste("(", k, ")", sep = ""))
#     text(6, ncat + 3 - k, paste(sequence.matrix[k, 2:maxNI],
#                                 collapse = "-"), adj = c(0), cex = 0.7, col = "blue")
#   }
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     locator.response <- resp.matrix[j, locator.item]
#     if (is.na(locator.response) == TRUE)
#       locator.response <- (round(median(1:ncat)))
#     item.order <- sequence.matrix[locator.response,
#                                   ]
#     if (sum(items.available) < ni)
#       item.order <- setdiff(item.order, which(items.available ==
#                                                 FALSE))
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       item.selected <- item.order[ni.given + 1]
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "PSER") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     old.SE <- 1
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     if (first.item.selection == 4)
#       theta.current <- ext.theta$theta[j]
#     else theta.current <- start.theta
#     posterior <- prior
#     while (ni.given < max.to.administer) {
#       array.se <- calc.Predicted.PSD(posterior, theta.current)
#       array.info <- 1/array.se^2
#       array.info[is.na(array.info)] <- 0
#       ni.available <- sum(array.info > 0)
#       info.index <- order(array.info, decreasing = TRUE)
#       item.selected <- select.maxInfo()
#       if (ni.given == 0) {
#         if (first.item.selection == 3 && first.item >=
#             1 && first.item <= ni) {
#           if (items.available[first.item] == TRUE) {
#             item.selected <- first.item
#           }
#         }
#         else if (first.item.selection == 2 || first.item.selection ==
#                  4) {
#           array.info <- calcInfo(theta.current)
#           info.index <- order(array.info, decreasing = TRUE)
#           item.selected <- select.maxInfo()
#         }
#         else item.selected <- select.maxInfo()
#       }
#       se.change <- old.SE - array.se[item.selected]
#       if (ni.given >= minNI) {
#         if ((old.SE <= maxSE) && (se.change < continueSE)) {
#           break
#         }
#         else if ((old.SE > maxSE) && (se.change <
#                                       stopSE)) {
#           break
#         }
#       }
#       if (check.extreme.response()) {
#         break
#       }
#       resp <- resp.matrix[j, item.selected]
#       prob <- pp[, item.selected, resp]
#       posterior <- posterior * prob
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       old.SE <- estimates$SEM
#       theta.current <- estimates$THETA
#       theta.CAT[j] <- estimates$THETA
#       sem.CAT[j] <- estimates$SEM
#       LH.matrix[j, ] <- estimates$LH
#       posterior.matrix[j, ] <- estimates$posterior
#       ni.administered[j] <- ni.given
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
#if (toupper(selection.method) == "MI") {
#   for (j in 1:nExaminees) {
#     critMet <- FALSE
#     items.available <- rep(TRUE, ni)
#     items.available[is.na(resp.matrix[j, 1:ni])] <- FALSE
#     if (content.balancing) {
#       current.content.dist <- numeric(ncc)
#       current.content.freq <- numeric(ncc)
#       if (any(target.content.dist == 0))
#         items.available[content.cat %in% which(target.content.dist ==
#                                                  0)] <- FALSE
#     }
#     max.to.administer <- ifelse(sum(items.available) <=
#                                   maxNI, sum(items.available), maxNI)
#     ni.given <- 0
#     if (first.item.selection == 4)
#       theta.current <- ext.theta$theta[j]
#     else theta.current <- start.theta
#     posterior <- prior
#     while (critMet == FALSE && ni.given < max.to.administer) {
#       array.info <- calc.MI(posterior)
#       ni.available <- sum(array.info > 0)
#       info.index <- order(array.info, decreasing = TRUE)
#       item.selected <- select.maxInfo()
#       if (ni.given == 0) {
#         if (first.item.selection == 3 && first.item >=
#             1 && first.item <= ni) {
#           if (items.available[first.item] == TRUE) {
#             item.selected <- first.item
#           }
#         }
#         else if (first.item.selection == 2 || first.item.selection ==
#                  4) {
#           array.info <- calcInfo(theta.current)
#           info.index <- order(array.info, decreasing = TRUE)
#           item.selected <- select.maxInfo()
#         }
#       }
#       resp <- resp.matrix[j, item.selected]
#       prob <- pp[, item.selected, resp]
#       posterior <- posterior * prob
#       ni.given <- ni.given + 1
#       items.used[j, ni.given] <- item.selected
#       if (content.balancing)
#         update.content.dist()
#       items.available[item.selected] <- FALSE
#       selected.item.resp[j, ni.given] <- resp.matrix[j,
#                                                      item.selected]
#       estimates <- calcEAP(j, ni.given)
#       theta.history[j, ni.given] <- estimates$THETA
#       se.history[j, ni.given] <- estimates$SEM
#       theta.current <- estimates$THETA
#       if (ni.given >= max.to.administer || (estimates$SEM <=
#                                             maxSE && ni.given >= minNI) || check.extreme.response() ||
#           check.SE.change()) {
#         critMet <- TRUE
#         theta.CAT[j] <- estimates$THETA
#         sem.CAT[j] <- estimates$SEM
#         LH.matrix[j, ] <- estimates$LH
#         posterior.matrix[j, ] <- estimates$posterior
#         ni.administered[j] <- ni.given
#       }
#     }
#     if (show.theta.audit.trail)
#       plot.theta.audit.trail()
#   }
# }
# par(mfrow = c(1, 1))
# if (simulateTheta) {
#   cor.theta <- round(cor(true.theta, theta.CAT), 3)
#   rmsd.theta <- round(sqrt(mean((true.theta - theta.CAT)^2)))
#   if (toupper(popDist) == "GRID") {
#     boxplot(theta.CAT ~ true.theta, col = "yellow",
#             boxwex = 0.5, ylim = c(minTheta, maxTheta),
#             names = (format(popPar, digits = 1)), xlim = c(1,
#                                                            length(popPar)), xlab = "True Theta", ylab = "CAT Theta",
#             main = "CAT vs. True Theta")
#     text(popPar[1], maxTheta, adj = 0, paste("r = ",
#                                              cor.theta, sep = ""))
#     segments(1, minTheta, length(popPar), maxTheta,
#              col = "red", lwd = 2, lty = 2)
#     for (i in minTheta:maxTheta) {
#       abline(h = i, lty = 3, col = "light grey")
#     }
#   }
#   else {
#     plot(minTheta:maxTheta, minTheta:maxTheta, xlab = "True Theta",
#          ylab = "CAT Theta", main = "CAT vs. True Theta",
#          type = "n", las = 1)
#     points(true.theta, theta.CAT, col = "blue")
#     text(minTheta, maxTheta, adj = 0, paste("r = ",
#                                             cor.theta, sep = ""))
#     abline(0, 1)
#     grid()
#   }
# }
# else if (eapFullLength) {
#   cor.theta <- round(cor(ext.theta$theta, theta.CAT),
#                      3)
#   rmsd.theta <- round(sqrt(mean((ext.theta$theta - theta.CAT)^2)))
#   plot(minTheta:maxTheta, minTheta:maxTheta, xlab = "Full-Bank Theta",
#        ylab = "CAT Theta", main = "CAT vs. Full-Bank Theta Estimates",
#        type = "n", las = 1)
#   points(ext.theta$theta, theta.CAT, col = "blue")
#   text(minTheta, maxTheta, adj = 0, paste("r = ", cor.theta,
#                                           sep = ""))
#   abline(0, 1)
#   grid()
# }
# else {
#   cor.theta <- round(cor(ext.theta$theta, theta.CAT),
#                      3)
#   rmsd.theta <- round(sqrt(mean((ext.theta$theta - theta.CAT)^2)))
#   plot(minTheta:maxTheta, minTheta:maxTheta, xlab = "External Theta",
#        ylab = "CAT Theta", main = "CAT vs. External Theta Estimates",
#        type = "n", las = 1)
#   points(ext.theta$theta, theta.CAT, col = "blue")
#   text(minTheta, maxTheta, adj = 0, paste("r = ", cor.theta,
#                                           sep = ""))
#   abline(0, 1)
#   grid()
# }
# if (plot.usage)
#   plot.item.usage()
# if (content.balancing) {
#   overall.content.dist <- overall.content.freq/sum(overall.content.freq)
#   content.dist <- rbind(target.content.dist, overall.content.dist)
#   par.mar <- par()$mar
#   par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
#   barplot(content.dist, ylab = "Proportion", xlab = "Content Category",
#           las = 1, names.arg = paste(1:ncc), col = c("black",
#                                                      "grey"), beside = T)
#   legend(ncc * 3 + 0.5, max(target.content.dist, current.content.dist)/2,
#          c("Target", "Current"), fill = c("black", "grey"))
#   par(xpd = F, mar = par.mar)
# }
# if (bank.diagnosis) {
#   plot.max.info()
#   plot.Q3(ext.theta$theta)
# }
# if (plot.info)
#   plot.item.info()
# if (plot.prob)
#   plot.item.prob()
# if (add.final.theta || !(file.other.thetas == "")) {
#   final.thetas <- run.final.theta.estimators()
#   if (add.final.theta) {
#     pairs(final.thetas, panel = function(x, y) {
#       points(x, y, col = 4)
#       abline(0, 1, lwd = 2)
#     }, diag.panel = function(x) {
#       par(new = T)
#       hist(x, main = "", axes = F, nclass = 12, probability = T)
#       lines(density(x))
#       points(x, rep(0, length(x)), pch = "|")
#     })
#   }
#   if (!(file.other.thetas == "")) {
#     write.table(final.thetas, file = file.other.thetas,
#                 sep = ",", na = " ", row.names = FALSE, col.names = TRUE)
#   }
# }
# if (!(file.items.used == "")) {
#   colnames(items.used) <- paste("Item", seq(1:maxNI),
#                                 sep = "")
#   write.table(items.used, file = file.items.used, sep = ",",
#               na = " ", row.names = FALSE, col.names = TRUE)
# }
# if (!(file.theta.history == "")) {
#   colnames(theta.history) <- paste("Item", seq(1:maxNI),
#                                    sep = "")
#   write.table(theta.history, file = file.theta.history,
#               sep = ",", na = " ", row.names = FALSE, col.names = TRUE)
# }
# if (!(file.se.history == "")) {
#   colnames(se.history) <- paste("Item", seq(1:maxNI),
#                                 sep = "")
#   write.table(se.history, file = file.se.history, sep = ",",
#               na = " ", row.names = FALSE, col.names = TRUE)
# }
final.theta.se <- as.data.frame(cbind(theta.CAT, sem.CAT))
# if (!(file.final.theta.se == "")) {
#   colnames(final.theta.se) <- c("Theta", "SEM")
#   write.table(final.theta.se, file = file.final.theta.se,
#               sep = ",", na = " ", row.names = FALSE, col.names = TRUE)
# }
# if (!(file.likelihood.dist == "")) {
#   colnames(LH.matrix) <- paste("Theta=", theta, sep = "")
#   write.table(LH.matrix, file = file.likelihood.dist,
#               sep = ",", na = " ", row.names = FALSE, col.names = TRUE)
# }
# if (!(file.posterior.dist == "")) {
#   colnames(posterior.matrix) <- paste("Theta=", theta,
#                                       sep = "")
#   write.table(posterior.matrix, file = file.posterior.dist,
#               sep = ",", na = " ", row.names = FALSE, col.names = TRUE)
# }
# if (!(file.matrix.info == "")) {
#   colnames(matrix.info) <- paste("Item", 1:ni, sep = "")
#   write.table(matrix.info, file = file.matrix.info, sep = ",",
#               na = " ", row.names = FALSE, col.names = TRUE)
# }
# if (!(file.full.length.theta == "")) {
#   write.table(ext.theta, file = file.full.length.theta,
#               sep = ",", na = " ", row.names = FALSE, col.names = TRUE)
# }
# if (!(file.selected.item.resp == "")) {
#   colnames(selected.item.resp) <- paste("Item", seq(1:maxNI),
#                                         sep = "")
#   write.table(selected.item.resp, file = file.selected.item.resp,
#               sep = ",", na = " ", row.names = FALSE, col.names = TRUE)
# }
mean.nia <- sum(!is.na(items.used))/dim(resp.data)[1]
mean.SE <- mean(sem.CAT)
out <- list(call = call, mean.nia = mean.nia,
            #cor.theta = cor.theta,
            #rmsd.theta = rmsd.theta,
            true.theta = true.theta, mean.SE = mean.SE,
            item.par = item.par, resp = resp.matrix, items.used = items.used,
            theta.history = theta.history, se.history = se.history,
            selected.item.resp = selected.item.resp, final.theta.se = final.theta.se,
            likelihood.dist = LH.matrix,
            posterior.dist = posterior.matrix,
            matrix.info = matrix.info)
return(out)
}
