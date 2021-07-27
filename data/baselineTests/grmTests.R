
devtools::load_all(".")

load("./data/iparDep.rda")
load("./data/iparPedImp.rda")

setwd("./data/baselineTests")

#### Depression ####

# NIHTB and PROMIS Defaults

sink("Dep_Best3.txt")
cat("Depression Item Bank, 15 items, API settings with Best Health rule at 3 items.\n",
    "Settings: min/max Theta = -/+4, 81 quadpts; all responses = 1 [min score]; min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=1\n")

dep_best3 <- grmCAT(ipar=iparDep[,2:7],resp=matrix(rep(1,15),nrow=1),
                    minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                    muEAP=0, sdEAP=1, scoreCumulativeDiffuse = FALSE,
                    bestAt=3, deltaSD=0, verbose=T,bestStop = 'LO')
sink()

sink("Dep_PROMISdefault_all1.txt")
cat("Depression Item Bank, 15 items, PROMIS Defaults.\n",
    "Settings: min/max Theta = -/+4, 81 quadpts; all responses = 1 [min score]; min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=1\n")

dep_all1 <- grmCAT(ipar=iparDep[,2:7],resp=matrix(rep(1,15),nrow=1),
                   minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                   muEAP=0, sdEAP=1, scoreCumulativeDiffuse = FALSE,
                   bestAt=NULL, deltaSD=0, verbose=T)
sink()

sink("Dep_PROMISdefault_inc1to5.txt")
cat("Depression Item Bank, 15 items, PROMIS Defaults.\n",
    "Settings: min/max Theta = -/+4, 81 quadpts; responses increment from 1 to 5 and then repeat (3x); min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=1\n")

dep_inc1to5 <- grmCAT(ipar=iparDep[,2:7],resp=matrix(rep(c(1:5),times=3),nrow=1),
                      minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                      muEAP=0, sdEAP=1, scoreCumulativeDiffuse = FALSE,
                      bestAt=NULL, deltaSD=0, verbose=T)
sink()

# MTB Repeat Testing Defaults


sink("Dep_MTB_noPrevTheta_all1.txt")
cat("Depression Item Bank, 15 items, MTB repeat testing defaults with no previous theta score.\n",
    "Settings: min/max Theta = -/+6, 121 quadpts; all responses = 1 [min score]; min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=3\n")

dep_MTB_noSC1 <- grmCAT(ipar=iparDep[,2:7],resp=matrix(rep(1,15),nrow=1),
                        minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                        muEAP=0, sdEAP=3, scoreCumulativeDiffuse = T, prevTheta = NULL,
                        bestAt=NULL, deltaSD=0, verbose=T)
sink()

sink("Dep_MTB_noPrevTheta_inc1to5.txt")
cat("Depression Item Bank, 15 items, MTB repeat testing defaults with no previous theta score.\n",
    "Settings: min/max Theta = -/+6, 121 quadpts; responses increment from 1 to 5 and then repeat (3x); min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=3\n")

dep_MTB_noSCinc1to5 <- grmCAT(ipar=iparDep[,2:7],resp=matrix(rep(c(1:5),times=3),nrow=1),
                              minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                              muEAP=0, sdEAP=3, scoreCumulativeDiffuse = T,prevTheta = NULL,
                              bestAt=NULL, deltaSD=0, verbose=T)
sink()

sink("Dep_MTB_PrevThetaEq1_all1.txt")
cat("Depression Item Bank, 15 items, MTB repeat testing defaults with the previous theta equal to 1.0.\n",
    "Settings: min/max Theta = -/+6, 121 quadpts; all responses = 1 [min score]; min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=3; prevTheta=1.0\n")

dep_MTB_prevSC1_1 <- grmCAT(ipar=iparDep[,2:7],resp=matrix(rep(1,15),nrow=1),
                            minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                            muEAP=0, sdEAP=3, scoreCumulativeDiffuse = T,prevTheta = 1,
                            bestAt=NULL, deltaSD=0, verbose=T)
sink()

sink("Dep_MTB_PrevThetaEq1_inc1to5.txt")
cat("Depression Item Bank, 15 items, MTB repeat testing defaults with the previous theta equal to 1.0.\n",
    "Settings: min/max Theta = -/+6, 121 quadpts; responses increment from 1 to 5 and then repeat (3x); min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=6; prevTheta=1.0\n")

dep_MTB_prevSCinc1to5_1 <- grmCAT(ipar=iparDep[,2:7],resp=matrix(rep(c(1:5),times=3),nrow=1),
                                  minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                                  muEAP=0, sdEAP=3, scoreCumulativeDiffuse = T,prevTheta = 1,
                                  bestAt=NULL, deltaSD=0, verbose=T)
sink()

#### Pediatric Strength Impact ####

# NIHTB and PROMIS Defaults

sink("PedStrImp_Best3.txt")
cat("Strength Impact Item Bank, 10 items, API settings with Best Health rule at 3 items.\n",
    "Settings: min/max Theta = -/+4, 81 quadpts; all responses = 1 [min score]; min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=1\n")

PedStrImp_best3 <- grmCAT(ipar=iparPedImp[,2:7],resp=matrix(rep(1,10),nrow=1),
                          minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                          muEAP=0, sdEAP=1, scoreCumulativeDiffuse = FALSE,
                          bestAt=3, deltaSD=0, verbose=T,bestStop = 'LO')
sink()

sink("PedStrImp_PROMISdefault_all1.txt")
cat("Strength Impact Item Bank, 10 items, PROMIS Defaults.\n",
    "Settings: min/max Theta = -/+4, 81 quadpts; all responses = 1 [min score]; min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=1\n")

PedStrImp_all1 <- grmCAT(ipar=iparPedImp[,2:7],resp=matrix(rep(1,10),nrow=1),
                         minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                         muEAP=0, sdEAP=1, scoreCumulativeDiffuse = FALSE,
                         bestAt=NULL, deltaSD=0, verbose=T)
sink()

sink("PedStrImp_PROMISdefault_inc1to5.txt")
cat("Strength Impact Item Bank, 10 items, PROMIS Defaults.\n",
    "Settings: min/max Theta = -/+4, 81 quadpts; responses increment from 1 to 5 and then repeat (2x); min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=1\n")

PedStrImp_inc1to5 <- grmCAT(ipar=iparPedImp[,2:7],resp=matrix(rep(c(1:5),times=2),nrow=1),
                            minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                            muEAP=0, sdEAP=1, scoreCumulativeDiffuse = FALSE,
                            bestAt=NULL, deltaSD=0, verbose=T)
sink()

# MTB Repeat Testing Defaults


sink("PedStrImp_MTB_noPrevTheta_all1.txt")
cat("Strength Impact Item Bank, 10 items, MTB repeat testing defaults with no previous theta score.\n",
    "Settings: min/max Theta = -/+6, 121 quadpts; all responses = 1 [min score]; min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=3\n")

PedStrImp_MTB_noSC1 <- grmCAT(ipar=iparPedImp[,2:7],resp=matrix(rep(1,10),nrow=1),
                              minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                              muEAP=0, sdEAP=3, scoreCumulativeDiffuse = T, prevTheta = NULL,
                              bestAt=NULL, deltaSD=0, verbose=T)
sink()

sink("PedStrImp_MTB_noPrevTheta_inc1to5.txt")
cat("Strength Impact Item Bank, 10 items, MTB repeat testing defaults with no previous theta score.\n",
    "Settings: min/max Theta = -/+6, 121 quadpts; responses increment from 1 to 5 and then repeat (2x); min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=3\n")

PedStrImp_MTB_noSCinc1to5 <- grmCAT(ipar=iparPedImp[,2:7],resp=matrix(rep(c(1:5),times=2),nrow=1),
                                    minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                                    muEAP=0, sdEAP=3, scoreCumulativeDiffuse = T, prevTheta = NULL,
                                    bestAt=NULL, deltaSD=0, verbose=T)
sink()

sink("PedStrImp_MTB_PrevThetaEq1_all1.txt")
cat("Strength Impact Item Bank, 10 items, MTB repeat testing defaults with the previous theta equal to 1.0.\n",
    "Settings: min/max Theta = -/+6, 121 quadpts; all responses = 1 [min score]; min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=3; prevTheta=1.0\n")

PedStrImp_MTB_prevSC1_1 <- grmCAT(ipar=iparPedImp[,2:7],resp=matrix(rep(1,10),nrow=1),
                                  minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                                  muEAP=0, sdEAP=3, scoreCumulativeDiffuse = T, prevTheta = 1,
                                  bestAt=NULL, deltaSD=0, verbose=T)
sink()

sink("PedStrImp_MTB_PrevThetaEq1_inc1to5.txt")
cat("Strength Impact Item Bank, 10 items, MTB repeat testing defaults with the previous theta equal to 1.0.\n",
    "Settings: min/max Theta = -/+6, 121 quadpts; responses increment from 1 to 5 and then repeat (3x); min/max items = 4 and 8; maxSD = 0.3; muEAP=0; sdEAP=6; prevTheta=1.0\n")

PedStrImp_MTB_prevSCinc1to5_1 <- grmCAT(ipar=iparPedImp[,2:7],resp=matrix(rep(c(1:5),times=2),nrow=1),
                                        minTheta=-4, maxTheta=4, nQpts=81, minNI=4, maxNI=8,
                                        muEAP=0, sdEAP=3, scoreCumulativeDiffuse = T, prevTheta = 1,
                                        bestAt=NULL, deltaSD=0, verbose=T)
sink()

