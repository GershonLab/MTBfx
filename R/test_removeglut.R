# test of Firestar_removeglut.R function
# assesses whether output from Firestar and Firestar_removeglut is the same
# when successively removing currently unused features


# for theta limits [-6, 6]
test_firestar_theta6 <- Firestar_removeglut(filename.ipar = "PF_2.0_ipar.csv",
                                            filename.resp= "case1.csv",
                                            minTheta=-6, maxTheta=6, maxNI=8,min.SE.change=0.01,
                                            show.theta.audit.trail = F,
                                            #plot.usage = F,
                                            topN=1,
                                            model="GRM",selection.method='MPWI')


# for theta limits [-4, 4]


# original function
test_firestar_theta4 <- Firestar(filename.ipar = "PF_2.0_ipar.csv",
                                 filename.resp= "case1.csv",
                                 minTheta=-4, maxTheta=4, maxNI=8,min.SE.change=0.01,
                                 show.theta.audit.trail = F,
                                 inc = (1/15), # this produces 121 quadrature points in theta
                                 #plot.usage = F,
                                 topN=1,
                                 prior.dist = 2,
                             #    nSimulee = 100,
                                 model="GRM", selection.method='MPWI')


test_firestar_theta4_rm <- Firestar_removeglut(filename.ipar = "PF_2.0_ipar.csv",
                          filename.resp= "case1.csv",
                          minTheta=-4, maxTheta=4, maxNI=8,min.SE.change=0.01,
                          show.theta.audit.trail = F,
                          inc = (1/15), # this produces 121 quadrature points in theta
                          # in firestar the seq options is by = inc
                          # in grmCAT the seq options is length.out = nQpts

                          #plot.usage = F,
                          topN=1,
                          prior.dist = 2,
                          nSimulee = 2,
                          model="GRM", selection.method='MPWI')


test_grmCAT.v1_theta4 <- grmCAT.v1(ipar=PFipar, resp=resp5000[1,], minTheta=-4,
                            maxNI=8, nQpts=121, deltaSD=0.01,
                            maxTheta=4)



test_grmCAT.v1 <- grmCAT.v1(ipar=PFipar, resp=resp5000[1,], minTheta=-6,
                            maxNI=8,
                            maxTheta=6)



# function to test whether the two functions Firestar and Firestar_removeglut are produce
# equivalent outputs

fstar_output_equal <- function(firestar_orig, firestar_new,
                           list_names = c("true.theta", "mean.SE", "item.par",
                                          "resp", "items.used", "theta.history",
                                          "se.history", "selected.item.resp",
                                          "final.theta.se", "likelihood.dist",
                                          "posterior.dist",  "matrix.info")) {

  #firestar_orig and firestar_new are lists produced by the original and new functions
  sapply(1:length(list_names), function(i)
         all.equal(firestar_orig[list_names[i]],
         firestar_new[list_names[i]]))

}

fstar_output_equal(test_firestar_theta4_rm, test_firestar_theta4)
