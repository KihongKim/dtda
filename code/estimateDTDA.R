# estimate multilevel MNL models
# by Kihong Kim


setwd("/home/kihong/dissertation/e2_dtda") # on the sapporo server
library(mlogit)
load("mydata.15dt.RData")

# null model
ids = unique(mydata.15dt$dtID)
alt.ids = unique(mydata.15dt$alt)
logLik.null <- length(ids) * log(1/length(alt.ids))

# constants-only model
mnl.asc <- mlogit(chosen~alt, data=mydata.15dt, reflevel="ST")
summary(mnl.asc)
logLik.asc <- logLik(mnl.asc)

# create additional state dummies that are located in the first place of the right-hand side of mlogit fungction
mydata.15dt$thisH2_EO <- with(mydata.15dt, ifelse(alt=="EO", thisH2, 0))
mydata.15dt$thisH2_ES <- with(mydata.15dt, ifelse(alt=="ES", thisH2, 0))
mydata.15dt$thisH2_HC <- with(mydata.15dt, ifelse(alt=="HC", thisH2, 0))
mydata.15dt$thisH2_PB <- with(mydata.15dt, ifelse(alt=="PB", thisH2, 0))
mydata.15dt$thisH2_SH <- with(mydata.15dt, ifelse(alt=="SH", thisH2, 0))
mydata.15dt$thisH2_SR <- with(mydata.15dt, ifelse(alt=="SR", thisH2, 0))

mydata.15dt$thisHC_EO <- with(mydata.15dt, ifelse(alt=="EO", thisHC, 0))
mydata.15dt$thisHC_ES <- with(mydata.15dt, ifelse(alt=="ES", thisHC, 0))
mydata.15dt$thisHC_HC <- with(mydata.15dt, ifelse(alt=="HC", thisHC, 0))
mydata.15dt$thisHC_PB <- with(mydata.15dt, ifelse(alt=="PB", thisHC, 0))
mydata.15dt$thisHC_SH <- with(mydata.15dt, ifelse(alt=="SH", thisHC, 0))
mydata.15dt$thisHC_SR <- with(mydata.15dt, ifelse(alt=="SR", thisHC, 0))

# single-level MNL
mnl.sl <- mlogit(chosen~
                    thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR+
                    thisHC_EO+thisHC_ES+thisHC_HC+thisHC_PB+thisHC_SH+thisHC_SR|
                    thisEO+thisES+thisPB+thisSH+thisSR+
                    log(actDur15)+
                    hSIN1+hCOS1|0,
                  data=mydata.15dt, reflevel="ST")
summary(mnl.sl)
mnl.sl2 <- mlogit(chosen~
                   thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR+
                   thisHC_EO+thisHC_ES+thisHC_HC+thisHC_PB+thisHC_SH+thisHC_SR|
                   thisEO+thisES+thisPB+thisSH+thisSR+
                   log(actDur15)+
                   hSIN1+hCOS1+hSIN2+hCOS2|0,
                 data=mydata.15dt, reflevel="ST")
summary(mnl.sl2)
mnl.sl3 <- mlogit(chosen~
                    thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR+
                    thisHC_EO+thisHC_ES+thisHC_HC+thisHC_PB+thisHC_SH+thisHC_SR|
                    thisEO+thisES+thisPB+thisSH+thisSR+
                    log(actDur15)+
                    hSIN1+hCOS1+hSIN2+hCOS2+hSIN3+hCOS3|0,
                  data=mydata.15dt, reflevel="ST")
summary(mnl.sl3)
mnl.sl4 <- mlogit(chosen~
                    thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR+
                    thisHC_EO+thisHC_ES+thisHC_HC+thisHC_PB+thisHC_SH+thisHC_SR|
                    thisEO+thisES+thisPB+thisSH+thisSR+
                    log(actDur15)+
                    hSIN1+hCOS1+hSIN2+hCOS2+hSIN3+hCOS3+hSIN4+hCOS4|0,
                  data=mydata.15dt, reflevel="ST")
summary(mnl.sl4)
save(mnl.sl, file="mnl.sl.RData")

# multilevel MNL with variance only
t1 <- Sys.time()
mnl.ml.var <- mlogit(chosen~
                       thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR+
                       thisHC_EO+thisHC_ES+thisHC_HC+thisHC_PB+thisHC_SH+thisHC_SR|
                       thisEO+thisES+thisPB+thisSH+thisSR+
                       log(actDur15)+
                       hSIN1+hCOS1|0,
                     data=mydata.15dt, reflevel="ST",
                     rpar=c("H2:(intercept)"="n",
                            "H3:(intercept)"="n",
                            "ES:(intercept)"="n",
                            "EO:(intercept)"="n",
                            "HC:(intercept)"="n",
                            "PB:(intercept)"="n",
                            "SH:(intercept)"="n",
                            "SR:(intercept)"="n"),
                     R=1500, halton=NA, print.level=1, panel=TRUE)
t2 <- Sys.time()
t2-t1
summary(mnl.ml.var)
save(mnl.ml.var, file="mnl.ml.var.RData")

# multilevel with covariance
t3 <- Sys.time()
mnl.ml.cov <- mlogit(chosen~
                       thisH2_EO+thisH2_ES+thisH2_HC+thisH2_PB+thisH2_SH+thisH2_SR+
                       thisHC_EO+thisHC_ES+thisHC_HC+thisHC_PB+thisHC_SH+thisHC_SR|
                       thisEO+thisES+thisPB+thisSH+thisSR+
                       log(actDur15)+
                       hSIN1+hCOS1|0,
                     data=mydata.15dt, reflevel="ST",
                     rpar=c("H2:(intercept)"="n",
                            "H3:(intercept)"="n",
                            "ES:(intercept)"="n",
                            "EO:(intercept)"="n",
                            "HC:(intercept)"="n",
                            "PB:(intercept)"="n",
                            "SH:(intercept)"="n",
                            "SR:(intercept)"="n"),
                     R=1000, halton=NA, print.level=1, panel=TRUE, correlation=TRUE)
t4 <- Sys.time()
summary(mnl.ml.cov)
save(mnl.ml.cov, file="mnl.ml.cov.RData")

#

look at what hod of HC?

nd <- data.frame(hod=c(0:23),
                 thisH2_EO=0, thisH2_ES=0, thisH2_HC=0, thisH2_PB=0, thisH2_SH=0, thisH2_SR=0,
                 thisHC_EO=0, thisHC_ES=0, thisHC_HC=0, thisHC_PB=0, thisHC_SH=0, thisHC_SR=0,
                 thisEO=0, thisES=0, thisPB=0, thisSH=0, thisSR=0,
                 actDur15=mean(mydata.15dt[which(mydata.15dt$thisH1==1),]$actDur15))
nd$hSIN1 <- sin((2*pi/24)*nd$hod)
nd$hCOS1 <- cos((2*pi/24)*nd$hod)
pp.nd <- cbind(nd, predict(mnl.sl, newdata=nd, type="probs", se=TRUE))




### The End


betahat <- mnl.ml.cov$coefficients
L1 = betahat['EO:(intercept)']+betahat['thisH2_EO']
L2 = betahat['ES:(intercept)']+betahat['thisH2_ES']
L3 = betahat['HC:(intercept)']+betahat['thisH2_HC']
L4 = betahat['PB:(intercept)']+betahat['thisH2_PB']
L5 = betahat['SH:(intercept)']+betahat['thisH2_SH']
L6 = betahat['SR:(intercept)']+betahat['thisH2_SR']

mnl.2l.15dt <- mlogit(chosen~
						thisH2_EO+
						thisH2_ES+
						thisH2_HC+
						thisH2_OM+
						thisH2_RE+
						thisH2_SH+
						thisH2_SO+
						thisRE_EO+
						thisRE_HC+
						thisRE_OM+
						thisRE_SH+
						thisRE_SO+cumDay_H3|
  						thisEO+thisES+thisOM+thisSH+
  						log(actDur15)+
                    	hSIN1+hCOS1|0,
                	data=mlogitData.15dt, reflevel="ST",
                    rpar=c("H2:(intercept)"="n",
                    		"H3:(intercept)"="n",
                        	"ES:(intercept)"="n",
                        	"EO:(intercept)"="n",
                            "HC:(intercept)"="n",
                            "OM:(intercept)"="n",
                            "RE:(intercept)"="n",
                            "SH:(intercept)"="n",
                            "SO:(intercept)"="n"),
                    R=500, halton=NA, print.level=0, panel=TRUE)
summary(mnl.2l.15dt)
save(mnl.2l.15dt, file="mnl.2l.15dt.RData")

mnl.2l.cov <- mlogit(chosen~
						thisH2_EO+
						thisH2_ES+
						thisH2_HC+
						thisH2_OM+
						thisH2_RE+
						thisH2_SH+
						thisH2_SO+
						thisRE_EO+
						thisRE_HC+
						thisRE_OM+
						thisRE_SH+
						thisRE_SO+cumDay_H3|
  						thisEO+thisES+thisOM+thisSH+
  						log(actDur15)+
                    	hSIN1+hCOS1|0,
                	data=mlogitData.15dt, reflevel="ST",
                    rpar=c("H2:(intercept)"="n",
                    		"H3:(intercept)"="n",
                        	"ES:(intercept)"="n",
                        	"EO:(intercept)"="n",
                            "HC:(intercept)"="n",
                            "OM:(intercept)"="n",
                            "RE:(intercept)"="n",
                            "SH:(intercept)"="n",
                            "SO:(intercept)"="n"),
                    R=5000, halton=NA, print.level=0, panel=TRUE, correlation=TRUE)
summary(mnl.2l.cov)
save(mnl.2l.cov, file="mnl.2l.cov.RData")



> table(event.g1$thisActG,event.g1$nextActG)
    
      EO  ES  H2  H3  HC  OM  RE  SH  SO
  EO   5   8  28  78   2  22   7  38   7
  ES   6   9  32  28   1   9   4  11   7
  H1  59  32   0   0  66 142 107 116  66
  H2  35  34   0   0  11  60  34  64  34
  HC   9   2  13  30   3   9   0  33   6
  OM  33   7  53 112   7  64  13  70  13
  RE  10   2  41  69   6  15   1  32   8
  SH  23   8  83 199   6  40  13 100  19
  SO  15   5  22  72   3  11   5  27   6


mlogitData.15dt$tour1 <- with(mlogitData.15dt, ifelse(tourNo==1, 1, 0))
mlogitData.15dt$tour1_H2 <- with(mlogitData.15dt, ifelse(alt=="H2", tour1, 0)) 

mlogitData.15dt$hSIN2_H3 <- with(mlogitData.15dt, ifelse(alt=="H3", hSIN2, 0)) 
mlogitData.15dt$hCOS2_H3 <- with(mlogitData.15dt, ifelse(alt=="H3", hCOS2, 0)) 

mnl.gen <- mlogit(chosen~thisEO+thisES++thisOM+thisRE+thisSH+thisSO|
						log(actDur15)+hSIN1+hCOS1|0,
						data=mlogitData.15dt, reflevel="ST")
summary(mnl.gen)
thisH1+thisH2+thisHC

mnl.15dt <- mlogit(chosen~
					thisH2_EO+
					thisH2_ES+
					thisH2_HC+
					thisH2_OM+
					thisH2_RE+
					thisH2_SH+
					thisH2_SO+
					thisRE_EO+
					thisRE_HC+
					thisRE_OM+
					thisRE_SH+
					thisRE_SO+cumDay_H3|
					thisEO+thisES+thisOM+thisSH+
					log(actDur15)+
					hSIN1+hCOS1|0,
  					data=mlogitData.15dt, reflevel="ST")
summary(mnl.15dt)
save(mnl4.15dt, file="mnl4.15dt.RData")


mnl.sl2 <- mlogit(chosen~
					thisH2_EO+thisH2_ES+thisH2_HC+thisH2_OM+thisH2_RE+thisH2_SH+thisH2_SO|
					thisEO+thisES+thisOM+thisSH+thisSO+
					log(actDur15)+
					hSIN1+hCOS1|0,
  					data=mlogitData.15dt, reflevel="ST")
summary(mnl.sl2)

mnl.sl3 <- mlogit(chosen~
					thisH2_EO+thisH2_ES+thisH2_HC+thisH2_OM+thisH2_RE+thisH2_SH+thisH2_SO|
					thisEO+thisES+thisHC+thisOM+thisSH+thisSO+
					log(actDur15)+
					hSIN1+hCOS1|0,
  					data=mlogitData.15dt, reflevel="ST")
summary(mnl.sl3)

mnl.sl4 <- mlogit(chosen~
					thisH2_EO+thisH2_ES+thisH2_HC+thisH2_OM+thisH2_RE+thisH2_SH+thisH2_SO+
					thisRE_EO+thisRE_ES+thisRE_HC+thisRE_OM+thisRE_RE+thisRE_SH+thisRE_SO|
					thisEO+thisES+thisHC+thisOM+thisSH+thisSO+
					log(actDur15)+
					hSIN1+hCOS1|0,
  					data=mlogitData.15dt, reflevel="ST")
summary(mnl.sl4)




mlogitData.15dt$hSIN2_H3 <- with(mlogitData.15dt, ifelse(alt=="H3", hSIN2, 0))
mlogitData.15dt$hCOS2_H3 <- with(mlogitData.15dt, ifelse(alt=="H3", hCOS2, 0))
mlogitData.15dt$hSIN2_EO <- with(mlogitData.15dt, ifelse(alt=="EO", hSIN2, 0))
mlogitData.15dt$hCOS2_EO <- with(mlogitData.15dt, ifelse(alt=="EO", hCOS2, 0))


mnl.test <- mlogit(chosen~hSIN2_H3+hCOS2_H3+hSIN2_EO+hCOS2_EO|
					log(actDur15)+hSIN1+hCOS1|0,
					data=mlogitData.15dt,reflevel="ST")
summary(mnl.test)

						thisRE_EO+thisRE_ES+thisRE_HC+thisRE_
						thisRE_OM+
						thisRE_SH+
						thisRE_SO+cumDay_H3|


save(mnl4.15dt, file="mnl4.15dt.RData")

					thisEO+thisES+thisOM+thisRE+thisSH+thisSO+


					thisEO+thisES+thisOM+thisRE+thisSH+thisSO+




