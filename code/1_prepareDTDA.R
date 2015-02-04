rm(list=ls())

setwd("/Users/Potenza/Documents/01_dissertation/e2_dtda") # on my macbook air
setwd("/home/kihong/dissertation/e2_dtda") # on the sapporo server
options(scipen=999) # to remove any scientific notations

library(survival)
library(mlogit)
library(lme4)
library(sqldf)
library(reshape)

# import data from 2011 OTAS
activity <- read.csv("../data/2011OTAS/ACTIVITY.csv", header=T)
# linkedtrip <- read.csv("../data/2011OTAS/LINKEDTRIP.csv", header=T)
person <- read.csv("../data/2011OTAS/PER.csv", header=T)
household <- read.csv("../data/2011OTAS/HH.csv", header=T)

# sort activity by SAMPN, PERNO, and PLANO
activity <- with(activity, activity[order(SAMPN,PERNO,PLANO),])
# linkedtrip <- with(linkedtrip, linkedtrip[order(SAMPN,PERNO,PLANO),])

# select households only in Metro (AREA=11; 4799 households), not RTC (AREA=21; 1650 households)
event <- subset(activity, SAMPN>8000000)
# event <- subset(linkedtrip, SAMPN>8000000)

# merge event with household and person
event <- merge(event, household, by="SAMPN", all.x=T)
event <- merge(event, person, by=c("SAMPN","PERNO"), all.x=T)

# add 'perid' (the maximum PERNO is 8)
event$perid <- with(event, SAMPN*10+PERNO) 

# add 'tourNo' and 'maxTour'
event$tourNo <- event$BEGTOURNUM
event$maxTour <- with(event, ave(tourNo, SAMPN,PERNO, FUN=max))

# add 'actNo' and 'maxAct'
event$actNo <- with(event, ave(PLANO, SAMPN,PERNO, FUN=seq))
event$maxAct <- with(event, ave(actNo, SAMPN,PERNO, FUN=length))
# tip: 'ave' is to do group averages over the combined levels of factors

# add 'arrive' and 'depart' on the 1440-minute time scale
event$arrive <- with(event, ARR_HR*60+ARR_MIN)
event$depart <- with(event, DEP_HR*60+DEP_MIN)
# correct 'arrive' and 'depart' after midnight
event$arrive <- with(event, ifelse(arrive<180, arrive+1440, arrive))
event$depart <- with(event, ifelse(depart<180, depart+1440, depart))

### define the sample!!!
# 1. delete persons who have no trip during the day
NoTrip <- sqldf("select perid from event where actNo==1 and maxAct==1")
event <- subset(event, !(event$perid %in% NoTrip$perid))

# 2. delete persons whose first activity is not 'Home'
NotHomeAtFirst <- sqldf("select perid from event where actNo=1 and TPURP!=1 and TPURP!=2")
event <- subset(event, !(event$perid %in% NotHomeAtFirst$perid))

# 3. delete persons whose last activity is not 'Home'
NotHomeAtLast <- sqldf("select perid from event where actNo=maxAct and TPURP!=1 and TPURP!=2")
event <- subset(event, !(event$perid %in% NotHomeAtLast$perid))

# 4. delete persons who were not home before 5AM or 300 mins
# NotHomeAt300 <- sqldf("select perid from event where actNo=1 and depart < 300")
# event <- subset(event, !(event$perid %in% NotHomeAt300$perid))

# 5. delete persons who were not home after midnight or 1440 mins 
# NotHomeAt1440 <- sqldf("select perid from event where actNo=maxAct and arrive > 1440")
# event <- subset(event, !(event$perid %in% NotHomeAt1440$perid))

### delete CM activities to use only linked trips
event <- subset(event, TPURP!=7)

# update actNo and maxAct
event$actNo <- with(event, ave(PLANO, SAMPN,PERNO, FUN=seq))
event$maxAct <- with(event, ave(actNo, SAMPN,PERNO, FUN=length))

# aggregate activity types (thisActG)
aggregateActivities = read.csv("../data/aggregateActivities4.csv", stringsAsFactors=FALSE)
event$thisActG = aggregateActivities$ActG[match(event$TPURP, aggregateActivities$Code)]
event$thisActG_Name = aggregateActivities$ActG_Name[match(event$TPURP, aggregateActivities$Code)]

# disaggregate home activities into H1, H2, and H3
attach(event)
event$thisActG[thisActG=="HM" & actNo==1] <- "H1"
event$thisActG[thisActG=="HM" & actNo!=1 & actNo!=maxAct] <- "H2"
event$thisActG[thisActG=="HM" & actNo!=1 & actNo==maxAct] <- "H3"
event$thisActG_Name[thisActG=="H1"] <- "HomeBegining"
event$thisActG_Name[thisActG=="H2"] <- "HomeMiddle"
event$thisActG_Name[thisActG=="H3"] <- "HomeEnd"
detach(event)

# disaggregate home activities into H1 and H2
#attach(event)
#event$thisActG[thisActG=="HM" & actNo==1] <- "H1"
#event$thisActG[thisActG=="HM" & actNo!=1] <- "H2"
#event$thisActG_Name[thisActG=="H1"] <- "HomeBegining"
#event$thisActG_Name[thisActG=="H2"] <- "HomeReturning"
#detach(event)

# lastActG
event$lastActG <- c(NA, event$thisActG[-length(event$thisActG)])
event$lastActG[event$actNo==1] <- NA
# tip: '-length()' is all elements except for the last observation

# nextActG
event$nextActG <- c(event$thisActG[-1], NA)
event$nextActG[event$actNo==event$maxAct] <- NA

# delete 3 person involving a transition from H1 to H2
sqldf("select perid from event where lastActG=='H1' and thisActG=='H2'")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80043571")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80856423")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==81325631")
H1toH2 <- sqldf("select perid from event where lastActG=='H1' and thisActG=='H2'")
event <- subset(event, !(event$perid %in% H1toH2$perid))
# event <- event[-which(event$lastActG=='H1' & event$thisActG=='H2'),]

# delete 14 persons involving a transition from H2 to H3
sqldf("select perid from event where lastActG=='H2' and thisActG=='H3'")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80252491")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80333872")
sqldf("select perid,PLANO,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80954302")
H2toH3 <- sqldf("select perid from event where lastActG=='H2' and thisActG=='H3'")
event <- subset(event, !(event$perid %in% H2toH3$perid))
# event <- event[-which(event$thisActG=='H2' & event$nextActG=='H3'),]

# delete 5 persons involving a transition from H1 to H3
sqldf("select perid from event where lastActG=='H1' and thisActG=='H3'")
sqldf("select perid,PLANO,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==80824324")
sqldf("select perid,PLANO,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==81181904")
sqldf("select perid,PLANO,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,CMMODENAMES from event where perid==81204141")
H1toH3 <- sqldf("select perid from event where lastActG=='H1' and thisActG=='H3'")
event <- subset(event, !(event$perid %in% H1toH3$perid))

# delete 8 persons involving a transition from H1 to H2
#sqldf("select perid from event where lastActG=='H1' and thisActG=='H2'")
#H1toH2 <- sqldf("select perid from event where lastActG=='H1' and thisActG=='H2'")
#event <- subset(event, !(event$perid %in% H1toH2$perid))

# delete 18 persons involcing a transition from H2 to H2
#sqldf("select perid from event where lastActG=='H2' and thisActG=='H2'")
#H2toH2 <- sqldf("select perid from event where lastActG=='H2' and thisActG=='H2'")
#event <- subset(event, !(event$perid %in% H2toH2$perid))

# add lastDepart
event$lastDepart <- c(NA, event$depart[-length(event$depart)])
event$lastDepart[event$actNo==1] <- NA

# add tripDur
event$tripDur <- with(event, arrive-lastDepart)

# add actDur
event <- rename(event, c(ACTDUR="ACTDUR_OLD"))
event$actDur <- with(event, depart-arrive)

# compute activity duration and trip duration by thisActG
sqldf("select thisActG,count(*),round(avg(actDur),1),round(stdev(actDur),1),median(actDur),min(actDur),max(actDur) from event group by thisActG")
sqldf("select thisActG,count(*),round(avg(tripDur),1),round(stdev(tripDur),1),median(tripDur),min(tripDur),max(tripDur) from event group by thisActG")

# add 'actDur15' (15-min time intervals of activity duration)
event$actDur15 <- with(event, ifelse(actDur%%15==0, actDur/15, actDur%/%15+1))  # (,]

# add 'actDur10' (10-min time intervals of activity duration)
event$actDur10 <- with(event, ifelse(actDur%%10==0, actDur/10, actDur%/%10+1))  # (,]

# add 'actDur5' (5-min time intervals of activity duration)
event$actDur5 <- with(event, ifelse(actDur%%5==0, actDur/5, actDur%/%5+1))  # (,]

# add 'dayDepart'
event$dayDepart <- with(event, ave(depart, perid, FUN=min))

# add 'cumTour' (cummulative time since first departure of the tour)
# event$cumTour <- with(event, arrive - (TOURDEP_HR*60+TOURDEP_MIN))
# event$cumTour[event$actNo==1] <-NA

# checking...
sqldf("select perid,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,arrive,depart,actDur,
      actDur15,actDur10,actDur5,dayDepart,TOURDEP_HR,TOURDEP_MIN
      from event where perid=80000072")

# time-of-day indicators to tell when the this activity is over and the next activity is chosen
event$T0304 <- with(event, ifelse(DEP_HR==3, 1, 0))
event$T0405 <- with(event, ifelse(DEP_HR==4, 1, 0))
event$T0506 <- with(event, ifelse(DEP_HR==5, 1, 0))
event$T0607 <- with(event, ifelse(DEP_HR==6, 1, 0))
event$T0708 <- with(event, ifelse(DEP_HR==7, 1, 0))
event$T0809 <- with(event, ifelse(DEP_HR==8, 1, 0))
event$T0910 <- with(event, ifelse(DEP_HR==9, 1, 0))
event$T1011 <- with(event, ifelse(DEP_HR==10, 1, 0))
event$T1112 <- with(event, ifelse(DEP_HR==11, 1, 0))
event$T1213 <- with(event, ifelse(DEP_HR==12, 1, 0))
event$T1314 <- with(event, ifelse(DEP_HR==13, 1, 0))
event$T1415 <- with(event, ifelse(DEP_HR==14, 1, 0))
event$T1516 <- with(event, ifelse(DEP_HR==15, 1, 0))
event$T1617 <- with(event, ifelse(DEP_HR==16, 1, 0))
event$T1718 <- with(event, ifelse(DEP_HR==17, 1, 0))
event$T1819 <- with(event, ifelse(DEP_HR==18, 1, 0))
event$T1920 <- with(event, ifelse(DEP_HR==19, 1, 0))
event$T2021 <- with(event, ifelse(DEP_HR==20, 1, 0))
event$T2122 <- with(event, ifelse(DEP_HR==21, 1, 0))
event$T2223 <- with(event, ifelse(DEP_HR==22, 1, 0))
event$T2324 <- with(event, ifelse(DEP_HR==23, 1, 0))
event$T2401 <- with(event, ifelse(DEP_HR==24, 1, 0))
event$T0102 <- with(event, ifelse(DEP_HR==1, 1, 0))
event$T0203 <- with(event, ifelse(DEP_HR==2, 1, 0))

# periodic or circular time-of-day effects
# hour scale
event$DEP_HR2 <- with(event, ifelse(DEP_HR<24, DEP_HR, DEP_HR-24))
event$hSIN1 <- with(event, sin((2*pi/24)*DEP_HR2))
event$hCOS1 <- with(event, cos((2*pi/24)*DEP_HR2))
event$hSIN2 <- with(event, sin((2*pi/24)*DEP_HR2*2))
event$hCOS2 <- with(event, cos((2*pi/24)*DEP_HR2*2))
event$hSIN3 <- with(event, sin((2*pi/24)*DEP_HR2*3))
event$hCOS3 <- with(event, cos((2*pi/24)*DEP_HR2*3))
event$hSIN4 <- with(event, sin((2*pi/24)*DEP_HR2*4))
event$hCOS4 <- with(event, cos((2*pi/24)*DEP_HR2*4))
# min scale
event$depart2 <- with(event, ifelse(depart<1440, depart, depart-1440))
event$mSIN1 <- with(event, sin((2*pi/1440)*depart2))
event$mCOS1 <- with(event, cos((2*pi/1440)*depart2))
event$mSIN2 <- with(event, sin((2*pi/1440)*depart2*2))
event$mCOS2 <- with(event, cos((2*pi/1440)*depart2*2))
event$mSIN3 <- with(event, sin((2*pi/1440)*depart2*3))
event$mCOS3 <- with(event, cos((2*pi/1440)*depart2*3))
event$mSIN4 <- with(event, sin((2*pi/1440)*depart2*4))
event$mCOS4 <- with(event, cos((2*pi/1440)*depart2*4))

# current activity indicators to tell what the current activity is
event$thisH1 <- with(event, ifelse(thisActG=="H1", 1, 0))
event$thisH2 <- with(event, ifelse(thisActG=="H2", 1, 0))
event$thisH3 <- with(event, ifelse(thisActG=="H3", 1, 0))
event$thisWK <- with(event, ifelse(thisActG=="WK", 1, 0))
event$thisWR <- with(event, ifelse(thisActG=="WR", 1, 0))
event$thisSC <- with(event, ifelse(thisActG=="SC", 1, 0))
event$thisES <- with(event, ifelse(thisActG=="ES", 1, 0))
event$thisEO <- with(event, ifelse(thisActG=="EO", 1, 0))
event$thisPB <- with(event, ifelse(thisActG=="PB", 1, 0))
event$thisHC <- with(event, ifelse(thisActG=="HC", 1, 0))
event$thisSH <- with(event, ifelse(thisActG=="SH", 1, 0))
event$thisSR <- with(event, ifelse(thisActG=="SR", 1, 0))
event$thisOT <- with(event, ifelse(thisActG=="OT", 1, 0))

# next activity indicators to tell what the next activity is
event$nextH1 <- with(event, ifelse(nextActG=="H1", 1, 0))
event$nextH2 <- with(event, ifelse(nextActG=="H2", 1, 0))
event$nextH3 <- with(event, ifelse(nextActG=="H3", 1, 0))
event$nextWK <- with(event, ifelse(nextActG=="WK", 1, 0))
event$nextWR <- with(event, ifelse(nextActG=="WR", 1, 0))
event$nextSC <- with(event, ifelse(nextActG=="SC", 1, 0))
event$nextES <- with(event, ifelse(nextActG=="ES", 1, 0))
event$nextEO <- with(event, ifelse(nextActG=="EO", 1, 0))
event$nextPB <- with(event, ifelse(nextActG=="PB", 1, 0))
event$nextHC <- with(event, ifelse(nextActG=="HC", 1, 0))
event$nextSH <- with(event, ifelse(nextActG=="SH", 1, 0))
event$nextSR <- with(event, ifelse(nextActG=="SR", 1, 0))
event$nextOT <- with(event, ifelse(nextActG=="OT", 1, 0))

### delete 'H3' since it has no choice for the next activity
sqldf("select thisActG,count(*) from event where actNo=maxAct group by thisActG")
event <- event[-which(event$thisActG=="H3"),] 
#event <- event[-which(event$actNo==event$maxAct),] 

### add an indicator of censored observations
#sqldf("select perid from event where actNo=maxAct limit 5")
#sqldf("select perid,actNo,maxAct,thisActG,nextActG,arrive,depart from event where perid=80006473")
#event$censor <- with(event, ifelse(actNo==maxAct, 1, 0))
#event$nextActG[event$actNo==event$maxAct] <- 0

# checking...
sqldf("select perid,tourNo,maxTour,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,actDur,
      actDur15,actDur10,actDur5,dayDepart
      from event where perid=80000072")
#
save(event, file="../e2_dtda/outputs/event.RData")

####################################
### a data set for each group
####################################
# group 1 - retired, age>=65, and no school
event.g1 <- subset(event, WKSTAT==1 & AGE>=65 & AGE<95 & STUDE==3)
# delete 2 persons with thisActG=OT
peridWithOT <- sqldf("select perid from 'event.g1' where thisActG='OT' ")
event.g1 <- subset(event.g1, !(event.g1$perid %in% peridWithOT$perid))

save(event.g1, file="../e2_dtda/outputs/event.g1.RData")

# group 2 - workers
event.g2 <- subset(event, )
# compute activity duration and trip duration by thisActG
sqldf("select thisActG,count(*),round(avg(actDur),1),round(stdev(actDur),1),median(actDur),min(actDur),max(actDur) from 'event.g1' group by thisActG")
sqldf("select nextActG,count(*),round(avg(actDur),1),round(stdev(actDur),1),median(actDur),min(actDur),max(actDur) from 'event.g1' group by nextActG")
sqldf("select thisActG,count(*),round(avg(tripDur),1),round(stdev(tripDur),1),median(tripDur),min(tripDur),max(tripDur) from 'event.g1' group by thisActG")
sqldf("select perid,PNAME,tourNo,maxTour,actNo,maxAct,thisActG,nextActG,arrive,depart,actDur,tripDur from 'event.g1' limit 5")
sqldf("select perid,AGE,PNAME,tourNo,maxTour,actNo,maxAct,thisActG,nextActG,arrive,depart,actDur,tripDur from 'event.g1' where perid=80000402")

#tour <- subset(event.g1, thisActG=="H2")
#tour$actNo <- tour$actNo+0.5
#tour$tourNo <- tour$tourNo+1
#sqldf("select perid,AGE,PNAME,tourNo,maxTour,actNo,maxAct,thisActG,nextActG,arrive,depart,actDur,tripDur from 'tour' where perid=80000402")
#event.g1.tour <- rbind(event.g1,tour)
#event.g1.tour <- with(event.g1.tour, event.g1.tour[order(perid,actNo),])
#sqldf("select perid,AGE,PNAME,tourNo,maxTour,actNo,maxAct,thisActG,nextActG,arrive,depart,actDur,tripDur from 'event.g1.tour' where perid=80000402")
#update actNo and maxAct within a tour
#event.g1.tour$actNo <- with(event.g1.tour, ave(actNo, perid,tourNo, FUN=seq))
#event.g1.tour$maxAct <- with(event.g1.tour, ave(actNo, perid,tourNo, FUN=length))


#sqldf("select thisActG,count(*) from 'event.g1' where actNo=maxAct group by thisActG")
#sqldf("select perid from 'event.g1' where actNo=maxAct")
#sqldf("select perid,actNo,maxAct,TPURP,thisActG,nextActG,arrive,depart,actDur,
#      actDur15,actDur10,actDur5,dayDepart,TOURDEP_HR,TOURDEP_MIN
#      from 'event.g1' where perid==80145981")

##########################################################
# convert from "person-episode" to "person-episode-period"
##########################################################
# using 5-min interavals
event.g1.5dt <- survSplit(event.g1, cut=(1:max(event.g1$actDur5)), end="actDur5", event="nextActG", start="start")
event.g1.5dt <- with(event.g1.5dt, event.g1.5dt[order(SAMPN, PERNO, actNo, start),])
# add time dummies
for (i in 1:max(event.g1.5dt$actDur5)) {
  event.g1.5dt[[paste("t",i,sep="")]] <- ifelse(event.g1.5dt$actDur5==i, 1, 0)
}
# add exposure
event.g1.5dt$exposure <- with(event.g1.5dt, ifelse(actDur5*5 <= actDur, 5, actDur%%5))
# testing
sqldf("select perid,actNo,maxAct,tripDur,thisActG,nextActG,actDur,actDur5,exposure,arrive,depart from 'event.g1.5dt' limit 126")
table(event.g1.5dt$nextActG, useNA="ifany")
#
save(event.g1.5dt, file="../e2_dtda/outputs/event.g1.5dt.RData")

# using 10-min interavals
event.g1.10dt <- survSplit(event.g1, cut=(1:max(event.g1$actDur10)), end="actDur10", event="nextActG", start="start")
event.g1.10dt <- with(event.g1.10dt, event.g1.10dt[order(SAMPN, PERNO, actNo, start),])
# add time dummies
#library(caret)
#timeDummies <- dummyVars(~as.factor(actDur10), data=event.g1.10dt, levelsOnly=TRUE)
#test <- predict(timeDummies, event.g1.10dt)
for (i in 1:max(event.g1.10dt$actDur10)) {
  event.g1.10dt[[paste("t",i,sep="")]] <- ifelse(event.g1.10dt$actDur10==i, 1, 0)
}
# add exposure
event.g1.10dt$exposure <- with(event.g1.10dt, ifelse(actDur10*10 <= actDur, 10, actDur%%10))
# testing
sqldf("select perid,actNo,maxAct,thisActG,nextActG,actDur,actDur10,exposure,arrive,depart from 'event.g1.10dt' limit 100")
table(event.g1.10dt$nextActG, useNA="ifany")
#
save(event.g1.10dt, file="../e2_dtda/outputs/event.g1.10dt.RData")

### using 15-min interavals
event.g1.15dt <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextActG", start="start")
event.g1.15dt <- with(event.g1.15dt, event.g1.15dt[order(SAMPN, PERNO, actNo, start),])
# add time dummies
for (i in 1:max(event.g1.15dt$actDur15)) {
  event.g1.15dt[[paste("t",i,sep="")]] <- ifelse(event.g1.15dt$actDur15==i, 1, 0)
}
# add exposure
event.g1.15dt$exposure <- with(event.g1.15dt, ifelse(actDur15*15 <= actDur, 15, actDur%%15))
# viewing
sqldf("select perid,tourNo,maxTour,actNo,maxAct,thisActG,nextActG,actDur,actDur15,exposure,arrive,depart,t1,t2,t10,t11,t68,t69 from 'event.g1.15dt' limit 100")
table(event.g1.15dt$nextActG, useNA="ifany")
#
save(event.g1.15dt, file="../e2_dtda/outputs/event.g1.15dt.RData")


#######################################################
# 1. Multinomial Approach to Modeling Competing Risks
#######################################################
#
createLong <- function(x,t) {
  x$nextActG[x$nextActG==0] <- "ST"
  x$nextActG <- as.factor(x$nextActG)
  choiceSet <- levels(x$nextActG)
  x$aeID <- x$perid*100+x$actNo
  x$dtID <- x$aeID*1000+x[[paste0("actDur",t)]]
  alts <- list()
  for (i in choiceSet) {
    x$alt <- i
    x$chosen <- ifelse(x$nextActG==i, 1, 0)
    if (i=="H2") {x$available <- ifelse(x$thisActG=="H1"|x$thisActG=="H2", 0, 1)}
    else {if (i=="H3") {x$available <- ifelse(x$thisActG=="H1"|x$thisActG=="H2", 0, 1)}
          else {x$available <- 1}}
    alts[[i]] <- x
  }
  long <- do.call("rbind", alts)
  long <- with(long, long[order(dtID,alt),])
  long <- subset(long, available==1)
  long$numAlt <- with(long, ave(alt, dtID, FUN=length))
  return(long)
}

#
event.g1.5dt.long <- createLong(event.g1.5dt,5)
event.g1.10dt.long <- createLong(event.g1.10dt,10)
event.g1.15dt.long <- createLong(event.g1.15dt,15)
save(event.g1.5dt.long, file="../e2_dtda/outputs/event.g1.5dt.long.RData")
save(event.g1.10dt.long, file="../e2_dtda/outputs/event.g1.10dt.long.RData")
save(event.g1.15dt.long, file="../e2_dtda/outputs/event.g1.15dt.long.RData")

#
H1 <- subset(event.g1.15dt.long, thisActG=="H1")
mdata.H1 <- mlogit.data(H1, shape="long", choice="chosen", chid.var="dtID", alt.var="alt", id="perid")
mnl.H1 <- mlogit(chosen ~ 0|log(actDur15)+hSIN1+hCOS1|0,
					data=mdata.H1,ref="ST")
summary(mnl.H1)
#
nonH1 <- subset(event.g1.15dt.long, thisActG!="H1")
sqldf("select nextActG,count(*),round(avg(actDur),1),round(stdev(actDur),1),median(actDur),min(actDur),max(actDur) from 'event.g1' group by nextActG")

mdata.nonH1 <- mlogit.data(nonH1, shape="long", choice="chosen", chid.var="dtID", alt.var="alt", id="perid")
mnl.nonH1 <- mlogit(chosen ~ 0|log(actDur15)
								+hSIN1+hCOS1
								+thisES+thisEO+thisOM+thisSH|0,
					data=mdata.nonH1,ref="ST")
summary(mnl.nonH1)


#
mydata.5dt <- mlogit.data(event.g1.5dt.long,shape="long",choice="chosen",chid.var="dtID",alt.var="alt",id="perid")
mydata.10dt <- mlogit.data(event.g1.10dt.long,shape="long",choice="chosen",chid.var="dtID",alt.var="alt",id="perid")
mydata.15dt <- mlogit.data(event.g1.15dt.long,shape="long",choice="chosen",chid.var="dtID",alt.var="alt",id="perid")
head(index(mydata.5dt),10)
head(index(mydata.10dt),10)
head(index(mydata.15dt),10)
save(mydata.5dt, file="../e2_dtda/outputs/mydata.5dt.RData")
save(mydata.10dt, file="../e2_dtda/outputs/mydata.10dt.RData")
save(mydata.15dt, file="../e2_dtda/outputs/mydata.15dt.RData")

#
createBGdat <- function(x) {
  myvars <- c("perid","actNo","thisActG","actDur15","nextActG",
              "DEP_HR2","arrive","dayDepart","TOURDEP_HR","TOURDEP_MIN",
              "thisH1","thisH2","thisEO","thisES","thisHC","thisPB","thisSH","thisSR",
              "hSIN1","hCOS1","hSIN2","hCOS2","hSIN3","hCOS3","hSIN4","hCOS4")
  x <- x[myvars]
  x$thisActG <- factor(x$thisActG, levels=c("H1","H2","EO","ES","HC","PB","SH","SR"))
  x$thisActG <- as.numeric(x$thisActG)
  x$nextActG[x$nextActG==0] <- "ST"
  x$nextActG <- factor(x$nextActG, levels=c("ST","H2","H3","EO","ES","HC","PB","SH","SR"))
  x$nextActG <- as.numeric(x$nextActG)
  x$cumDay <- with(x, ifelse(thisActG==1|thisActG==2, 0, ((arrive-dayDepart)+(actDur15*15))))
  x$cumTour <- with(x, ifelse(thisActG==1|thisActG==2, 0, ((arrive-(TOURDEP_HR*60+TOURDEP_MIN))+(actDur15*15))))
  x$aeID <- x$perid*100+x$actNo
  x$dtID <- x$aeID*1000+x$actDur15
  alts <- lapply(1:10, function(i) i=x)
  alts <- lapply(seq_along(alts),
                 function(i,x) {
                   x[[i]]$alt <- i
                   return (x[[i]])
                 }, alts)
  long <- do.call("rbind", alts)
  long <- with(long, long[order(dtID,alt),])
  long$ST_AV <- 1
  long$H2_AV <- with(long, ifelse(thisActG==1|thisActG==2, 0, 1))
  long$H3_AV <- with(long, ifelse(thisActG==1|thisActG==2, 0, 1))
  long$EO_AV <- 1
  long$ES_AV <- 1
  long$HC_AV <- 1
  long$PB_AV <- 1
  long$SH_AV <- 1
  long$SR_AV <- 1
  return(long)
}

#event.g1.5dt.biogeme <- createBiogeme(event.g1.5dt,5)
#event.g1.10dt.biogeme <- createBiogeme(event.g1.10dt,10)
event.g1.15dt.long.BG <- createBGdat(event.g1.15dt)

#save(event.g1.5dt.biogeme, file="event.g1.5dt.biogeme.RData")
#save(event.g1.10dt.biogeme, file="event.g1.10dt.biogeme.RData")
save(event.g1.15dt.long.BG, file="event.g1.15dt.long.BG.RData")

#write.table(event.g1.5dt.biogeme, "mydata5.dat", sep="\t")
#write.table(event.g1.10dt.biogeme, "mydata10.dat", sep="\t")
write.table(event.g1.15dt.long.BG, "myBGdata15.dat", sep="\t", row.names=FALSE, quote=FALSE)

###############################
### MNL model specification ###
###############################

########### 5-min discrete-time
load("mydata.5dt.RData")
## baseline hazard (mnl0)
mnl0.5dt <- mlogit(chosen~0|log(actDur5)|0, data=mydata.5dt, reflevel="ST")
summary(mnl0.5dt)
save(mnl0.5dt, file="mnl0.5dt.RData")
## add circular time-of-day covariates either hour or minute scale (mnl1), rather than adding time-of-day dummies
mnl1.5dt <- mlogit(chosen~0|log(actDur5)+
						hSIN1+hCOS1+hSIN2+hCOS2+hSIN3+hCOS3+hSIN4+hCOS4|0,
					data=mydata.5dt, reflevel="ST")
summary(mnl1.5dt)
save(mnl1.5dt, file="mnl1.5dt.RData")
## add current activity type indicators (mnl2)
mnl2.5dt <- mlogit(chosen~0|log(actDur5)+
						hSIN1+hCOS1+hSIN2+hCOS2+hSIN3+hCOS3+hSIN4+hCOS4+
						thisH1+thisH2+thisES+thisEO+thisPB+thisHC+thisSH+thisSO+thisRE+thisOM|0,
					data=mydata.5dt, reflevel="ST")
summary(mnl2.5dt)
save(mnl2.5dt, file="mnl2.5dt.RData")


########### 10-min discrete-time
load("mydata.10dt.RData")
## baseline hazard (mnl0)
mnl0.10dt <- mlogit(chosen~0|log(actDur10)|0, data=mydata.10dt, reflevel="ST")
summary(mnl0.10dt)
save(mnl0.10dt, file="mnl0.10dt.RData")
## add circular time-of-day covariates either hour or minute scale (mnl1), rather than adding time-of-day dummies
mnl1.10dt <- mlogit(chosen~0|log(actDur10)+
						hSIN1+hCOS1+hSIN2+hCOS2+hSIN3+hCOS3+hSIN4+hCOS4|0,
					data=mydata.10dt, reflevel="ST")
summary(mnl1.10dt)
save(mnl1.10dt, file="mnl1.10dt.RData")
## add current activity type indicators (mnl2)
mnl2.10dt <- mlogit(chosen~0|log(actDur10)+
						hSIN1+hCOS1+hSIN2+hCOS2+hSIN3+hCOS3+hSIN4+hCOS4+
						thisH1+thisH2+thisES+thisEO+thisPB+thisHC+thisSH+thisSO+thisRE+thisOM|0,
					data=mydata.10dt, reflevel="ST")
summary(mnl2.10dt)
save(mnl2.10dt, file="mnl2.10dt.RData")


########### 15-min discrete-time
load("mydata.15dt.RData")
## baseline hazard (mnl0)
mnl0 <- mlogit(chosen~0|log(actDur15)|0, data=mydata.15dt, reflevel="ST")
summary(mnl0)
save(mnl0, file="mnl0.RData")
## add covariates (mnl1)
# cumDay and CumTour
mydata.15dt$cumDay <- with(mydata.15dt, ifelse(thisActG=='H1'|thisActG=='H2', 0, ((arrive-dayDepart)+(actDur15*15))))
mydata.15dt$cumTour <- with(mydata.15dt, ifelse(thisActG=='H1'|thisActG=='H2', 0, ((arrive-(TOURDEP_HR*60+TOURDEP_MIN))+(actDur15*15))))
# episode-specific variables with an alternative-specific coefficient
# H2: thisES_H2+thisHC_H2+thisOM_H2+thisRE_H2+thisSH_H2+thisSO_H2
mydata.15dt$thisES_H2 <- with(mydata.15dt, ifelse(alt=="H2", thisES, 0))
mydata.15dt$thisEO_H2 <- with(mydata.15dt, ifelse(alt=="H2", thisEO, 0))
mydata.15dt$thisHC_H2 <- with(mydata.15dt, ifelse(alt=="H2", thisHC, 0))
mydata.15dt$thisOM_H2 <- with(mydata.15dt, ifelse(alt=="H2", thisOM, 0))
mydata.15dt$thisRE_H2 <- with(mydata.15dt, ifelse(alt=="H2", thisRE, 0))
mydata.15dt$thisSH_H2 <- with(mydata.15dt, ifelse(alt=="H2", thisSH, 0))
mydata.15dt$thisSO_H2 <- with(mydata.15dt, ifelse(alt=="H2", thisSO, 0))
# H3: thisES_H3+thisEO_H3+thisHC_H3+thisOM_H3+thisRE_H3+thisSH_H3+thisSO_H3
mydata.15dt$thisES_H3 <- with(mydata.15dt, ifelse(alt=="H3", thisES, 0))
mydata.15dt$thisEO_H3 <- with(mydata.15dt, ifelse(alt=="H3", thisEO, 0))
mydata.15dt$thisHC_H3 <- with(mydata.15dt, ifelse(alt=="H3", thisHC, 0))
mydata.15dt$thisOM_H3 <- with(mydata.15dt, ifelse(alt=="H3", thisOM, 0))
mydata.15dt$thisRE_H3 <- with(mydata.15dt, ifelse(alt=="H3", thisRE, 0))
mydata.15dt$thisSH_H3 <- with(mydata.15dt, ifelse(alt=="H3", thisSH, 0))
mydata.15dt$thisSO_H3 <- with(mydata.15dt, ifelse(alt=="H3", thisSO, 0))
# ES: thisH2_ES+thisES_ES+thisEO_ES+thisHC_ES+thisOM_ES+thisRE_ES+thisSH_ES+thisSO_ES
mydata.15dt$thisH2_ES <- with(mydata.15dt, ifelse(alt=="ES", thisH2, 0))
mydata.15dt$thisES_ES <- with(mydata.15dt, ifelse(alt=="ES", thisES, 0))
mydata.15dt$thisEO_ES <- with(mydata.15dt, ifelse(alt=="ES", thisEO, 0))
mydata.15dt$thisHC_ES <- with(mydata.15dt, ifelse(alt=="ES", thisHC, 0))
mydata.15dt$thisOM_ES <- with(mydata.15dt, ifelse(alt=="ES", thisOM, 0))
mydata.15dt$thisRE_ES <- with(mydata.15dt, ifelse(alt=="ES", thisRE, 0))
mydata.15dt$thisSH_ES <- with(mydata.15dt, ifelse(alt=="ES", thisSH, 0))
mydata.15dt$thisSO_ES <- with(mydata.15dt, ifelse(alt=="ES", thisSO, 0))
# EO: thisH2_EO+thisES_EO+thisEO_EO+thisHC_EO+thisOM_EO+thisRE_EO+thisSH_EO+thisSO_EO
mydata.15dt$thisH2_EO <- with(mydata.15dt, ifelse(alt=="EO", thisH2, 0))
mydata.15dt$thisES_EO <- with(mydata.15dt, ifelse(alt=="EO", thisES, 0))
mydata.15dt$thisEO_EO <- with(mydata.15dt, ifelse(alt=="EO", thisEO, 0))
mydata.15dt$thisHC_EO <- with(mydata.15dt, ifelse(alt=="EO", thisHC, 0))
mydata.15dt$thisOM_EO <- with(mydata.15dt, ifelse(alt=="EO", thisOM, 0))
mydata.15dt$thisRE_EO <- with(mydata.15dt, ifelse(alt=="EO", thisRE, 0))
mydata.15dt$thisSH_EO <- with(mydata.15dt, ifelse(alt=="EO", thisSH, 0))
mydata.15dt$thisSO_EO <- with(mydata.15dt, ifelse(alt=="EO", thisSO, 0))
# HC: thisH2_HC+thisES_HC+thisEO_HC+thisHC_HC+thisOM_HC+thisRE_HC+thisSH_HC+thisSO_HC
mydata.15dt$thisH2_HC <- with(mydata.15dt, ifelse(alt=="HC", thisH2, 0))
mydata.15dt$thisES_HC <- with(mydata.15dt, ifelse(alt=="HC", thisES, 0))
mydata.15dt$thisEO_HC <- with(mydata.15dt, ifelse(alt=="HC", thisEO, 0))
mydata.15dt$thisHC_HC <- with(mydata.15dt, ifelse(alt=="HC", thisHC, 0))
mydata.15dt$thisOM_HC <- with(mydata.15dt, ifelse(alt=="HC", thisOM, 0))
mydata.15dt$thisRE_HC <- with(mydata.15dt, ifelse(alt=="HC", thisRE, 0))
mydata.15dt$thisSH_HC <- with(mydata.15dt, ifelse(alt=="HC", thisSH, 0))
mydata.15dt$thisSO_HC <- with(mydata.15dt, ifelse(alt=="HC", thisSO, 0))
# OM: thisH2_OM+thisES_OM+thisEO_OM+thisHC_OM+thisOM_OM+thisRE_OM+thisSH_OM+thisSO_OM
mydata.15dt$thisH2_OM <- with(mydata.15dt, ifelse(alt=="OM", thisH2, 0))
mydata.15dt$thisES_OM <- with(mydata.15dt, ifelse(alt=="OM", thisES, 0))
mydata.15dt$thisEO_OM <- with(mydata.15dt, ifelse(alt=="OM", thisEO, 0))
mydata.15dt$thisHC_OM <- with(mydata.15dt, ifelse(alt=="OM", thisHC, 0))
mydata.15dt$thisOM_OM <- with(mydata.15dt, ifelse(alt=="OM", thisOM, 0))
mydata.15dt$thisRE_OM <- with(mydata.15dt, ifelse(alt=="OM", thisRE, 0))
mydata.15dt$thisSH_OM <- with(mydata.15dt, ifelse(alt=="OM", thisSH, 0))
mydata.15dt$thisSO_OM <- with(mydata.15dt, ifelse(alt=="OM", thisSO, 0))
# RE: thisH2_RE+thisES_RE+thisEO_RE+thisHC_RE+thisOM_RE+thisRE_RE+thisSH_RE+thisSO_RE
mydata.15dt$thisH2_RE <- with(mydata.15dt, ifelse(alt=="RE", thisH2, 0))
mydata.15dt$thisES_RE <- with(mydata.15dt, ifelse(alt=="RE", thisES, 0))
mydata.15dt$thisEO_RE <- with(mydata.15dt, ifelse(alt=="RE", thisEO, 0))
mydata.15dt$thisHC_RE <- with(mydata.15dt, ifelse(alt=="RE", thisHC, 0))
mydata.15dt$thisOM_RE <- with(mydata.15dt, ifelse(alt=="RE", thisOM, 0))
mydata.15dt$thisRE_RE <- with(mydata.15dt, ifelse(alt=="RE", thisRE, 0))
mydata.15dt$thisSH_RE <- with(mydata.15dt, ifelse(alt=="RE", thisSH, 0))
mydata.15dt$thisSO_RE <- with(mydata.15dt, ifelse(alt=="RE", thisSO, 0))
# SH: thisH2_SH+thisES_SH+thisEO_SH+thisHC_SH+thisOM_SH+thisRE_SH+thisSH_SH+thisSO_SH
mydata.15dt$thisH2_SH <- with(mydata.15dt, ifelse(alt=="SH", thisH2, 0))
mydata.15dt$thisES_SH <- with(mydata.15dt, ifelse(alt=="SH", thisES, 0))
mydata.15dt$thisEO_SH <- with(mydata.15dt, ifelse(alt=="SH", thisEO, 0))
mydata.15dt$thisHC_SH <- with(mydata.15dt, ifelse(alt=="SH", thisHC, 0))
mydata.15dt$thisOM_SH <- with(mydata.15dt, ifelse(alt=="SH", thisOM, 0))
mydata.15dt$thisRE_SH <- with(mydata.15dt, ifelse(alt=="SH", thisRE, 0))
mydata.15dt$thisSH_SH <- with(mydata.15dt, ifelse(alt=="SH", thisSH, 0))
mydata.15dt$thisSO_SH <- with(mydata.15dt, ifelse(alt=="SH", thisSO, 0))
# SO: thisH2_SO+thisES_SO+thisEO_SO+thisHC_SO+thisOM_SO+thisRE_SO+thisSH_SO+thisSO_SO
mydata.15dt$thisH2_SO <- with(mydata.15dt, ifelse(alt=="SO", thisH2, 0))
mydata.15dt$thisES_SO <- with(mydata.15dt, ifelse(alt=="SO", thisES, 0))
mydata.15dt$thisEO_SO <- with(mydata.15dt, ifelse(alt=="SO", thisEO, 0))
mydata.15dt$thisHC_SO <- with(mydata.15dt, ifelse(alt=="SO", thisHC, 0))
mydata.15dt$thisOM_SO <- with(mydata.15dt, ifelse(alt=="SO", thisOM, 0))
mydata.15dt$thisRE_SO <- with(mydata.15dt, ifelse(alt=="SO", thisRE, 0))
mydata.15dt$thisSH_SO <- with(mydata.15dt, ifelse(alt=="SO", thisSH, 0))
mydata.15dt$thisSO_SO <- with(mydata.15dt, ifelse(alt=="SO", thisSO, 0))
# cumDay & cumTour
mydata.15dt$cumDay_H3 <- with(mydata.15dt, ifelse(alt=="H3", cumDay, 0))
mydata.15dt$cumTour_H2 <- with(mydata.15dt, ifelse(alt=="H2", cumTour, 0))
#
save(mydata.15dt,file="mydata.15dt.RData")
#
mnl2 <- mlogit(chosen~
            	 		   thisES_H2+thisEO_H2+thisHC_H2+thisOM_H2+thisRE_H2+thisSH_H2+
                 		   thisES_H3+thisEO_H3+thisHC_H3+thisOM_H3+thisRE_H3+thisSH_H3+
                 thisH2_ES+thisES_ES+thisEO_ES+thisHC_ES+thisOM_ES+thisRE_ES+thisSH_ES+thisSO_ES+
                 thisH2_EO+thisES_EO+thisEO_EO+thisHC_EO+thisOM_EO+thisRE_EO+thisSH_EO+thisSO_EO+
                 thisH2_HC+thisES_HC+thisEO_HC+thisHC_HC+thisOM_HC+thisRE_HC+thisSH_HC+thisSO_HC+
                 thisH2_OM+thisES_OM+thisEO_OM+thisHC_OM+thisOM_OM+thisRE_OM+thisSH_OM+thisSO_OM+
                 thisH2_RE+thisES_RE+thisEO_RE+			 thisOM_RE+thisRE_RE+thisSH_RE+thisSO_RE+
                 thisH2_SH+thisES_SH+thisEO_SH+thisHC_SH+thisOM_SH+thisRE_SH+thisSH_SH+thisSO_SH+
                 thisH2_SO+thisES_SO+thisEO_SO+thisHC_SO+thisOM_SO+thisRE_SO+thisSH_SO+thisSO_SO|
                 log(actDur15)+
                 hSIN1+hCOS1|0,
					data=mydata.15dt, reflevel="ST")
summary(mnl2)
save(mnl2, file="mnl2.RData")
mnl3 <- mlogit(chosen~
            	 		   thisES_H2+thisEO_H2+thisHC_H2+thisOM_H2+thisRE_H2+thisSH_H2+
                 		   thisES_H3+thisEO_H3+thisHC_H3+thisOM_H3+thisRE_H3+thisSH_H3+
                 thisH2_ES+thisES_ES+thisEO_ES+thisHC_ES+thisOM_ES+thisRE_ES+thisSH_ES+thisSO_ES+
                 thisH2_EO+thisES_EO+thisEO_EO+thisHC_EO+thisOM_EO+thisRE_EO+thisSH_EO+thisSO_EO+
                 thisH2_HC+thisES_HC+thisEO_HC+thisHC_HC+thisOM_HC+thisRE_HC+thisSH_HC+thisSO_HC+
                 thisH2_OM+thisES_OM+thisEO_OM+thisHC_OM+thisOM_OM+thisRE_OM+thisSH_OM+thisSO_OM+
                 thisH2_RE+thisES_RE+thisEO_RE+thisHC_RE+thisOM_RE+thisRE_RE+thisSH_RE+thisSO_RE+
                 thisH2_SH+thisES_SH+thisEO_SH+thisHC_SH+thisOM_SH+thisRE_SH+thisSH_SH+thisSO_SH+
                 thisH2_SO+thisES_SO+thisEO_SO+thisHC_SO+thisOM_SO+thisRE_SO+thisSH_SO+thisSO_SO+
                 cumDay_H3+cumTour_H2|
                 log(actDur15)+
                 hSIN1+hCOS1|0,
					data=mydata.15dt, reflevel="ST")
summary(mnl3)
save(mnl3, file="mnl3.RData")
mnl4 <- mlogit(chosen~
            	 		   thisES_H2+thisEO_H2+thisHC_H2+thisOM_H2+thisRE_H2+thisSH_H2+
                 		   thisES_H3+thisEO_H3+thisHC_H3+thisOM_H3+thisRE_H3+thisSH_H3+
                 thisH2_ES+thisES_ES+thisEO_ES+thisHC_ES+thisOM_ES+thisRE_ES+thisSH_ES+thisSO_ES+
                 thisH2_EO+thisES_EO+thisEO_EO+thisHC_EO+thisOM_EO+thisRE_EO+thisSH_EO+thisSO_EO+
                 thisH2_HC+thisES_HC+thisEO_HC+thisHC_HC+thisOM_HC+thisRE_HC+thisSH_HC+thisSO_HC+
                 thisH2_OM+thisES_OM+thisEO_OM+thisHC_OM+thisOM_OM+thisRE_OM+thisSH_OM+thisSO_OM+
                 thisH2_RE+thisES_RE+thisEO_RE+thisHC_RE+thisOM_RE+thisRE_RE+thisSH_RE+thisSO_RE+
                 thisH2_SH+thisES_SH+thisEO_SH+thisHC_SH+thisOM_SH+thisRE_SH+thisSH_SH+thisSO_SH+
                 thisH2_SO+thisES_SO+thisEO_SO+thisHC_SO+thisOM_SO+thisRE_SO+thisSH_SO+thisSO_SO+
                 cumDay_H3+cumTour_H2|
                 log(actDur15)+
                 hSIN1+hCOS1+
                 tourNo|0,
					data=mydata.15dt, reflevel="ST")
summary(mnl4)
save(mnl4, file="mnl4.RData")


# multilevel
mnl0.2l <- mlogit(chosen~0|log(actDur15)|0, data=mydata.15dt, reflevel="ST",
                 rpar=c("H2:(intercept)"="n",
                        "H3:(intercept)"="n",
                        "ES:(intercept)"="n",
                        "EO:(intercept)"="n",
                        "HC:(intercept)"="n",
                        "OM:(intercept)"="n",
                        "SH:(intercept)"="n",
                        "SO:(intercept)"="n",
                        "RE:(intercept)"="n"),
                 R=500, halton=NA, print.level=1)
summary(mnl0.2l)
save(mnl0.2l, file="mnl0.2l.RData")

mnl3.2l <- mlogit(chosen~
            	 		   thisES_H2+thisEO_H2+thisHC_H2+thisOM_H2+thisRE_H2+thisSH_H2+
                 		   thisES_H3+thisEO_H3+thisHC_H3+thisOM_H3+thisRE_H3+thisSH_H3+
                 thisH2_ES+thisES_ES+thisEO_ES+thisHC_ES+thisOM_ES+thisRE_ES+thisSH_ES+thisSO_ES+
                 thisH2_EO+thisES_EO+thisEO_EO+thisHC_EO+thisOM_EO+thisRE_EO+thisSH_EO+thisSO_EO+
                 thisH2_HC+thisES_HC+thisEO_HC+thisHC_HC+thisOM_HC+thisRE_HC+thisSH_HC+thisSO_HC+
                 thisH2_OM+thisES_OM+thisEO_OM+thisHC_OM+thisOM_OM+thisRE_OM+thisSH_OM+thisSO_OM+
                 thisH2_RE+thisES_RE+thisEO_RE+thisHC_RE+thisOM_RE+thisRE_RE+thisSH_RE+thisSO_RE+
                 thisH2_SH+thisES_SH+thisEO_SH+thisHC_SH+thisOM_SH+thisRE_SH+thisSH_SH+thisSO_SH+
                 thisH2_SO+thisES_SO+thisEO_SO+thisHC_SO+thisOM_SO+thisRE_SO+thisSH_SO+thisSO_SO+
                 cumDay_H3+cumTour_H2|
                 log(actDur15)+
                 hSIN1+hCOS1|0,
               data=mydata.15dt, reflevel="ST",
               rpar=c("H2:(intercept)"="n",
                      "H3:(intercept)"="n",
                      "ES:(intercept)"="n",
                      "EO:(intercept)"="n",
                      "HC:(intercept)"="n",
                      "OM:(intercept)"="n",
                      "SH:(intercept)"="n",
                      "SO:(intercept)"="n",
                      "RE:(intercept)"="n"),
               R=500, halton=NA, print.level=1)
summary(mnl3.2l)
save(mnl3.2l, file="mnl3.2l.RData")




# there are two approaches to modeling competing risks
# 1. multinomial logit model for y(tij)

MNL <- mlogit.data(event.g1.15dt, shape="wide", choice="nextActG", id="perid")
head(index(MNL),10)
# there are no alternative-specific variables; only choice-situation-specific variables
# the wide format is suitable to store choice-specific variables
 

#####################################
# a series of binary response model #
#####################################
load("event.g1.RData")
#
binaryDT <- function(x, y, t) {
  # to create a data set in "person-episode-period" or "discrete-time" format
  # note that the event is binary
  # x: a data set in "person-episode" format
  # y: one of next activity types (H2,H3,ES,EO,HC,OM,SH,SO,RE)
  # t: one of time scales (5,10,15)
  nextAct <- paste0("next", y)
  time <- paste0("actDur", t)
  max <- max(x[[time]])
  DT <- survSplit(x, cut=(1:max), end=time, event=nextAct, start="start")
  DT <- with(DT, DT[order(SAMPN, PERNO, actNo, start),])
  # add time dummies
  for (i in 1:max) {
    DT[[paste0("t",i)]] <- ifelse(DT[[time]]==i, 1, 0)
  }
  return(DT)
}
#
hazardPlot <- function(x,y,t) {
  # x: a binary discrete-time data set
  # y: one of next activity types (H2,H3,ES,EO,HC,OM,SH,SO,RE)
  # t: one of time scales (5,10,15)
  nextAct <- paste0("next",y)
  time <- paste0("actDur",t)
  maximum <-   max(x[[time]])
  fit <- survfit(Surv(time=x[[time]], event=x[[nextAct]])~1, conf.type="none", data=x)
  h <- round(fit$n.event/fit$n.risk,4)
  nlost <- fit$n.risk - fit$n.event - fit$n.risk[-1]
  nlost[maximum] <- fit$n.risk[maximum] - fit$n.event[maximum]
  LifeTable <- cbind(time=fit$time, risk=fit$n.risk, left=fit$n.event,censored=nlost, hazard=h, survival=round(fit$surv,4))
  plot(fit$time, h, type="l", ylab="Estimated hazard probability", xlab="15-min time intervals")
  # plot(fit$time, fit$surv, type="l", ylab="Estimated Survival Probability", xlab="15-min time intervals")
  return(LifeTable)
}
# circular
library(circular)
HOD <- event.g1$DEP_HR2*2*pi/24      # hours of the day
hist.HOD <- circular(HOD,type="angle",units="radian",rotation="clock")
rose.diag(hist.HOD-pi/2,bins=24,shrink=0.5,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE,prop=1.5)

HOD.H2 <- event.g1[which(event.g1$nextH2==1),]$DEP_HR2*2*pi/24
hist.HOD.H2 <- circular(HOD.H2,type="angle",units="radian",rotation="clock")
rose.diag(hist.HOD.H2-pi/2,bins=24,shrink=0.5,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE,prop=2.5)
rose.diag(hist.HOD.H2-pi/2,bins=24,shrink=1,prop=2)
HOD.H3 <- event.g1[which(event.g1$nextH3==1),]$DEP_HR2*2*pi/24
hist.HOD.H3 <- circular(HOD.H3,type="angle",units="radian",rotation="clock")
rose.diag(hist.HOD.H3-pi/2,bins=24,shrink=0.5,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE,prop=1.5)
HOD.ES <- event.g1[which(event.g1$nextES==1),]$DEP_HR2*2*pi/24
hist.HOD.ES <- circular(HOD.ES,type="angle",units="radian",rotation="clock")
rose.diag(hist.HOD.ES-pi/2,bins=24,shrink=0.5,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE,prop=1.5)
HOD.EO <- event.g1[which(event.g1$nextEO==1),]$DEP_HR2*2*pi/24
hist.HOD.EO <- circular(HOD.EO,type="angle",units="radian",rotation="clock")
rose.diag(hist.HOD.EO-pi/2,bins=24,shrink=0.5,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE,prop=1.5)

######################################## H2 vs. no event
eventH2 <- binaryDT(event.g1,"H2",15)
hazardPlot(eventH2,"H2",15)
## develop the baseline hazard (bl0)
eventH2.bl0 <- glm(nextH2~log(actDur15), data=eventH2, family=binomial(link=logit))
summary(eventH2.bl0)
## add a time-varying covariate: cummulative tour time (bl1)
eventH2$cumTour <- with(eventH2, ifelse(thisActG=='H1'|thisActG=='H2', 0, ((arrive-(TOURDEP_HR*60+TOURDEP_MIN))+(actDur15*15))))
eventH2.bl1 <- glm(nextH2~log(actDur15)+cumTour, data=eventH2, family=binomial(link=logit))
summary(eventH2.bl1)
## add episode-specific covariates:
# periodic or circular time-of-day covariates, rather than adding time-of-day dummies (bl2)
eventH2.bl2 <- glm(nextH2~log(actDur15)+cumTour+
                     hSIN1+hCOS1, data=eventH2, family=binomial(link=logit))
summary(eventH2.bl2)
# current activity type indicators (bl3)
eventH2.bl3 <- glm(nextH2~log(actDur15)+cumTour+
                     hSIN1+hCOS1+
                     thisES+thisEO+thisHC+thisOM+thisRE+thisSH, data=eventH2, family=binomial(link=logit))
summary(eventH2.bl3)
thisES+thisEO+thisHC+thisOM+thisRE+thisSH+thisSO
## add individual-specific covariates: AGE & dayDepart (bl4)
eventH2.bl4 <- glm(nextH2~log(actDur15)+cumTour+
                     hSIN1+hCOS1+
                     thisES+thisHC+thisOM+thisRE+thisSH+thisSO+
                     dayDepart, data=eventH2, family=binomial(link=logit))
summary(eventH2.bl4)
## adding a random intercept effect: (1|perid) (bl5)
eventH2.bl5 <- glmer(nextH2~log(actDur15)+cumTour+
                     hSIN1+hCOS1+
                     thisES+thisEO+thisHC+thisOM+thisRE+thisSH+
                     (1|perid), data=eventH2, family=binomial)
summary(eventH2.bl5)

######################################## H3 vs. no event
eventH3 <- binaryDT(event.g1,"H3",15)
hazardPlot(eventH3,"H3",15)
## develop the baseline hazard (bl0)
eventH3.bl0 <- glm(nextH3~log(actDur15), data=eventH3, family=binomial(link=logit))
summary(eventH3.bl0)
## add a time-varying covariate: cummulative day time (bl1)
eventH3$cumDay <- with(eventH3, ifelse(thisActG=='H1'|thisActG=='H2', 0, ((arrive-dayDepart)+(actDur15*15))))
eventH3.bl1 <- glm(nextH3~log(actDur15)+cumDay, data=eventH3, family=binomial(link=logit))
summary(eventH3.bl1)
## add episode-specific covariates:
# periodic or circular time-of-day covariates, rather than adding time-of-day dummies (bl2)
eventH3.bl2 <- glm(nextH3~log(actDur15)+cumDay+
                     hSIN1+hCOS1, data=eventH3, family=binomial(link=logit))
summary(eventH3.bl2)
# current activity type indicators (bl3)
eventH3.bl3 <- glm(nextH3~log(actDur15)+cumDay+
                     hSIN1+hCOS1+
                     thisEO+thisHC+thisRE+thisSH+thisSO, data=eventH3, family=binomial(link=logit))
summary(eventH3.bl3)
## add individual-specific covariates: AGE & dayDepart (bl4)
eventH3.bl4 <- glm(nextH3~log(actDur15)+cumDay+
                     hSIN1+hCOS1+
                     thisES+thisEO+thisHC+thisOM+thisRE+thisSH+
                     dayDepart, data=eventH3, family=binomial(link=logit))
summary(eventH3.bl4)
eventH3.bl4a <- glm(nextH3~log(actDur15)+cumDay+
                     hSIN1+hCOS1+
                     thisEO+thisHC+thisOM+thisRE+thisSH+thisSO+
                     dayDepart, data=eventH3, family=binomial(link=logit))
summary(eventH3.bl4a)
## adding a random intercept effect: (1|perid) (bl5)
eventH3.bl5 <- glmer(nextH3~log(actDur15)+cumDay+
                     hSIN1+hCOS1+
                     thisEO+thisHC+thisOM+thisRE+thisSH+thisSO+
                     dayDepart+
                     (log(actDur15)|perid), data=eventH3, family=binomial(link=logit))
summary(eventH3.bl5)
(1|tourID)+

######################################## ES vs. no event
eventES <- binaryDT(event.g1,"ES",15)
hazardPlot(eventES,"ES",15)
## develop the baseline hazard (bl0)
eventES.s0 <- glm(nextES~log(actDur15), data=eventES, family=binomial(link=logit))
summary(eventES.s0)
## add episode-specific covariates:
# periodic or circular time-of-day covariates, rather than adding time-of-day dummies (bl2)
eventES.s2 <- glm(nextES~log(actDur15)+
                    hSIN1+hCOS1, data=eventES, family=binomial(link=logit))
summary(eventES.s2)
# current activity type indicators (bl3)
eventES.bl3 <- glm(nextES~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisHC+thisOM+thisRE+thisSH+thisSO, data=eventES, family=binomial(link=logit))
summary(eventES.bl3)
## add individual-specific covariates: AGE & dayDepart (bl4)
eventES.bl4 <- glm(nextES~log(actDur15)+
                     hSIN1+hCOS1+hSIN2+hCOS2+
                     thisH2+thisEO+thisHC+thisOM+thisRE+thisSH+thisSO+
                     dayDepart, data=eventES, family=binomial(link=logit))
summary(eventES.bl4)
## adding a random intercept effect: (1|perid) (bl5)
eventES.m <- glmer(nextES~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisHC+thisOM+thisRE+thisSH+thisSO+
                     (1|perid), data=eventES, family=binomial(link=logit))
summary(eventES.m)


######################################## EO vs. no event
eventEO <- binaryDT(event.g1,"EO",15)
hazardPlot(eventEO,"EO",15)
## develop the baseline hazard (s0)
eventEO.s0 <- glm(nextEO~log(actDur15), data=eventEO, family=binomial(link=logit))
summary(eventEO.s0)
## add covariates (s1)
eventEO$cumDay <- with(eventEO, ifelse(thisActG=='H1'|thisActG=='H2', 0, ((arrive-dayDepart)+(actDur15*15))))
eventEO$cumTour <- with(eventEO, ifelse(thisActG=='H1'|thisActG=='H2', 0, ((arrive-(TOURDEP_HR*60+TOURDEP_MIN))+(actDur15*15))))
eventEO.s1 <- glm(nextEO~log(actDur15)+
                    thisH2+thisES+thisEO+thisHC+thisOM+thisRE+thisSH+thisSO+
                    hSIN1+hCOS1+hSIN2+hCOS2+hSIN3+hCOS3+
                    cumDay+cumTour+                    
                    tourNo, data=eventEO, family=binomial(link=logit))
summary(eventEO.s1)
## multi-level model (m)
eventEO$aeID <- with(eventEO, perid*100 + actNo)
eventEO.m <- glmer(nextEO~log(actDur15)+
                     thisH2+thisES+thisEO+thisHC+thisOM+thisRE+thisSH+thisSO+
                     hSIN1+hCOS1+hSIN2+hCOS2+hSIN3+hCOS3+
                     cumDay+cumTour+                    
                     tourNo+
                     (1|perid)+, data=eventEO, family=binomial(link=logit))
summary(eventEO.m)

## add episode-specific covariatEO:
# periodic or circular time-of-day covariatEO, rather than adding time-of-day dummiEO (bl2)
eventEO.bl2 <- glm(nextEO~log(actDur15)+
                     hSIN1+hCOS1, data=eventEO, family=binomial(link=logit))
summary(eventEO.bl2)
# current activity type indicators (bl3)
eventEO.bl3 <- glm(nextEO~log(actDur15)+
                     hSIN1+hCOS1+hSIN2+hCOS2+
                     thisH2+thisES+thisHC+thisOM+thisSH+thisSO, data=eventEO, family=binomial(link=logit))
summary(eventEO.bl3)
## add individual-specific covariatEO: AGE & dayDepart (bl4)
eventEO.bl4 <- glm(nextEO~log(actDur15)+
                     hSIN1+hCOS1+hSIN2+hCOS2+
                     thisH2+thisES+thisHC+thisOM+thisSH+thisSO+
                     dayDepart, data=eventEO, family=binomial(link=logit))
summary(eventEO.bl4)
## adding a random intercept effect: (1|perid) (bl5)
#eventEO.bl5 <- glm(nextEO~log(actDur15)+cumDay+
#                     hSIN1+hCOS1+
#                     thisEO+thisHC+thisRE+thisSH+thisSO+
#                     AGE+dayDepart+
#                     (1|perid), data=eventEO, family=binomial(link=logit))
#summary(eventEO.bl5)

event.g1 <- subset(event.g1, perid!=80864181)


######################################## HC vs. no event
eventHC <- binaryDT(event.g1,"HC",15)
hazardPlot(eventHC,"HC",15)
## develop the baseline hazard (bl0)
eventHC.bl0 <- glm(nextHC~log(actDur15), data=eventHC, family=binomial(link=logit))
summary(eventHC.bl0)
## add episode-specific covariatHC:
# periodic or circular time-of-day covariatHC, rather than adding time-of-day dummiHC (bl2)
eventHC.bl2 <- glm(nextHC~log(actDur15)+
                     hSIN1+hCOS1, data=eventHC, family=binomial(link=logit))
summary(eventHC.bl2)
# current activity type indicators (bl3)
eventHC.bl3 <- glm(nextHC~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisOM+thisRE+thisSH+thisSO, data=eventHC, family=binomial(link=logit))
summary(eventHC.bl3)
+thisES+thisHC+thisOM+thisSH+thisSO
## add individual-specific covariatHC: AGE & dayDepart (bl4)
eventHC.bl4 <- glm(nextHC~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisOM+thisRE+thisSH+thisSO+
                     dayDepart, data=eventHC, family=binomial(link=logit))
summary(eventHC.bl4)
## adding a random intercept effect: (1|perid) (bl5)
#eventHC.bl5 <- glm(nextHC~log(actDur15)+cumDay+
#                     hSIN1+hCOS1+
#                     thisHC+thisHC+thisRE+thisSH+thisSO+
#                     AGE+dayDepart+
#                     (1|perid), data=eventHC, family=binomial(link=logit))
#summary(eventHC.bl5)


######################################## OM vs. no event
eventOM <- binaryDT(event.g1,"OM",15)
hazardPlot(eventOM,"OM",15)
## develop the baseline hazard (bl0)
eventOM.bl0 <- glm(nextOM~log(actDur15), data=eventOM, family=binomial(link=logit))
summary(eventOM.bl0)
## add episode-specific covariatOM:
# periodic or circular time-of-day covariatOM, rather than adding time-of-day dummiOM (bl2)
eventOM.bl2 <- glm(nextOM~log(actDur15)+
                     hSIN1+hCOS1, data=eventOM, family=binomial(link=logit))
summary(eventOM.bl2)
# current activity type indicators (bl3)
eventOM.bl3 <- glm(nextOM~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisSH, data=eventOM, family=binomial(link=logit))
summary(eventOM.bl3)
## add individual-specific covariatOM: AGE & dayDepart (bl4)
eventOM.bl4 <- glm(nextOM~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisSH+
                     dayDepart, data=eventOM, family=binomial(link=logit))
summary(eventOM.bl4)
## adding a random intercept effect: (1|perid) (bl5)
#eventOM.bl5 <- glm(nextOM~log(actDur15)+cumDay+
#                     hSIN1+OMOS1+
#                     thisOM+thisOM+thisRE+thisSH+thisSO+
#                     AGE+dayDepart+
#                     (1|perid), data=eventOM, family=binomial(link=logit))
#summary(eventOM.bl5)

######################################## RE vs. no event
eventRE <- binaryDT(event.g1,"RE",15)
hazardPlot(eventRE,"RE",15)
## develop the baseline hazard (bl0)
eventRE.bl0 <- glm(nextRE~log(actDur15), data=eventRE, family=binomial(link=logit))
summary(eventRE.bl0)
## add episode-specific covariatRE:
# periodic or circular time-of-day covariatRE, rather than adding time-of-day dummiRE (bl2)
eventRE.bl2 <- glm(nextRE~log(actDur15)+
                     hSIN1+hCOS1, data=eventRE, family=binomial(link=logit))
summary(eventRE.bl2)
# current activity type indicators (bl3)
eventRE.bl3 <- glm(nextRE~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisOM+thisSH+thisSO, data=eventRE, family=binomial(link=logit))
summary(eventRE.bl3)
## add individual-specific covariatRE: AGE & dayDepart (bl4)
eventRE.bl4 <- glm(nextRE~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisOM+thisSH+thisSO+
                     dayDepart, data=eventRE, family=binomial(link=logit))
summary(eventRE.bl4)
## adding a randRE intercept effect: (1|perid) (bl5)
#eventRE.bl5 <- glm(nextRE~log(actDur15)+cumDay+
#                     hSIN1+REOS1+
#                     thisRE+thisRE+thisRE+thisSH+thisSO+
#                     AGE+dayDepart+
#                     (1|perid), data=eventRE, family=binomial(link=logit))
#summary(eventRE.bl5)


######################################## SH vs. no event
eventSH <- binaryDT(event.g1,"SH",15)
hazardPlot(eventSH,"SH",15)
## develop the baseline hazard (bl0)
eventSH.bl0 <- glm(nextSH~log(actDur15), data=eventSH, family=binomial(link=logit))
summary(eventSH.bl0)
## add episode-specific covariates:
# periodic or circular time-of-day covariates, rather than adding time-of-day dummies (bl2)
eventSH.bl2 <- glm(nextSH~log(actDur15)+
                     hSIN1+hCOS1, data=eventSH, family=binomial(link=logit))
summary(eventSH.bl2)
# current activity type indicators (bl3)
eventSH.bl3 <- glm(nextSH~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisOM+thisRE+thisSO, data=eventSH, family=binomial(link=logit))
summary(eventSH.bl3)
## add individual-specific covariates: AGE & dayDepart (bl4)
eventSH.bl4 <- glm(nextSH~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisOM+thisRE+thisSO+
                     dayDepart, data=eventSH, family=binomial(link=logit))
summary(eventSH.bl4)
## adding a randSH intercept effect: (1|perid) (bl5)
#eventSH.bl5 <- glm(nextSH~log(actDur15)+cumDay+
#                     hSIN1+SHOS1+
#                     thisSH+thisSH+thisSH+thisSH+thisSO+
#                     AGE+dayDepart+
#                     (1|perid), data=eventSH, family=binomial(link=logit))
#summary(eventSH.bl5)


######################################## SO vs. no event
eventSO <- binaryDT(event.g1,"SO",15)
hazardPlot(eventSO,"SO",15)
## develop the baseline hazard (bl0)
eventSO.bl0 <- glm(nextSO~log(actDur15), data=eventSO, family=binomial(link=logit))
summary(eventSO.bl0)
## add episode-specific covariates:
# periodic or circular time-of-day covariates, rather than adding time-of-day dummies (bl2)
eventSO.bl2 <- glm(nextSO~log(actDur15)+
                     hSIN1+hCOS1+hSIN2+hCOS2, data=eventSO, family=binomial(link=logit))
summary(eventSO.bl2)
# current activity type indicators (bl3)
eventSO.bl3 <- glm(nextSO~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisHC+thisOM+thisRE+thisSH, data=eventSO, family=binomial(link=logit))
summary(eventSO.bl3)
## add individual-specific covariates: AGE & dayDepart (bl4)
eventSO.bl4 <- glm(nextSO~log(actDur15)+
                     hSIN1+hCOS1+
                     thisH2+thisES+thisEO+thisHC+thisOM+thisRE+thisSH+thisSO+
                     dayDepart, data=eventSO, family=binomial(link=logit))
summary(eventSO.bl4)
## adding a random intercept effect: (1|perid) (bl5)
#eventSO.bl5 <- glm(nextSO~log(actDur15)+cumDay+
#                     hSIN1+SOOS1+
#                     thisSO+thisSO+thisSO+thisSO+thisSO+
#                     AGE+dayDepart+
#                     (1|perid), data=eventSO, family=binomial(link=logit))
#summary(eventSO.bl5)

# when the next Activity is "H2"
# convert from "person-episode" to "person-episode-period"
eventH2 <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextH2", start="start")
eventH2 <- with(eventH2, eventH2[order(SAMPN, PERNO, actNo, start),])
# estimate hazard
H2.fit <- survfit(Surv(time=actDur15, event=nextH2)~1, conf.type="none", data=eventH2)
H2.H <- with(H2.fit, n.event/n.risk)
H2.LT <- cbind(time=H2.fit$time, risk=H2.fit$n.risk, move=H2.fit$n.event, hazard=H2.H, survival=H2.fit$surv)
H2.LT
plot(H2.fit$time, H2.H, type="l", ylab="Estimated hazard probability", xlab="15-min time intervals")
plot(H2.fit$time, H2.fit$surv, type="l", ylab="Estimated Survival Probability", xlab="15-min time intervals")
# add time dummies
eventH2$t1 <- with(eventH2, ifelse(actDur15==1, 1, 0)) # (0, 15]
eventH2$t2 <- with(eventH2, ifelse(actDur15==2, 1, 0)) # (15, 30]
eventH2$t3 <- with(eventH2, ifelse(actDur15==3, 1, 0)) # (30, 45]
eventH2$t4 <- with(eventH2, ifelse(actDur15==4, 1, 0)) # (45, 60]
eventH2$t5 <- with(eventH2, ifelse(actDur15==5, 1, 0)) # (60, 75]
eventH2$t6 <- with(eventH2, ifelse(actDur15==6, 1, 0)) # (75, 90]
eventH2$t7 <- with(eventH2, ifelse(actDur15==7, 1, 0)) # (90, 105]
eventH2$t8 <- with(eventH2, ifelse(actDur15==8, 1, 0)) # (105, 120]
eventH2$t9 <- with(eventH2, ifelse(actDur15==9, 1, 0)) # (120, 135]
eventH2$t10 <- with(eventH2, ifelse(actDur15==10, 1, 0)) # (135, 150]
eventH2$t11 <- with(eventH2, ifelse(actDur15==11, 1, 0)) # (150, 165]
eventH2$t12 <- with(eventH2, ifelse(actDur15==12, 1, 0)) # (165, 180]
eventH2$t13 <- with(eventH2, ifelse(actDur15==13, 1, 0)) # (180, 195]
eventH2$t14 <- with(eventH2, ifelse(actDur15==14, 1, 0)) # (195, 210]
eventH2$t15 <- with(eventH2, ifelse(actDur15>=15, 1, 0)) # (210, ...]

# estimate a binary logit model (m1)
eventH2.m1 <- glm(nextH2~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple states
                    thisES+thisEO+thisHC+thisOM+thisRE+thisSH+
                    # time-of-day effects
                    T0607+T0708+T0809+T0910+T1011+T1112+T1213+T1314+T1415+T1516+T1617+T1718+
                    # episode-level effects
                    tripDur+
                    # individual-level effects
                    dayDepart+GEND+AGE+PBIKE,
                  data=eventH2, family=binomial(link=logit))
summary(eventH2.m1)

eventH2.m1 <- glm(nextH2~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventH2, family=binomial(link=logit))
eventH2.m2 <- glm(nextH2~log(actDur15),
                  data=eventH2, family=binomial(link=logit))
eventH2.m3 <- glm(nextH2~actDur15+I(actDur15^2),
                  data=eventH2, family=binomial(link=logit))
summary(eventH2.m1)
summary(eventH2.m2)
summary(eventH2.m3)
# add cummulative tour time (time-varying variable)
eventH2$cumTour <- with(eventH2, ifelse(thisActG=='H1'|thisActG=='H2', 0, ((arrive-(TOURDEP_HR*60+TOURDEP_MIN))+(actDur15*15))))

eventH2.m4 <- glm(nextH2~log(actDur15)+
                    cumTour,
                  data=eventH2, family=binomial(link=logit))
eventH2.m5 <- glm(nextH2~actDur15+I(actDur15^2)+
                    cumTour,
                  data=eventH2, family=binomial(link=logit))

summary(eventH2.m4)
summary(eventH2.m5)

# when the next Activity is "H3"
# convert from "person-episode" to "person-episode-period"
eventH3 <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextH3", start="start")
eventH3 <- with(eventH3, eventH3[order(SAMPN, PERNO, actNo, start),])
# estimate hazard
H3.fit <- survfit(Surv(time=actDur15, event=nextH3)~1, conf.type="none", data=eventH3)
H3.H <- with(H3.fit, n.event/n.risk)
H3.LT <- cbind(time=H3.fit$time, risk=H3.fit$n.risk, move=H3.fit$n.event, hazard=H3.H, survival=H3.fit$surv)
H3.LT
plot(H3.fit$time, H3.H, type="l", ylab="Estimated hazard probability", xlab="15-min time intervals")
plot(H3.fit$time, H3.fit$surv, type="l", ylab="Estimated Survival Probability", xlab="15-min time intervals")
# add time dummies
eventH3$t1 <- with(eventH3, ifelse(actDur15==1, 1, 0)) # (0, 15]
eventH3$t2 <- with(eventH3, ifelse(actDur15==2, 1, 0)) # (15, 30]
eventH3$t3 <- with(eventH3, ifelse(actDur15==3, 1, 0)) # (30, 45]
eventH3$t4 <- with(eventH3, ifelse(actDur15==4, 1, 0)) # (45, 60]
eventH3$t5 <- with(eventH3, ifelse(actDur15==5, 1, 0)) # (60, 75]
eventH3$t6 <- with(eventH3, ifelse(actDur15==6, 1, 0)) # (75, 90]
eventH3$t7 <- with(eventH3, ifelse(actDur15==7, 1, 0)) # (90, 105]
eventH3$t8 <- with(eventH3, ifelse(actDur15==8, 1, 0)) # (105, 120]
eventH3$t9 <- with(eventH3, ifelse(actDur15==9, 1, 0)) # (120, 135]
eventH3$t10 <- with(eventH3, ifelse(actDur15==10, 1, 0)) # (135, 150]
eventH3$t11 <- with(eventH3, ifelse(actDur15==11, 1, 0)) # (150, 165]
eventH3$t12 <- with(eventH3, ifelse(actDur15==12, 1, 0)) # (165, 180]
eventH3$t13 <- with(eventH3, ifelse(actDur15==13, 1, 0)) # (180, 195]
eventH3$t14 <- with(eventH3, ifelse(actDur15==14, 1, 0)) # (195, 210]
eventH3$t15 <- with(eventH3, ifelse(actDur15>=15, 1, 0)) # (210, ...]
# estimate a binary logit model (m1)
eventH3.m1 <- glm(nextH3~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventH3, family=binomial(link=logit))
eventH3.m2 <- glm(nextH3~log(actDur15),
                  data=eventH3, family=binomial(link=logit))
eventH3.m3 <- glm(nextH3~actDur15+I(actDur15^2),
                  data=eventH3, family=binomial(link=logit))
summary(eventH3.m1)
summary(eventH3.m2)
summary(eventH3.m3)
# add cummulative day time (time-varying variable)
sqldf("select perid,actNo,arrive,depart,thisActG,actDur,actDur15,nextActG,dayDepart,TOURDEP_HR,TOURDEP_MIN,cumDay,cumTour
      from eventH3 where perid=80000402")
eventH3$cumDay <- with(eventH3, ifelse(thisActG=='H1'|thisActG=='H2', 0, ((arrive-dayDepart)+(actDur15*15))))

eventH3.m4 <- glm(nextH3~log(actDur15)+
                    cumDay,
                  data=eventH3, family=binomial(link=logit))
eventH3.m5 <- glm(nextH3~actDur15+I(actDur15^2)+
                    cumDay,
                  data=eventH3, family=binomial(link=logit))

summary(eventH3.m4)
summary(eventH3.m5)




eventH3.m1 <- glm(nextH3~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple states
                    thisES+thisEO+thisMT+thisSR+
                    # time-of-day effects
                    T0809+T0910+T1011+T1112+T1213+T1314+T1415+
                    T1516+T1617+T1718+T1819+T1920+T2021+T2122+T2223+
                    # episode-level effects
                    tripDur+
                    # individual-level effects
                    AGE,
                  data=eventH3, family=binomial(link=logit))
summary(eventH3.m1)

# when the next Activity is "ES"
# convert from "person-episode" to "person-episode-period"
eventES <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextES", start="start")
eventES <- with(eventES, eventES[order(SAMPN, PERNO, actNo, start),])
# estimate hazard
ES.fit <- survfit(Surv(time=actDur15, event=nextES)~1, conf.type="none", data=eventES)
ES.H <- with(ES.fit, n.event/n.risk)
ES.LT <- cbind(time=ES.fit$time, risk=ES.fit$n.risk, move=ES.fit$n.event, hazard=ES.H, survival=ES.fit$surv)
ES.LT
plot(ES.fit$time, ES.H, type="l", ylab="Estimated hazard probability", xlab="15-min time intervals")
plot(ES.fit$time, ES.fit$surv, type="l", ylab="Estimated Survival Probability", xlab="15-min time intervals")
# add time dummies
eventES$t1 <- with(eventES, ifelse(actDur15==1, 1, 0)) # [0, 15)
eventES$t2 <- with(eventES, ifelse(actDur15==2, 1, 0)) # [15, 30)
eventES$t3 <- with(eventES, ifelse(actDur15==3, 1, 0)) # [30, 45)
eventES$t4 <- with(eventES, ifelse(actDur15==4, 1, 0)) # [45, 60)
eventES$t5 <- with(eventES, ifelse(actDur15==5, 1, 0)) # [60, 75)
eventES$t6 <- with(eventES, ifelse(actDur15==6, 1, 0)) # [75, 90)
eventES$t7 <- with(eventES, ifelse(actDur15==7, 1, 0)) # [90, 105)
eventES$t8 <- with(eventES, ifelse(actDur15==8, 1, 0)) # [105, 120)
eventES$t9 <- with(eventES, ifelse(actDur15==9, 1, 0)) # [120, 135)
eventES$t10 <- with(eventES, ifelse(actDur15==10, 1, 0)) # [135, 150)
eventES$t11 <- with(eventES, ifelse(actDur15==11, 1, 0)) # [150, 165)
eventES$t12 <- with(eventES, ifelse(actDur15==12, 1, 0)) # [165, 180)
eventES$t13 <- with(eventES, ifelse(actDur15==13, 1, 0)) # [180, 195)
eventES$t14 <- with(eventES, ifelse(actDur15==14, 1, 0)) # [195, 210)
eventES$t15 <- with(eventES, ifelse(actDur15>=15, 1, 0)) # [210, ...)
# estimate a binary logit model (m1)
eventES.m1 <- glm(nextES~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventES, family=binomial(link=logit))
eventES.m2 <- glm(nextES~log(actDur15),
                  data=eventES, family=binomial(link=logit))
eventES.m3 <- glm(nextES~actDur15+I(actDur15^2),
                  data=eventES, family=binomial(link=logit))
summary(eventES.m1)
summary(eventES.m2)
summary(eventES.m3)

eventES.m1 <- glm(nextES~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple states
                    thisH2+thisES+thisEO+thisMT+
                    # time-of-day effects
                    T0708+T1011+T1112+T1213+T1314+T1415+
                    # episode-level effects
                    tripDur+
                    # individual-level effects
                    AGE,
                  data=eventES, family=binomial(link=logit))
summary(eventES.m1)

# when the next Activity is "EO"
# convert from "person-episode" to "person-episode-period"
eventEO <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextEO", start="start")
eventEO <- with(eventEO, eventEO[order(SAMPN, PERNO, actNo, start),])
# EOtimate hazard
EO.fit <- survfit(Surv(time=actDur15, event=nextEO)~1, conf.type="none", data=eventEO)
EO.H <- with(EO.fit, n.event/n.risk)
EO.LT <- cbind(time=EO.fit$time, risk=EO.fit$n.risk, move=EO.fit$n.event, hazard=EO.H, survival=EO.fit$surv)
EO.LT
plot(EO.fit$time, EO.H, type="l", ylab="EOtimated hazard probability", xlab="15-min time intervals")
plot(EO.fit$time, EO.fit$surv, type="l", ylab="EOtimated Survival Probability", xlab="15-min time intervals")
# add time dummiEO
eventEO$t1 <- with(eventEO, ifelse(actDur15==1, 1, 0)) # [0, 15)
eventEO$t2 <- with(eventEO, ifelse(actDur15==2, 1, 0)) # [15, 30)
eventEO$t3 <- with(eventEO, ifelse(actDur15==3, 1, 0)) # [30, 45)
eventEO$t4 <- with(eventEO, ifelse(actDur15==4, 1, 0)) # [45, 60)
eventEO$t5 <- with(eventEO, ifelse(actDur15==5, 1, 0)) # [60, 75)
eventEO$t6 <- with(eventEO, ifelse(actDur15==6, 1, 0)) # [75, 90)
eventEO$t7 <- with(eventEO, ifelse(actDur15==7, 1, 0)) # [90, 105)
eventEO$t8 <- with(eventEO, ifelse(actDur15==8, 1, 0)) # [105, 120)
eventEO$t9 <- with(eventEO, ifelse(actDur15==9, 1, 0)) # [120, 135)
eventEO$t10 <- with(eventEO, ifelse(actDur15==10, 1, 0)) # [135, 150)
eventEO$t11 <- with(eventEO, ifelse(actDur15==11, 1, 0)) # [150, 165)
eventEO$t12 <- with(eventEO, ifelse(actDur15==12, 1, 0)) # [165, 180)
eventEO$t13 <- with(eventEO, ifelse(actDur15==13, 1, 0)) # [180, 195)
eventEO$t14 <- with(eventEO, ifelse(actDur15==14, 1, 0)) # [195, 210)
eventEO$t15 <- with(eventEO, ifelse(actDur15>=15, 1, 0)) # [210, ...)
# EOtimate a binary logit model (m1)
eventEO.m1 <- glm(nextEO~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventEO, family=binomial(link=logit))
eventEO.m2 <- glm(nextEO~log(actDur15),
                  data=eventEO, family=binomial(link=logit))
eventEO.m3 <- glm(nextEO~actDur15+I(actDur15^2),
                  data=eventEO, family=binomial(link=logit))
summary(eventEO.m1)
summary(eventEO.m2)
summary(eventEO.m3)

eventEO.m1 <- glm(nextEO~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple statEO
                    thisH1+thisH2+thisEO+thisEO+thisMT+thisSR+
                    # time-of-day effects
                    T0708+T1011+T1112+T1213+T1314+T1415+
                    # episode-level effects
                    tripDur+
                    # individual-level effects
                    AGE,
                  data=eventEO, family=binomial(link=logit))
summary(eventEO.m1)

# when the next Activity is "HC"
# convert from "person-episode" to "person-episode-period"
eventHC <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextHC", start="start")
eventHC <- with(eventHC, eventHC[order(SAMPN, PERNO, actNo, start),])
# HCtimate hazard
HC.fit <- survfit(Surv(time=actDur15, event=nextHC)~1, conf.type="none", data=eventHC)
HC.H <- with(HC.fit, n.event/n.risk)
HC.LT <- cbind(time=HC.fit$time, risk=HC.fit$n.risk, move=HC.fit$n.event, hazard=HC.H, survival=HC.fit$surv)
HC.LT
plot(HC.fit$time, HC.H, type="l", ylab="HCtimated hazard probability", xlab="15-min time intervals")
plot(HC.fit$time, HC.fit$surv, type="l", ylab="HCtimated Survival Probability", xlab="15-min time intervals")
# add time dummiHC
eventHC$t1 <- with(eventHC, ifelse(actDur15==1, 1, 0)) # [0, 15)
eventHC$t2 <- with(eventHC, ifelse(actDur15==2, 1, 0)) # [15, 30)
eventHC$t3 <- with(eventHC, ifelse(actDur15==3, 1, 0)) # [30, 45)
eventHC$t4 <- with(eventHC, ifelse(actDur15==4, 1, 0)) # [45, 60)
eventHC$t5 <- with(eventHC, ifelse(actDur15==5, 1, 0)) # [60, 75)
eventHC$t6 <- with(eventHC, ifelse(actDur15==6, 1, 0)) # [75, 90)
eventHC$t7 <- with(eventHC, ifelse(actDur15==7, 1, 0)) # [90, 105)
eventHC$t8 <- with(eventHC, ifelse(actDur15==8, 1, 0)) # [105, 120)
eventHC$t9 <- with(eventHC, ifelse(actDur15==9, 1, 0)) # [120, 135)
eventHC$t10 <- with(eventHC, ifelse(actDur15==10, 1, 0)) # [135, 150)
eventHC$t11 <- with(eventHC, ifelse(actDur15==11, 1, 0)) # [150, 165)
eventHC$t12 <- with(eventHC, ifelse(actDur15==12, 1, 0)) # [165, 180)
eventHC$t13 <- with(eventHC, ifelse(actDur15==13, 1, 0)) # [180, 195)
eventHC$t14 <- with(eventHC, ifelse(actDur15==14, 1, 0)) # [195, 210)
eventHC$t15 <- with(eventHC, ifelse(actDur15>=15, 1, 0)) # [210, ...)
# HCtimate a binary logit model (m1)
eventHC.m1 <- glm(nextHC~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventHC, family=binomial(link=logit))
eventHC.m2 <- glm(nextHC~log(actDur15),
                  data=eventHC, family=binomial(link=logit))
eventHC.m3 <- glm(nextHC~actDur15+I(actDur15^2),
                  data=eventHC, family=binomial(link=logit))
summary(eventHC.m1)
summary(eventHC.m2)
summary(eventHC.m3)

eventHC.m1 <- glm(nextHC~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple statHC
                    thisH1+thisH2+thisHC+thisHC+thisHC+thisSR+
                    # time-of-day effects
                    T0708+T1011+T1112+T1213+T1314+T1415+
                    # episode-level effects
                    tripDur+MODE+
                    # individual-level effects
                    AGE,
                  data=eventHC, family=binomial(link=logit))
summary(eventHC.m1)

# when the next Activity is "OM"
# convert from "person-episode" to "person-episode-period"
eventOM <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextOM", start="start")
eventOM <- with(eventOM, eventOM[order(SAMPN, PERNO, actNo, start),])
# OMtimate hazard
OM.fit <- survfit(Surv(time=actDur15, event=nextOM)~1, conf.type="none", data=eventOM)
OM.H <- with(OM.fit, n.event/n.risk)
OM.LT <- cbind(time=OM.fit$time, risk=OM.fit$n.risk, move=OM.fit$n.event, hazard=OM.H, survival=OM.fit$surv)
OM.LT
plot(OM.fit$time, OM.H, type="l", ylab="OMtimated hazard probability", xlab="15-min time intervals")
plot(OM.fit$time, OM.fit$surv, type="l", ylab="OMtimated Survival Probability", xlab="15-min time intervals")
# add time dummiOM
eventOM$t1 <- with(eventOM, ifelse(actDur15==1, 1, 0)) # [0, 15)
eventOM$t2 <- with(eventOM, ifelse(actDur15==2, 1, 0)) # [15, 30)
eventOM$t3 <- with(eventOM, ifelse(actDur15==3, 1, 0)) # [30, 45)
eventOM$t4 <- with(eventOM, ifelse(actDur15==4, 1, 0)) # [45, 60)
eventOM$t5 <- with(eventOM, ifelse(actDur15==5, 1, 0)) # [60, 75)
eventOM$t6 <- with(eventOM, ifelse(actDur15==6, 1, 0)) # [75, 90)
eventOM$t7 <- with(eventOM, ifelse(actDur15==7, 1, 0)) # [90, 105)
eventOM$t8 <- with(eventOM, ifelse(actDur15==8, 1, 0)) # [105, 120)
eventOM$t9 <- with(eventOM, ifelse(actDur15==9, 1, 0)) # [120, 135)
eventOM$t10 <- with(eventOM, ifelse(actDur15==10, 1, 0)) # [135, 150)
eventOM$t11 <- with(eventOM, ifelse(actDur15==11, 1, 0)) # [150, 165)
eventOM$t12 <- with(eventOM, ifelse(actDur15==12, 1, 0)) # [165, 180)
eventOM$t13 <- with(eventOM, ifelse(actDur15==13, 1, 0)) # [180, 195)
eventOM$t14 <- with(eventOM, ifelse(actDur15==14, 1, 0)) # [195, 210)
eventOM$t15 <- with(eventOM, ifelse(actDur15>=15, 1, 0)) # [210, ...)
# OMtimate a binary logit model (m1)
eventOM.m1 <- glm(nextOM~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventOM, family=binomial(link=logit))
eventOM.m2 <- glm(nextOM~log(actDur15),
                  data=eventOM, family=binomial(link=logit))
eventOM.m3 <- glm(nextOM~actDur15+I(actDur15^2),
                  data=eventOM, family=binomial(link=logit))
summary(eventOM.m1)
summary(eventOM.m2)
summary(eventOM.m3)

eventOM.m1 <- glm(nextOM~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple statOM
                    thisH1+thisH2+thisOM+thisOM+thisOM+thisSR+
                    # time-of-day effects
                    T0708+T1011+T1112+T1213+T1314+T1415+
                    # episode-level effects
                    tripDur+MODE+
                    # individual-level effects
                    AGE,
                  data=eventOM, family=binomial(link=logit))
summary(eventOM.m1)

# when the next Activity is "RE"
# convert from "person-episode" to "person-episode-period"
eventRE <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextRE", start="start")
eventRE <- with(eventRE, eventRE[order(SAMPN, PERNO, actNo, start),])
# REtimate hazard
RE.fit <- survfit(Surv(time=actDur15, event=nextRE)~1, conf.type="none", data=eventRE)
RE.H <- with(RE.fit, n.event/n.risk)
RE.LT <- cbind(time=RE.fit$time, risk=RE.fit$n.risk, move=RE.fit$n.event, hazard=RE.H, survival=RE.fit$surv)
RE.LT
plot(RE.fit$time, RE.H, type="l", ylab="REtimated hazard probability", xlab="15-min time intervals")
plot(RE.fit$time, RE.fit$surv, type="l", ylab="REtimated Survival Probability", xlab="15-min time intervals")
# add time dummiRE
eventRE$t1 <- with(eventRE, ifelse(actDur15==1, 1, 0)) # [0, 15)
eventRE$t2 <- with(eventRE, ifelse(actDur15==2, 1, 0)) # [15, 30)
eventRE$t3 <- with(eventRE, ifelse(actDur15==3, 1, 0)) # [30, 45)
eventRE$t4 <- with(eventRE, ifelse(actDur15==4, 1, 0)) # [45, 60)
eventRE$t5 <- with(eventRE, ifelse(actDur15==5, 1, 0)) # [60, 75)
eventRE$t6 <- with(eventRE, ifelse(actDur15==6, 1, 0)) # [75, 90)
eventRE$t7 <- with(eventRE, ifelse(actDur15==7, 1, 0)) # [90, 105)
eventRE$t8 <- with(eventRE, ifelse(actDur15==8, 1, 0)) # [105, 120)
eventRE$t9 <- with(eventRE, ifelse(actDur15==9, 1, 0)) # [120, 135)
eventRE$t10 <- with(eventRE, ifelse(actDur15==10, 1, 0)) # [135, 150)
eventRE$t11 <- with(eventRE, ifelse(actDur15==11, 1, 0)) # [150, 165)
eventRE$t12 <- with(eventRE, ifelse(actDur15==12, 1, 0)) # [165, 180)
eventRE$t13 <- with(eventRE, ifelse(actDur15==13, 1, 0)) # [180, 195)
eventRE$t14 <- with(eventRE, ifelse(actDur15==14, 1, 0)) # [195, 210)
eventRE$t15 <- with(eventRE, ifelse(actDur15>=15, 1, 0)) # [210, ...)
# REtimate a binary logit model (m1)
eventRE.m1 <- glm(nextRE~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventRE, family=binomial(link=logit))
eventRE.m2 <- glm(nextRE~log(actDur15),
                  data=eventRE, family=binomial(link=logit))
eventRE.m3 <- glm(nextRE~actDur15+I(actDur15^2),
                  data=eventRE, family=binomial(link=logit))
summary(eventRE.m1)
summary(eventRE.m2)
summary(eventRE.m3)

eventRE.m1 <- glm(nextRE~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple statRE
                    thisH2+thisRE+thisRE+thisRE+thisRE+
                    # time-of-day effects
                    T0708+T1011+T1112+T1213+T1314+T1415+
                    T1516+T1617+T1718+T1819+T1920+T2021+T2122+
                    # episode-level effects
                    tripDur+MODE+
                    # individual-level effects
                    AGE,
                  data=eventRE, family=binomial(link=logit))
summary(eventRE.m1)

# when the next Activity is "SO"
# convert from "person-episode" to "person-episode-period"
eventSO <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextSO", start="start")
eventSO <- with(eventSO, eventSO[order(SAMPN, PERNO, actNo, start),])
# SOtimate hazard
SO.fit <- survfit(Surv(time=actDur15, event=nextSO)~1, conf.type="none", data=eventSO)
SO.H <- with(SO.fit, n.event/n.risk)
SO.LT <- cbind(time=SO.fit$time, risk=SO.fit$n.risk, move=SO.fit$n.event, hazard=SO.H, survival=SO.fit$surv)
SO.LT
plot(SO.fit$time, SO.H, type="l", ylab="SOtimated hazard probability", xlab="15-min time intervals")
plot(SO.fit$time, SO.fit$surv, type="l", ylab="SOtimated Survival Probability", xlab="15-min time intervals")
# add time dummiSO
eventSO$t1 <- with(eventSO, ifelse(actDur15==1, 1, 0)) # [0, 15)
eventSO$t2 <- with(eventSO, ifelse(actDur15==2, 1, 0)) # [15, 30)
eventSO$t3 <- with(eventSO, ifelse(actDur15==3, 1, 0)) # [30, 45)
eventSO$t4 <- with(eventSO, ifelse(actDur15==4, 1, 0)) # [45, 60)
eventSO$t5 <- with(eventSO, ifelse(actDur15==5, 1, 0)) # [60, 75)
eventSO$t6 <- with(eventSO, ifelse(actDur15==6, 1, 0)) # [75, 90)
eventSO$t7 <- with(eventSO, ifelse(actDur15==7, 1, 0)) # [90, 105)
eventSO$t8 <- with(eventSO, ifelse(actDur15==8, 1, 0)) # [105, 120)
eventSO$t9 <- with(eventSO, ifelse(actDur15==9, 1, 0)) # [120, 135)
eventSO$t10 <- with(eventSO, ifelse(actDur15==10, 1, 0)) # [135, 150)
eventSO$t11 <- with(eventSO, ifelse(actDur15==11, 1, 0)) # [150, 165)
eventSO$t12 <- with(eventSO, ifelse(actDur15==12, 1, 0)) # [165, 180)
eventSO$t13 <- with(eventSO, ifelse(actDur15==13, 1, 0)) # [180, 195)
eventSO$t14 <- with(eventSO, ifelse(actDur15==14, 1, 0)) # [195, 210)
eventSO$t15 <- with(eventSO, ifelse(actDur15>=15, 1, 0)) # [210, ...)
# SOtimate a binary logit model (m1)
eventSO.m1 <- glm(nextSO~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventSO, family=binomial(link=logit))
eventSO.m2 <- glm(nextSO~log(actDur15),
                  data=eventSO, family=binomial(link=logit))
eventSO.m3 <- glm(nextSO~actDur15+I(actDur15^2),
                  data=eventSO, family=binomial(link=logit))
summary(eventSO.m1)
summary(eventSO.m2)
summary(eventSO.m3)

eventSO.m1 <- glm(nextSO~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple statSO
                    thisH2+thisSO+thisSO+thisSO+thisSO+
                    # time-of-day effects
                    T0708+T1011+T1112+T1213+T1314+T1415+
                    T1516+T1617+T1718+T1819+T1920+T2021+T2122+
                    # episode-level effects
                    tripDur+MODE+
                    # individual-level effects
                    AGE,
                  data=eventSO, family=binomial(link=logit))
summary(eventSO.m1)

# when the next Activity is "SH"
# convert from "perSHn-epiSHde" to "perSHn-epiSHde-period"
eventSH <- survSplit(event.g1, cut=(1:max(event.g1$actDur15)), end="actDur15", event="nextSH", start="start")
eventSH <- with(eventSH, eventSH[order(SAMPN, PERNO, actNo, start),])
# SHtimate hazard
SH.fit <- survfit(Surv(time=actDur15, event=nextSH)~1, conf.type="none", data=eventSH)
SH.H <- with(SH.fit, n.event/n.risk)
SH.LT <- cbind(time=SH.fit$time, risk=SH.fit$n.risk, move=SH.fit$n.event, hazard=SH.H, survival=SH.fit$surv)
SH.LT
plot(SH.fit$time, SH.H, type="l", ylab="SHtimated hazard probability", xlab="15-min time intervals")
plot(SH.fit$time, SH.fit$surv, type="l", ylab="SHtimated Survival Probability", xlab="15-min time intervals")
# add time dummiSH
eventSH$t1 <- with(eventSH, ifelse(actDur15==1, 1, 0)) # [0, 15)
eventSH$t2 <- with(eventSH, ifelse(actDur15==2, 1, 0)) # [15, 30)
eventSH$t3 <- with(eventSH, ifelse(actDur15==3, 1, 0)) # [30, 45)
eventSH$t4 <- with(eventSH, ifelse(actDur15==4, 1, 0)) # [45, 60)
eventSH$t5 <- with(eventSH, ifelse(actDur15==5, 1, 0)) # [60, 75)
eventSH$t6 <- with(eventSH, ifelse(actDur15==6, 1, 0)) # [75, 90)
eventSH$t7 <- with(eventSH, ifelse(actDur15==7, 1, 0)) # [90, 105)
eventSH$t8 <- with(eventSH, ifelse(actDur15==8, 1, 0)) # [105, 120)
eventSH$t9 <- with(eventSH, ifelse(actDur15==9, 1, 0)) # [120, 135)
eventSH$t10 <- with(eventSH, ifelse(actDur15==10, 1, 0)) # [135, 150)
eventSH$t11 <- with(eventSH, ifelse(actDur15==11, 1, 0)) # [150, 165)
eventSH$t12 <- with(eventSH, ifelse(actDur15==12, 1, 0)) # [165, 180)
eventSH$t13 <- with(eventSH, ifelse(actDur15==13, 1, 0)) # [180, 195)
eventSH$t14 <- with(eventSH, ifelse(actDur15==14, 1, 0)) # [195, 210)
eventSH$t15 <- with(eventSH, ifelse(actDur15>=15, 1, 0)) # [210, ...)
# SHtimate a binary logit model (m1)
eventSH.m1 <- glm(nextSH~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15,
                  data=eventSH, family=binomial(link=logit))
eventSH.m2 <- glm(nextSH~log(actDur15),
                  data=eventSH, family=binomial(link=logit))
eventSH.m3 <- glm(nextSH~actDur15+I(actDur15^2),
                  data=eventSH, family=binomial(link=logit))
summary(eventSH.m1)
summary(eventSH.m2)
summary(eventSH.m3)

eventSH.m1 <- glm(nextSH~
                    # time indicators
                    -1+t1+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14+t15+
                    # multiple statSH
                    thisH2+thisSH+thisSH+thisSH+thisSH+
                    # time-of-day effects
                    T0708+T1011+T1112+T1213+T1314+T1415+
                    T1516+T1617+T1718+T1819+T1920+T2021+T2122+
                    # epiSHde-level effects
                    tripDur+MODE+
                    # individual-level effects
                    AGE,
                  data=eventSH, family=binomial(link=logit))
summary(eventSH.m1)


  

  
  
  
  
  
  
  
  
  




diary.g1.5dt$move2next <- with(diary.g1.5dt, ifelse(nextActG==0, 0, 1))
sqldf("select perid,actNo,maxAct,thisActG,arrive,depart,actDur5,nextActG,move2next from 'diary.g1.5dt' limit 150")

h <- with(diary.g1.5dt, n.event/n.risk)

# using 10-min intervals
table(diary.g1$actDur10) # to get the highest possible duration
diary.g1.10dt <- survSplit(diary.g1, cut=(1:104), end="actDur10", event="nextActG", start="start")
diary.g1.10dt <- with(diary.g1.10dt, diary.g1.10dt[order(SAMPN, PERNO, actNo, start),])
# diary.g1.10dt$nextActG[diary.g1.10dt$nextActG==0] <- "ST"

diary.g1.10dt$move2next <- with(diary.g1.10dt, ifelse(nextActG==0, 0, 1))
sqldf("select perid,actNo,maxAct,thisActG,arrive,depart,actDur10,nextActG,move2next from 'diary.g1.10dt' limit 150")

dt10 <- survfit( Surv(time=actDur10, event=move2next) ~ 1, conf.type="none", data=diary.g1.10dt)
h <- with(dt10, n.event / n.risk)
LT.dt10 <- cbind(time=dt10$time, risk=dt10$n.risk, move=dt10$n.event, hazard=h, survival=dt10$surv)
LT.dt10

plot(dt10$time, h, type="l", ylab="Estimated hazard probability", xlab="10-min time intervals")
plot(dt10$time, dt10$surv, type="l", ylab="Estimated Survival Probability", xlab="10-min time intervals")


# using 15-min intervals
table(diary.g1$actDur15) # to get the highest possible duration
diary.g1.15dt <- survSplit(diary.g1, cut=(1:70), end="actDur15", event="nextActG", start="start")
diary.g1.15dt <- with(diary.g1.15dt, diary.g1.15dt[order(SAMPN, PERNO, actNo, start),])
# diary.g1.10dt$nextActG[diary.g1.10dt$nextActG==0] <- "ST"

diary.g1.15dt$move2next <- with(diary.g1.15dt, ifelse(nextActG==0, 0, 1))
sqldf("select perid,actNo,maxAct,thisActG,arrive,depart,actDur15,nextActG,move2next from 'diary.g1.15dt' limit 150")

dt15 <- survfit( Surv(time=actDur15, event=move2next) ~ 1, conf.type="none", data=diary.g1.15dt)
h <- with(dt15, n.event / n.risk)
LT.dt15 <- cbind(time=dt15$time, risk=dt15$n.risk, move=dt15$n.event, hazard=h, survival=dt15$surv)
LT.dt15

plot(dt15$time, h, type="l", ylab="Estimated hazard probability", xlab="15-min time intervals")
plot(dt15$time, dt15$surv, type="l", ylab="Estimated Survival Probability", xlab="15-min time intervals")






# =================
event$h_anykids0to4 <- with(event, ave(AGE, SAMPN, FUN=function(x) any(x<=4)))  			  # household with children <= 4 years
event$h_anykids5to14 <- with(event, ave(AGE, SAMPN, FUN=function(x) any(x>=5&x<=14)))		# household with children 5 to 14 years
event$h_anykids15to17 <- with(event, ave(AGE, SAMPN, FUN=function(x) any(x>=15&x<=17)))	# household with children 15 to 17 years
event$h_anykids0to17 <- with(event, ave(AGE, SAMPN, FUN=function(x) any(x<=17)))			  # household with children <= 17 years

# delete TPURP==7 (change of mode/transportation) to ONLY have 'linked trips'
sqldf("select count(*) from diary where actNo==1 and TPURP==7")
sqldf("select count(*) from diary where actNo==maxAct and TPURP==7")
del1 <- sqldf("select perid from diary where TPURP==7 and (actNo==1 or actNo==maxAct)")
diary <- subset(diary, !(diary$perid %in% del1$perid))
diary <- subset(diary, TPURP!=7)



# delete persons who do not end at home
NotAtHome1619 <- sqldf("select perid from diary where actNo==maxAct and TPURP!=1 and TPURP!=2")
diary <- subset(diary, !(diary$perid %in% NotAtHome1619$perid))

# delete persons whose depart!=1619 in the end
NotDepart1619 <- sqldf("select perid from diary where actNo==maxAct and depart!=1619")
diary <- subset(diary, !(diary$perid %in% NotDepart1619$perid))

#
save(diary, file="diary.RData")


