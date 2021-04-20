########################################################################################################
#' @title calcQSaltPlateau

#' @author Bobby Hensley email: hensley@battelleecology.org

#' @description R script which calculates discharge from plateau injections of a salt tracer.
#' The governing equation is QCb + qCi = (Q+q)Cp where Q is discharge in the stream, Cb is the background
#' concentration in the stream, q is the injection drip rate, Ci is the injectate concentration, and Cp
#' is the plateau concentration.  This equation can then be re-written to solved for Q = q(Cd-Cp)/(Cp-Cb).
#' Units are L/s for flows and mg/L for concentrations.

########################################### Install and Load ############################################
library(neonUtilities)
library(plyr)
########################################################################################################


############################### Downloads L1 NEON data into R environment ##############################
siteID="COMO"
dateStart="2016-01"
dateEnd="2020-04"
rea<-loadByProduct(dpID="DP1.20190.001", siteID, dateStart, dateEnd,
                   package="expanded", check.size = F)
list2env(rea,.GlobalEnv)
######################################################################################################


################################### Calculates discharge from salt ###################################
#' Convert strings to dates
rea_fieldData$collectDate=as.Date(rea_fieldData$collectDate)
rea_backgroundFieldSaltData$collectDate=as.Date(rea_backgroundFieldSaltData$collectDate)
rea_plateauSampleFieldData$startDate=as.Date(rea_plateauSampleFieldData$startDate)
rea_externalLabDataSalt$startDate=as.Date(rea_externalLabDataSalt$startDate)

#' Removes dates when no injectate concentration was measured
rea_fieldData<-subset(rea_fieldData,injectateSampleCollected!="N")

#' Calculates average salt tracer drip rate
rea_fieldData$avgDripRate=(rea_fieldData$dripRateStart+rea_fieldData$dripRateEnd)/2
#' Converts drip rate in mL/min to L/s
rea_fieldData$avgDripRate=rea_fieldData$avgDripRate/1000/60

#' Pulls lab salt concentrations and appends to appropriate table using shared sample IDs
externalLabDataSalt<-rea_externalLabDataSalt[,c("saltSampleID","finalConcentration")]
externalLabDataSalt<-ddply(externalLabDataSalt,c("saltSampleID"),summarise,finalConcentration=mean(finalConcentration))
fieldData<-rea_fieldData[,c("collectDate","injectateSampleID","avgDripRate")]
fieldData<-merge(fieldData,externalLabDataSalt,by.x="injectateSampleID",by.y="saltSampleID",all.x="TRUE")
fieldData<-fieldData[,c("collectDate","avgDripRate","finalConcentration")]
names(fieldData)<-c("collectDate","avgDripRate","dripConc")
backgroundFieldSaltData<-rea_backgroundFieldSaltData[,c("collectDate","namedLocation","saltBackgroundSampleID")]
backgroundFieldSaltData<-merge(backgroundFieldSaltData,externalLabDataSalt,by.x="saltBackgroundSampleID",by.y="saltSampleID",all.x="TRUE")
plateauSampleFieldData<-rea_plateauSampleFieldData[,c("startDate","namedLocation","saltTracerSampleID")]
plateauSampleFieldData<-merge(plateauSampleFieldData,externalLabDataSalt,by.x="saltTracerSampleID",by.y="saltSampleID",all.x="TRUE")

#' Sets background concentrations to specified value when no measure was taken (this should be site specific)
for (i in 1:nrow(backgroundFieldSaltData))if(is.na(backgroundFieldSaltData[i,4])){backgroundFieldSaltData[i,4]=0}

#' Seperates each station into its own background and plateau concentration files ( multiple measurements get averaged)
sta1_background<-backgroundFieldSaltData[backgroundFieldSaltData$namedLocation %like% "AOS.reaeration.station.01",]
sta1_background<-ddply(sta1_background,c("collectDate"),summarise,backConc=mean(finalConcentration))
names(sta1_background)<-c("collectDate","sta1BackConc")
sta2_background<-backgroundFieldSaltData[backgroundFieldSaltData$namedLocation %like% "AOS.reaeration.station.02",]
sta2_background<-ddply(sta2_background,c("collectDate"),summarise,backConc=mean(finalConcentration))
names(sta2_background)<-c("collectDate","sta2BackConc")
sta3_background<-backgroundFieldSaltData[backgroundFieldSaltData$namedLocation %like% "AOS.reaeration.station.03",]
sta3_background<-ddply(sta3_background,c("collectDate"),summarise,backConc=mean(finalConcentration))
names(sta3_background)<-c("collectDate","sta3BackConc")
sta4_background<-backgroundFieldSaltData[backgroundFieldSaltData$namedLocation %like% "AOS.reaeration.station.04",]
sta4_background<-ddply(sta4_background,c("collectDate"),summarise,backConc=mean(finalConcentration))
names(sta4_background)<-c("collectDate","sta4BackConc")
sta1_plateau<-plateauSampleFieldData[plateauSampleFieldData$namedLocation %like% "AOS.reaeration.station.01",]
sta1_plateau<-ddply(sta1_plateau,c("startDate"),summarise,platConc=mean(finalConcentration))
names(sta1_plateau)<-c("collectDate","sta1PlatConc")
sta2_plateau<-plateauSampleFieldData[plateauSampleFieldData$namedLocation %like% "AOS.reaeration.station.02",]
sta2_plateau<-ddply(sta2_plateau,c("startDate"),summarise,platConc=mean(finalConcentration))
names(sta2_plateau)<-c("collectDate","sta2PlatConc")
sta3_plateau<-plateauSampleFieldData[plateauSampleFieldData$namedLocation %like% "AOS.reaeration.station.03",]
sta3_plateau<-ddply(sta3_plateau,c("startDate"),summarise,platConc=mean(finalConcentration))
names(sta3_plateau)<-c("collectDate","sta3PlatConc")
sta4_plateau<-plateauSampleFieldData[plateauSampleFieldData$namedLocation %like% "AOS.reaeration.station.04",]
sta4_plateau<-ddply(sta4_plateau,c("startDate"),summarise,platConc=mean(finalConcentration))
names(sta4_plateau)<-c("collectDate","sta4PlatConc")

#' Merges all data into one table
allSaltData<-merge(fieldData,sta1_background,by.x="collectDate",by.y="collectDate",all.x=TRUE,all.y=TRUE)
allSaltData<-merge(allSaltData,sta2_background,by.x="collectDate",by.y="collectDate",all.x=TRUE,all.y=TRUE)
allSaltData<-merge(allSaltData,sta3_background,by.x="collectDate",by.y="collectDate",all.x=TRUE,all.y=TRUE)
allSaltData<-merge(allSaltData,sta4_background,by.x="collectDate",by.y="collectDate",all.x=TRUE,all.y=TRUE)
allSaltData<-merge(allSaltData,sta1_plateau,by.x="collectDate",by.y="collectDate",all.x=TRUE,all.y=TRUE)
allSaltData<-merge(allSaltData,sta2_plateau,by.x="collectDate",by.y="collectDate",all.x=TRUE,all.y=TRUE)
allSaltData<-merge(allSaltData,sta3_plateau,by.x="collectDate",by.y="collectDate",all.x=TRUE,all.y=TRUE)
allSaltData<-merge(allSaltData,sta4_plateau,by.x="collectDate",by.y="collectDate",all.x=TRUE,all.y=TRUE)

#' Calculates discharge for each station and date
allSaltData$Q1=(allSaltData$dripConc-allSaltData$sta1PlatConc)/(allSaltData$sta1PlatConc-allSaltData$sta1BackConc)*allSaltData$avgDripRate
allSaltData$Q2=(allSaltData$dripConc-allSaltData$sta2PlatConc)/(allSaltData$sta2PlatConc-allSaltData$sta2BackConc)*allSaltData$avgDripRate
allSaltData$Q3=(allSaltData$dripConc-allSaltData$sta3PlatConc)/(allSaltData$sta3PlatConc-allSaltData$sta3BackConc)*allSaltData$avgDripRate
allSaltData$Q4=(allSaltData$dripConc-allSaltData$sta4PlatConc)/(allSaltData$sta4PlatConc-allSaltData$sta4BackConc)*allSaltData$avgDripRate

#' Calcuates average discharge across all stations for each date
allSaltData$avgQ=(allSaltData$Q1+allSaltData$Q2+allSaltData$Q3+allSaltData$Q4)/4

################################################################################################
