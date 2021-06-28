########################################################################################################
#' @title calcQSaltPulse

#' @author Bobby Hensley email: hensley@battelleecology.org

#' @description R script which calculates discharge from pulse injections of a salt tracer.


########################################### Install and Load ############################################
library(neonUtilities)
library(plyr)
########################################################################################################


############################### Downloads L1 NEON data into R environment ##############################
siteID="ARIK"
dateStart="2016-01"
dateEnd="2020-04"
rea<-loadByProduct(dpID="DP1.20193.001", siteID, dateStart, dateEnd,
                   package="expanded", check.size = F)
list2env(rea,.GlobalEnv)
######################################################################################################


################################### Calculates discharge from salt ###################################
#' Convert strings to dates
sbd_fieldData$startDate=as.Date(sbd_fieldData$startDate)
sbd_backgroundFieldCondData$startDate=as.Date(sbd_backgroundFieldCondData$startDate)
sbd_conductivityFieldData$startDate=as.Date(sbd_conductivityFieldData$startDate)


#' Creates a vector of dates of all injections
injDates<-unique(sbd_fieldData$startDate)


################################################################################################
