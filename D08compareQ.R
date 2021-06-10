########################################################################################################
#' @title D08compareQ

#' @author Bobby Hensley email: hensley@battelleecology.org

#' @description R script which downloads and compares USGS and NEON discharge data for Domain 08.
#'
#' Updated 5/6/2021

########################################################################################################
library(dataRetrieval)
library(neonUtilities)
library(lubridate)
library(plotly)

# BLWA ##################################################################################################
#' Loads USGS data from csv file
siteNumber <- "02466030"
parameterCd <- "00060"  # Discharge in cfs
startDate <- "2018-10-01" 
endDate <- "2021-05-01" 
usgsData<-dataRetrieval::readNWISuv(siteNumber, parameterCd, startDate, endDate)
#' Convertes discharge from cfs to cms
usgsData$dischargeCMS=usgsData$X_00060_00000*0.0283

#' Loads CSV on processed ADCP measurments
# neonData<-read.csv(file="neonBLWA.csv")
# neonData$collectDate<-as.POSIXct(neonData$collectDate,format="%Y-%m-%d %H:%M:%S")


#' Pulls from NEON API
neonData<-neonUtilities::loadByProduct(dpID="DP1.20048.001", site="BLWA", startdate="2012-01", enddate="2021-03", check.size=FALSE)
list2env(neonData,.GlobalEnv)
neonData<-dsc_fieldDataADCP

#' Merges USGS and NEON data using timestamps
neonData$startDate<-lubridate::round_date(neonData$startDate,unit="60 minute") 
mergedData<-merge(neonData,usgsData,by.x="startDate",by.y="dateTime",all.x=T,all.y=F)

#' Fits linear regression
fit<-lm(totalDischarge~dischargeCMS, data=mergedData)
summary(fit)

#' Calculates residuals
mergedData$residuals=mergedData$totalDischarge-mergedData$dischargeCMS

#' Generates Plots
plot_ly(data=usgsData, x=~dateTime,y=~dischargeCMS,name="USGS",type="scatter",mode="lines",line=list(color="black",width=1))%>%
  add_trace(data=neonData, x=~startDate,y=~totalDischarge,name="NEON",type="scatter",mode="markers",marker=list(color="blue",size=20),line=list(width=0))%>%
  layout(title ="D08.BLWA.DP01.20048",titlefont=list(size=30), xaxis = list(title = "Date",titlefont=list(size=30),tickfont=list(size=30)),yaxis = list (title = "Q (m3/s)",titlefont=list(size=30),tickfont=list(size=30)),margin=list(l=50, r=50, t=100, b=100, pad=4))

plot_ly(data=mergedData, x=~dischargeCMS,y=~totalDischarge,name="observations",type="scatter",mode="markers",marker=list(color="blue",size=20),line=list(width=0))%>%
  #add_lines(data=mergedData, x=~dischargeCMS,y=fitted(fit),name="linear model",line=list(color="black",width=2),marker=list(color="black",size=1))%>%
  layout(title ="D08.BLWA.DP01.20048",titlefont=list(size=30), xaxis = list(title = "USGS Q (m3/s)",titlefont=list(size=30),tickfont=list(size=30)),yaxis = list (title = "NEON Q (m3/s)",titlefont=list(size=30),tickfont=list(size=30)),margin=list(l=50, r=50, t=100, b=100, pad=4))

plot_ly(data=mergedData, x=~residuals,type="histogram", marker=list(color="blue"))%>%
  layout(title ="D08.BLWA.DP01.20048",titlefont=list(size=30), xaxis = list(title = "residual Q (m3/s)",titlefont=list(size=30),tickfont=list(size=30)),yaxis = list (title = "Observations",titlefont=list(size=30),tickfont=list(size=30)),margin=list(l=50, r=50, t=100, b=100, pad=4))

########################################################################################################



# TOMB ##################################################################################################
#' Loads USGS data from csv file
siteNumber <- "02469761"
parameterCd <- "00060"  # Discharge in cfs
startDate <- "2018-06-01" 
endDate <- "2021-05-01" 
usgsData<-dataRetrieval::readNWISuv(siteNumber, parameterCd, startDate, endDate)
#' Convertes discharge from cfs to cms
usgsData$dischargeCMS=usgsData$X_00060_00000*0.0283

#' Loads CSV on processed ADCP measurments
# neonData<-read.csv(file="neonTOMB.csv")
# neonData$collectDate<-as.POSIXct(neonData$collectDate,format="%Y-%m-%d %H:%M:%S")

#' Pulls from NEON API
neonData<-neonUtilities::loadByProduct(dpID="DP1.20048.001", site="TOMB", startdate="2012-01", enddate="2021-03", check.size=FALSE)
list2env(neonData,.GlobalEnv)
neonData<-dsc_fieldDataADCP

#' Merges USGS and NEON data using timestamps
neonData$startDate<-lubridate::round_date(neonData$startDate,unit="60 minute") 
mergedData<-merge(neonData,usgsData,by.x="startDate",by.y="dateTime",all.x=T,all.y=F)

#' Fits linear regression
fit<-lm(totalDischarge~dischargeCMS, data=mergedData)
summary(fit)

#' Calculates residuals
mergedData$residuals=mergedData$totalDischarge-mergedData$dischargeCMS

#' Generates Plots
plot_ly(data=usgsData, x=~dateTime,y=~dischargeCMS,name="USGS",type="scatter",mode="lines",line=list(color="black",width=1))%>%
  add_trace(data=neonData, x=~collectDate,y=~totalDischarge,name="NEON",type="scatter",mode="markers",marker=list(color="red",size=20),line=list(width=0))%>%
  layout(title ="D08.TOMB.DP01.20048",titlefont=list(size=30), xaxis = list(title = "Date",titlefont=list(size=30),tickfont=list(size=30)),yaxis = list (title = "Q (m3/s)",titlefont=list(size=30),tickfont=list(size=30)),margin=list(l=50, r=50, t=100, b=100, pad=4))

plot_ly(data=mergedData, x=~dischargeCMS,y=~totalDischarge,name="observations",type="scatter",mode="markers",marker=list(color="red",size=20),line=list(width=0))%>%
  add_lines(data=mergedData, x=~dischargeCMS,y=fitted(fit),name="linear model",line=list(color="black",width=2),marker=list(color="black",size=1))%>%
  layout(title ="D08.TOMB.DP01.20048",titlefont=list(size=30), xaxis = list(title = "USGS Q (m3/s)",titlefont=list(size=30),tickfont=list(size=30)),yaxis = list (title = "NEON Q (m3/s)",titlefont=list(size=30),tickfont=list(size=30)),margin=list(l=50, r=50, t=100, b=100, pad=4))

plot_ly(data=mergedData, x=~residuals, marker=list(color="red"))%>%
  layout(title ="D08.TOMB.DP01.20048",titlefont=list(size=30), xaxis = list(title = "residual Q (m3/s)",titlefont=list(size=30),tickfont=list(size=30)),yaxis = list (title = "Observations",titlefont=list(size=30),tickfont=list(size=30)),margin=list(l=50, r=50, t=100, b=100, pad=4))

########################################################################################################