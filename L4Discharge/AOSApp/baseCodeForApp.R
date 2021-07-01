##############################################################################################
#' @title YOUR TITLE

#' @author
#' YOUR NAME \email{EMAIL@battelleecology.org} \cr

#' @description BRIEF DESCRIPTION

#' @return OUTPUT DESCRIPTION

# changelog and author contributions / copyrights
#   YOUR NAME (YYYY-MM-DD)
#     original creation
##############################################################################################

# Load packages and set options
library(stageQCurve)
library(tidyverse)
library(neonUtilities)
library(htmlwidgets)
devtools::install_github(repo = "NEONScience/NEON-stream-discharge/L4Discharge/stageQCurve", force = TRUE)
library(lubridate, warn.conflicts = FALSE)
options(stringsAsFactors = F)

# Set input variables
site <- "HOPB"
startDate <-  "2018-10-01"
endDate <- "2019-09-30"

# Rating curve data queries need to span an entire water year to ensure we are getting all the appropriate data
searchIntervalStartDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = startDate)$startDate)
searchIntervalEndDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = endDate)$endDate)

# Get continuous discharge data from the NEON API
DP4.00130.001 <- neonUtilities::loadByProduct(
  dpID="DP4.00130.001",
  token = Sys.getenv("NEON_PAT"),
  package = "expanded",
  check.size = F,
  site = site,
  startdate = searchIntervalStartDate,
  enddate = searchIntervalEndDate
)

dsdf

# Get rating curve data from the NEON API
DP4.00133.001 <- neonUtilities::loadByProduct(
  dpID="DP4.00133.001",
  token = Sys.getenv("NEON_PAT"),
  package = "basic",
  check.size = F,
  site = site,
  startdate = searchIntervalStartDate,
  enddate = searchIntervalEndDate
)

#extract date and site in gaugeDischargeMeas from DP4.00133.001
sdrc_gaugeDischargeMeas <- DP4.00133.001$sdrc_gaugeDischargeMeas%>%
  separate(gaugeEventID,c("site","date"),5,remove = F)%>%
  mutate(date=paste0(as.Date(date,format="%Y%m%d")," 20:00:00"))

#extract continuous discharge data and gauge-pressure relationship data from DP4.00130.001
csd_continuousDischarge <- DP4.00130.001$csd_continuousDischarge
sdrc_gaugePressureRelationship <- DP4.00130.001$sdrc_gaugePressureRelationship

# rounding endDate to the nearest 20 minute minute
csd_continuousDischarge$roundDate <- lubridate::round_date(csd_continuousDischarge$endDate, "20 mins")
sdrc_gaugePressureRelationship$newDate <- lubridate::round_date(sdrc_gaugePressureRelationship$endDate, "20 mins")


#creating summary table for variables and  uncertainties to be included
continuousDischarge_sum <- csd_continuousDischarge%>%
  group_by(roundDate)%>%
  summarize(meanQ=mean(maxpostDischarge,na.rm = T),
            meanH=mean(equivalentStage,na.rm = T),
            meanHUnc=mean(stageUnc,na.rm = T),
            meanURemnUnc=mean(withRemnUncQUpper2Std,na.rm = T),
            meanLRemnUnc=mean(withRemnUncQLower2Std,na.rm = T),
            meanUParaUnc=mean(withParaUncQUpper2Std,na.rm = T),
            meanLParaUnc=mean(withParaUncQLower2Std,na.rm = T))%>%
  mutate(meanLHUnc=meanH-meanHUnc,
         meanUHUnc=meanH+meanHUnc)



sdrc_gaugeDischargeMeas$date <- as.POSIXct(sdrc_gaugeDischargeMeas$date)
continuousDischarge_sum$roundDate <- as.POSIXct(continuousDischarge_sum$roundDate)
sdrc_gaugePressureRelationship$newDate <- as.POSIXct(sdrc_gaugePressureRelationship$newDate)

#changing var name guageHeight of guagePressureRelationship
sdrc_gaugePressureRelationship$guage_Height <- sdrc_gaugePressureRelationship$gaugeHeight 
  sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship %>% 
  select(guage_Height)



#joining gauge discharge vars to continuous summary table
continuousDischarge_sum <- full_join(continuousDischarge_sum, sdrc_gaugeDischargeMeas, by =c("roundDate" = "date")) %>% 
  select(roundDate, meanH, meanQ, meanHUnc, meanURemnUnc,meanLRemnUnc,
         meanUParaUnc,meanLParaUnc,meanLHUnc,meanUHUnc, gaugeHeight,streamDischarge)

#joining guagepressure to  continuoussummary table
continuousDischarge_sum <- full_join(continuousDischarge_sum, sdrc_gaugePressureRelationship, by =c("roundDate" = "newDate")) 
  

#ploting with uncertainty
#plot <- plot_ly(data=csd_continuousDischarge_sum)%>%

