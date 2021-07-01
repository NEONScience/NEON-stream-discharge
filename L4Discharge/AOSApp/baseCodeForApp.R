library(tidyverse)
library(neonUtilities)
options(stringsAsFactors = F)
library(tidyverse)
library(htmlwidgets)
library(lubridate, warn.conflicts = FALSE)

site <- "HOPB"
startDate <-  "2018-01-01"
endDate <- "2019-01-01"

#searchIntervalStartDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = startDate)$startDate)
#searchIntervalEndDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = endDate)$endDate)

# Get continuous discharge data from the NEON API
DP4.00130.001 <- neonUtilities::loadByProduct(
  dpID="DP4.00130.001",
  token = Sys.getenv("NEON_PAT"),
  package = "basic",
  check.size = F,
  site = site,
  #startdate = searchIntervalStartDate,
  #enddate = searchIntervalEndDate
  startdate = startDate,
  enddate = endDate
)
# Get stage discharge data from the NEON API
DP4.00133.001 <- neonUtilities::loadByProduct(
  dpID="DP4.00133.001",
  token = Sys.getenv("NEON_PAT"),
  package = "basic",
  check.size = F,
  site = site,
  #startdate = searchIntervalStartDate,
  #enddate = searchIntervalEndDate
  startdate = startDate,
  enddate = endDate
)

# Unpack all data frames into objects
list2env(DP4.00130.001, .GlobalEnv)
list2env(DP4.00133.001, .GlobalEnv)

#extract date an site from gaugeDischarge Data frame
gaugeDischargeMeas <- DP4.00133.001$sdrc_gaugeDischargeMeas%>%
  separate(gaugeEventID,c("site","date"),5,remove = F)%>%
  mutate(date=paste0(as.Date(date,format="%Y%m%d")," 12:00:00"))

# rounding endDate to the nearest 25 minute minute
for (i in 1:length(csd_continuousDischarge$endDate)){
  csd_continuousDischarge$endDate[i] <-  round_date(csd_continuousDischarge$endDate[i], "25 mins")
}
 
#creating summary table for variables and  uncertainties to be included
continuousDischarge_sum <- csd_continuousDischarge%>%
  group_by(endDate)%>%
  summarize(meanQ=mean(maxpostDischarge,na.rm = T),
            meanH=mean(equivalentStage,na.rm = T),
            meanHUnc=mean(stageUnc,na.rm = T),
            meanURemnUnc=mean(withRemnUncQUpper2Std,na.rm = T),
            meanLRemnUnc=mean(withRemnUncQLower2Std,na.rm = T),
            meanUParaUnc=mean(withParaUncQUpper2Std,na.rm = T),
            meanLParaUnc=mean(withParaUncQLower2Std,na.rm = T))%>%
  mutate(meanLHUnc=meanH-meanHUnc,
         meanUHUnc=meanH+meanHUnc)
#filter to remove a=duplicates
continuousDischarge_sum <-  continuousDischarge_sum %>% 
  filter(horizontalPosition ==101)

#joining gauge discharge vars to continuous
continuousDischarge_sum <- full_join(continuousDischarge_sum, gauageDischargeMeas, by =c("endDate" = "date")) %>% 
  select(endDate, meanH, meanQ, meanHUnc, meanURemnUnc,meanLRemnUnc,
         meanUParaUnc,meanLParaUnc,meanLHUnc,meanUHUnc)


#ploting with uncertainty
#reviewPlotly <- plot_ly(data=csd_continuousDischarge_sum)%>%



