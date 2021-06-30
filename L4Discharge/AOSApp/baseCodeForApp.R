library(tidyverse)

site <- "HOPB"
startDate <-  "2018-01-01"
endDate <- "2019-01-01"

searchIntervalStartDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = startDate)$startDate)
searchIntervalEndDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = endDate)$endDate)

# Get continuous discharge data from the NEON API
DP4.00133.001 <- neonUtilities::loadByProduct(
  dpID="DP4.00133.001",
  token = Sys.getenv("NEON_PAT"),
  package = "basic",
  check.size = F,
  site = site,
  startdate = searchIntervalStartDate,
  enddate = searchIntervalEndDate
)
gaugeDischargeMeas <- DP4.00133.001$sdrc_gaugeDischargeMeas%>%
  separate(gaugeEventID,c("site","date"),5,remove = F)%>%
  mutate(date=paste0(as.Date(date,format="%Y%m%d")," 12:00:00"))
