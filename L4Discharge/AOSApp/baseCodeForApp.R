##############################################################################################
#' @title YOUR TITLE

#' @author
#' YOUR NAME \email{EMAIL@battelleecology.org} \cr

#' @description BRIEF DESCRIPTION

#' @return OUTPUT DESCRIPTION

# changelog and author contributions / copyrights
#   YOUR NAME (YYYY-MM-DD)
#     original creation
######################################################################################

# Load packages and set options
library(stageQCurve)
library(tidyverse)
library(neonUtilities)
library(htmlwidgets)
library(plotly)
library(tidyverse)
library(dplyr) 
#install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE, ...)
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
  select(guage_Height, newDate)


#joining gauge discharge vars to continuous summary table
continuousDischarge_sum <- full_join(continuousDischarge_sum, sdrc_gaugeDischargeMeas, by = c("roundDate" = "date")) %>% 
  select(roundDate, meanH, meanQ, meanHUnc, meanURemnUnc,meanLRemnUnc,
         meanUParaUnc,meanLParaUnc,meanLHUnc,meanUHUnc, gaugeHeight,streamDischarge)

#joining guagepressure to  continuoussummary table
continuousDischarge_sum <- full_join(continuousDischarge_sum, sdrc_gaugePressureRelationship, by =c("roundDate" = "newDate")) 
  
#plotting with uncertainty
plott <- plot_ly(data=continuousDischarge_sum)%>%
  
  # Q Uncertainty
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanURemnUnc,name="Q: Remn Unc Top",type='scatter',mode='line',line=list(color='red'),showlegend=T,legendgroup='group1')%>%
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanLRemnUnc,name="Q: Remn Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'red',showlegend=T,legendgroup='group1')%>%
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanUParaUnc,name="Q: Para Unc Top",type='scatter',mode='line',line=list(color='lightpink'),showlegend=T,legendgroup='group1')%>%
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanLParaUnc,name="Q: Para Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightpink',showlegend=T,legendgroup='group1')%>%
  
  # H Uncertainty
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanUHUnc,name="H: Unc Top",type='scatter',mode='line',line=list(color='lightgreen'),yaxis='y2',showlegend=T,legendgroup='group2')%>%
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanLHUnc,name="H: Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightgreen',yaxis='y2',showlegend=T,legendgroup='group2')%>%
  
  # H and Q Series
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanQ, name="Q: Flow Series",type='scatter',mode='lines',line = list(color = 'black'),showlegend=T,legendgroup='group3')%>%
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanH, name="H: Stage Series",type='scatter',mode='lines',line = list(color = 'green'),yaxis='y2',showlegend=T,legendgroup='group4')%>%
  
  # Empirical H and Q
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~streamDischarge,name="Q: Measured", type='scatter', mode='markers',marker = list(color = 'blue',size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group5')%>%
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~guage_Height,name='H: Measured (RC)',type='scatter',mode='markers',yaxis='y2',marker=list(color="purple",size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group6')%>%
  add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~guage_Height,name='H: Measured Guage Pressure',type='scatter',mode='markers',yaxis='y2',marker=list(color="orange",size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group6')%>%
  



layout(title = paste0(site," -- Continuous Discharge Time Series"),
       xaxis=list(tick=14,title="dateTime"),
       yaxis=list(side='left',
                  title='Discharge (lps)',
                  showgrid=FALSE,
                  zeroline=FALSE),
       yaxis2=list(side='right',
                   overlaying="y",
                   title='Stage (m)',
                   showgrid=FALSE,
                   zeroline=FALSE),
       #------
       updatemenus=list(
         list(
           type='buttons',
           buttons=list(
             list(label='linear',
                  method='relayout',
                  args=list(list(yaxis=list(type='linear')))),
             list(label='log',
                  method='relayout',
                  args=list(list(yaxis=list(type='log')))))))
       )#end of layout
#create an html plot
htmlwidgets::saveWidget(as_widget(plott),paste0(site,"_continuousQ_allWYs.html"))

