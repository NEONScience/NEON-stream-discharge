##################################################################################################################################################################
#
# Title:
# plotGaugeAndDischarge
#
# Authors:
# Kaelin Cawley, Research Scientist, National Ecological Observatory Network 
# email{KCawleyBattelleEcology.org}
# Nick Harrison, Research Scientist, National Ecological Observatory Network 
# email{NHarrison@BattelleEcology.org}
#
# Description: 
# This script calls L1 discharge data in from the NEON Data Portal using the API.  Discharge is re-calculated to account for any changes to depth, velocity, or   
# station values.  Gauge height vs. discharge is then plotted by water year.  Note that discharge measurements typically take about a week to transition from L0 
# to L1 once uploaded onto to Data Portal.  
#
# Parmeters:
# The only input is L1 discharge data.  
#
# Change Log:
# K.Cawley (2019-03-13): original creation.
# N.Harrison (2019-03-14): updated plot formats, added command to delete previous stacked folder, added dataframes per water year, updated documentation throughou   t script.
# N.Harrison (2019-04-10): updated plot formats, added orderID to allStageQ dataframe to help track the order in which measurements were collected.  
#
##################################################################################################################################################################

options(stringsAsFactors = FALSE,scipen = 999)

#Install neonUtilities  (can skip if already installed).
#install.packages("neonUtilities")
#install.packages("plotly")

#Load required packages (install these if not present).
library(neonUtilities)
library(plotly)
source("~/GitHub/NEON-AQU-data-quality/gaugeDischargePlotting/conv.calc.Q.R")

#Enter the data product ID (NEON discharge measurement collection = 20048) and enter the site ID (i.e. POSE) from which you wish to plot stage/Q data.
DPID <- "20048" #Look up the DP number at https://data.neonscience.org/data-product-catalog
siteID <- "HOPB"

#Enter the working directory where stacked data is to be sent by the API. 
setwd<-"C:/Users/nharrison/Documents/Discharge_Data"

#Deletes the old folder (if present) that contains previously stacked data.  
file.remove("filesToStack20048")
unlink('C:/Users/nharrison/Documents/Discharge_Data/filesToStack20048',recursive=TRUE)

#Utilizes the API to get discharge data and unzip it into working directory.  Choose one site at a time or all, if using all it will take a few minutes at least. 
zipsByProduct(dpID=paste0("DP1.",DPID,".001"), 
              site=siteID, 
              package="expanded", 
              check.size=FALSE, 
              savepath = setwd)
stackByTable(filepath = paste0(setwd,"/filesToStack",DPID),
             folder=TRUE)

#Read in the stacked data files.
allTables <- list.files(paste0(setwd,"/filesToStack",DPID,"/stackedFiles"))

for(i in allTables){
  assign(gsub("\\.csv","",i),
         read.delim(paste0(setwd,"/filesToStack",DPID,"/stackedFiles/",i),
                      sep = ","))
}

#Recalculate L1 discharge values using point information.  
dsc_fieldData <- conv.calc.Q(stageData = dsc_fieldData,dischargeData = dsc_individualFieldData)

#Set start and end dates to determine which data gets plotted.  Note that the API call gets all the data available for a given site.  
startDate <- "2014-10-01"
endDate <- "2019-04-10"

#Subsets the data based on the start and end dates specified above.
dsc_fieldDataPlot <- dsc_fieldData[dsc_fieldData$startDate>=startDate&dsc_fieldData$startDate<endDate,]

#Sorts the data by startDate.
dsc_fieldDataPlot <- dsc_fieldDataPlot[order(dsc_fieldDataPlot$startDate),]

#creates a simplified dataframe that shows all stage/Q data and identifies the  water year.  
allStageQ<-data.frame(matrix(nrow=length(dsc_fieldDataPlot$calcQ),ncol=7))
names(allStageQ)=c("site","orderID","collectDate","waterYear","GaugeHeight","Discharge (lps)","Discharge (cfs)")

allStageQ$site<-siteID
allStageQ$orderID<-1:length(allStageQ$collectDate)
allStageQ$collectDate<-dsc_fieldDataPlot$collectDate
allStageQ$collectDate <-  as.POSIXct(allStageQ$collectDate)

for(i in 1:(length(allStageQ$collectDate))){
  
  if(allStageQ$collectDate[i]>="2015-10-01" & allStageQ$collectDate[i]<"2016-10-01"){
    allStageQ$waterYear[i]<-"2016"
  } else {
    if(allStageQ$collectDate[i]>="2016-10-01" & allStageQ$collectDate[i]<"2017-10-01"){
      allStageQ$waterYear[i]<-"2017"
    } else {
      if(allStageQ$collectDate[i]>="2017-10-01" & allStageQ$collectDate[i]<"2018-10-01"){
        allStageQ$waterYear[i]<-"2018"
      } else {
        if(allStageQ$collectDate[i]>="2018-10-01" & allStageQ$collectDate[i]<"2019-10-01"){
          allStageQ$waterYear[i]<-"2019"
        } else {
          if(allStageQ$collectDate[i]>="2019-10-01" & allStageQ$collectDate[i]<"2020-10-01"){
            allStageQ$waterYear[i]<-"2020"
        } else{allStageQ$waterYear[1]<-"NA"}
              }}}}}
  
allStageQ$GaugeHeight<-dsc_fieldDataPlot$streamStage
allStageQ$`Discharge (lps)`<-dsc_fieldDataPlot$calcQ
allStageQ$`Discharge (lps)`<-round(allStageQ$`Discharge (lps)`,digits=0)
allStageQ$`Discharge (cfs)`<-allStageQ$`Discharge (lps)`*0.0353147
allStageQ$`Discharge (cfs)`<-round(allStageQ$`Discharge (cfs)`,digits=1)

#Subsets stage-Q data into individual water year dataframes.
wy15<-subset(allStageQ,allStageQ$waterYear=='2015')
wy16<-subset(allStageQ,allStageQ$waterYear=='2016')
wy17<-subset(allStageQ,allStageQ$waterYear=='2017')
wy18<-subset(allStageQ,allStageQ$waterYear=='2018')
wy19<-subset(allStageQ,allStageQ$waterYear=='2019')
wy20<-subset(allStageQ,allStageQ$waterYear=='2020')

#Plots data by water year.  
tick<-list(size=20,family="Arial, sans-serif",color='black')
font<-list(size=19,color='black')
xAxis<-list(title="Gauge Height (m)", zeroline=FALSE,type="log",titlefont=font, tickfont=tick)
yAxis<-list(title="Discharge (lps)", zeroline=FALSE,type="log",titlefont=font, tickfont=tick)

plot_ly(data=wy19,x=~GaugeHeight, y=~`Discharge (lps)`, name=~waterYear, type='scatter', mode='markers',text=~paste("collectDate:",collectDate,";","orderID:",orderID),marker=list(size=15,line=list(color='black',width=1)))%>%
  add_trace(data=wy18,x= ~GaugeHeight, y=~`Discharge (lps)`,name = ~waterYear,type='scatter', mode='markers', text=~paste("collectDate:",collectDate,";","orderID:",orderID), marker=list(size=15,line=list(color='black',width=1)))%>%
  add_trace(data=wy17,x= ~GaugeHeight, y=~`Discharge (lps)`,name = ~waterYear,type='scatter', mode='markers', text=~paste("collectDate:",collectDate,";","orderID:",orderID), marker=list(size=15,line=list(color='black',width=1)))%>%
  add_trace(data=wy16,x= ~GaugeHeight, y=~`Discharge (lps)`,name = ~waterYear,type='scatter', mode='markers', text=~paste("collectDate:",collectDate,";","orderID:",orderID), marker=list(size=15,line=list(color='black',width=1)))%>%
  add_trace(data=wy15,x= ~GaugeHeight, y=~`Discharge (lps)`,name = ~waterYear,type='scatter', mode='markers', text=~paste("collectDate:",collectDate,";","orderID:",orderID), marker=list(size=15,line=list(color='black',width=1)))%>%
  add_trace(data=wy20,x= ~GaugeHeight, y=~`Discharge (lps)`,name = ~waterYear,type='scatter', mode='markers', text=~paste("collectDate:",collectDate,";","orderID:",orderID), marker=list(size=15,line=list(color='black',width=1)))%>%
   layout(title = paste(siteID,":","Gauge Height (m) vs. Discharge (lps)"), xaxis=xAxis, yaxis=yAxis)

#Calcs median discharge data for all years.
median<-median(allStageQ$`Discharge (lps)`)

wy16RatingIDs<-4
wy16Rating<-allStageQ<-data.frame(matrix(nrow=length(dsc_fieldDataPlot$calcQ),ncol=7))
names(wy16Rating)=c("site","orderID","collectDate","waterYear","GaugeHeight","Discharge (lps)","Discharge (cfs)")

wy16Rating$site<-siteID
wy16Rating$collectDate<-subset(allStageQ$waterYear,allStageQ$orderID=='4')

test<-wy16[wy16$orderID==paste(2,4,6,8),1:7]
