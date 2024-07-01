##############################################################################################
#' @title YOUR TITLE

#' @author
#' Nick Harrison \email{nharrison@battelleecology.org} \cr

#' @description BRIEF DESCRIPTION

#' @return OUTPUT DESCRIPTION

# changelog and author contributions / copyrights
#   Nick Harrison (2022-03-10)
#     original creation
#   Zachary Nickerson (2022-03-15)
#     migrate script from Box to Github, make edits specific to my file-paths
##############################################################################################

#Load packages
library(plotly)
library(calibrate)

#Set site, domain, and survey date
siteID<- "GUIL"
domainID<- "D04"
surveyDate1<- "20200312"
currCurveID <- "GUIL.2020"

#Set local Box directory
#BoxDir <- "C:/Users/nharrison/Box"
BoxDir <- "C:/Users/nickerson/Box"

#Read in survey data
surveyData1<-read.csv(paste0(BoxDir,"/L4-Discharge-Development-And-Testing/allSites/Discharge_XS/",siteID,"/",domainID,"_",siteID,"_surveyPts_",surveyDate1,".csv"))
#Check that the correct headers are in the survey data frame
all(c("name","mapCode","E","N","H")%in%names(surveyData1))# If FALSE, check surveyData1

### create dataframes #######################################################################################################################

#Creates dataframe of all points associated with surveyData1 transect DSC.
unique(surveyData1$mapCode)
# dischargePointsXS1<-subset(surveyData1,mapCode=="Transect_DSC1")
dischargePointsXS1<-subset(surveyData1,mapCode=="Transect_DSC")
dischargePointsXS1<-dischargePointsXS1[order(dischargePointsXS1$E),] #default order by E, can change to N.
rownames(dischargePointsXS1)<-seq(length=nrow(dischargePointsXS1))

#Creates dataframe of staff gauge points
# unique(surveyData1$mapCode)
staffGaugePoints1=subset(surveyData1,surveyData1$mapCode=="Gauge")
# staffGaugePoints1=subset(surveyData1,surveyData1$mapCode=="Gauge_Perm")
staffGaugePoints1<-staffGaugePoints1[order(staffGaugePoints1$E),] #default order by E, can change to N.
rownames(staffGaugePoints1)<-seq(length=nrow(staffGaugePoints1))

### create distance Adj #######################################################################################################################

#Manually select NorthStart and EastStart coordinates.
unique(dischargePointsXS1$name)
# dischargeXS1NorthStart<-dischargePointsXS1$N[dischargePointsXS1$name%in%c("DSC_XS1","DSC_XS")]
# dischargeXS1EastStart<-dischargePointsXS1$E[dischargePointsXS1$name%in%c("DSC_XS1","DSC_XS")]
dischargeXS1NorthStart<-dischargePointsXS1$N[dischargePointsXS1$name=="DSC_LFW"] #If no DSC_XS1 or DSC_XS
dischargeXS1EastStart<-dischargePointsXS1$E[dischargePointsXS1$name=="DSC_LFW"] #If no DSC_XS1 or DSC_XS

#Assigns a raw Distance value to each point relative to the NorthStart and EastStart coordinates.
for(i in 1:(length(dischargePointsXS1$name))){
  dischargeXS1PointN<-dischargePointsXS1$N[i]
  dischargeXS1PointE<-dischargePointsXS1$E[i]
  dischargePointsXS1$DistanceRaw[i]<-sqrt(((dischargeXS1PointN-dischargeXS1NorthStart)^2)+((dischargeXS1PointE-dischargeXS1EastStart)^2))
}

#To manually select ReferenceDistance:
# dischargeXS1ReferenceDistance <- dischargePointsXS1$DistanceRaw[dischargePointsXS1$name=="DSC_RB_PIN"]
# dischargeXS1ReferenceDistance <- dischargePointsXS1$DistanceRaw[dischargePointsXS1$name=="DSC_LB_PIN"]
dischargeXS1ReferenceDistance <- dischargePointsXS1$DistanceRaw[dischargePointsXS1$name=="DSC_LFW"] #When no LB PIN is available

#Sets Horizontal adjustment value based on reference point coordinate.  
dischargeXS1HorizontalAdjust<-0-dischargeXS1ReferenceDistance

#Transforms raw distance to adjusted distance based on reference distance point.
for(i in 1:(length(dischargePointsXS1$name))){
  dischargePointsXS1$DistanceAdj[i]<-dischargePointsXS1$DistanceRaw[i]+dischargeXS1HorizontalAdjust
}

#Creates dataframe of bankfull shots mapped during the survey.  
dsc1BankfullCalls<-data.frame(matrix(nrow=2,ncol=3))
names(dsc1BankfullCalls)=c("name","DistanceAdj","H")
dsc1BankfullCalls[1,1]<-dischargePointsXS1$name[grepl("LBF",dischargePointsXS1$name)]
dsc1BankfullCalls[1,2]<-dischargePointsXS1$DistanceAdj[grepl("LBF",dischargePointsXS1$name)]
dsc1BankfullCalls[1,3]<-dischargePointsXS1$H[grepl("LBF",dischargePointsXS1$name)]
dsc1BankfullCalls[2,1]<-dischargePointsXS1$name[grepl("RBF",dischargePointsXS1$name)]
dsc1BankfullCalls[2,2]<-dischargePointsXS1$DistanceAdj[grepl("RBF",dischargePointsXS1$name)]
dsc1BankfullCalls[2,3]<-dischargePointsXS1$H[grepl("RBF",dischargePointsXS1$name)]

### plot discharge XS #######################################################################################################################

#Sets plot1 settings.  
xAxisTitle1<-list(title="Distance (m)",zeroline=FALSE, range=c(-20,50))
yAxisTitle1<-list(title="H (m)",zeroline=FALSE)
font<-list(size=12,color='black')

#Plot the cross section by distance and gauge height.  
(Q_XS <- plot_ly(data=dischargePointsXS1,x=~DistanceAdj, y=~H, name='Distance vs. H', type='scatter', mode='markers+lines', text=~name)%>%
  add_trace(data=dsc1BankfullCalls,x=~DistanceAdj,y=~H,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
  layout(title = paste(siteID,"_",surveyDate1,sep=""), xaxis=xAxisTitle1, yaxis=yAxisTitle1))

### adjust H for gauge height #######################################################################################################################

#Set meter mark where the staff gauge was shot in and the name of the staff gauge point:

staffGauge1MeterMark<-as.numeric(gsub("M","",gsub(".*\\_","",staffGaugePoints1$name)))[nrow(staffGaugePoints1)]
staffGauge1Elevation <- staffGaugePoints1$H[nrow(staffGaugePoints1)]
 
#Converts discharge XS1 transect point elevations to gauge height (rounded to 2 digits).
dischargePointsXS1$gaugeHeight<-dischargePointsXS1$H - (staffGauge1Elevation - staffGauge1MeterMark)
dischargePointsXS1$gaugeHeight<-round(dischargePointsXS1$gaugeHeight,digits=2)

#Converts bankfull elevations to gauge height (rounded to 2 digits).
dsc1BankfullCalls$gaugeHeight<-dsc1BankfullCalls$H - (staffGauge1Elevation - staffGauge1MeterMark)
dsc1BankfullCalls$gaugeHeight<-round(dsc1BankfullCalls$gaugeHeight,digits=2)
 
#Assigns a unique to each measurement for plot viewing purposes.  
dischargePointsXS1$ID<-c(1:length(dischargePointsXS1$name))
dischargePointsXS1 <- dischargePointsXS1[order(dischargePointsXS1$DistanceAdj),]
 
#Sets plot2 settings.  
xAxisTitle2<-list(title="Distance (m)",zeroline=FALSE, range=c(-70,50))
yAxisTitle2<-list(title="Gauge Height  (m)",zeroline=FALSE)
font<-list(size=12,color='black')
 
#Plot the cross section by distance and gauge height.  
(Q_XS <- plot_ly(data=dischargePointsXS1,x=~DistanceAdj, y=~gaugeHeight, name='Distance vs. Gauge Height', type='scatter', mode='markers+lines', text=~name)%>%
  add_trace(data=dsc1BankfullCalls,x=~DistanceAdj,y=~gaugeHeight,name='Bankfull Field Calls',type='scatter',mode='markers',text=~name,marker=list(size=10,line=list(color='black',width=.5)))%>%
  #add_trace(y= 0,name = 'Gauge Height = 0.00m',mode='lines',line = list(color = 'red', width = 2, dash='dash')) %>%
  layout(title = paste(siteID,"_",surveyDate1,sep=""), xaxis=xAxisTitle2, yaxis=yAxisTitle2))

### set baseflow gauge and axis limits for final plot #######################################################################################################################

# Pull L0 prior parameters and curveIdentification data from restR
if (!siteID%in%c("TKIN","TKOT")) {
  DPID <- "DP0.00131.001"
  ingestTable <- "geo" 
  siteID_L0pull <- siteID
}else{
  DPID <- "DP0.00132.001"
  ingestTable <- "bat"
  siteID_L0pull <- "TOOK"
}
priorParameters <- restR2::get.os.l0.data(
  stack = "prod",
  dpID = DPID,
  ingestTable = paste(ingestTable,"priorParameters_in",sep="_"),
  minStartDate = "2014-01-01T00:00:00.000Z",
  namedLocationName = siteID_L0pull,
  inclDescendants = T
)
curveIdentification <- restR2::get.os.l0.data(
  stack = "prod",
  dpID = DPID,
  ingestTable = paste(ingestTable,"curveIdentification_in",sep="_"),
  minStartDate = "2014-01-01T00:00:00.000Z",
  namedLocationName = siteID_L0pull,
  inclDescendants = T
)
# priorParameters <- restR::get.os.l0.by.namedLocation(
#   stack = "cert",
#   tab = paste0(DPID,":",ingestTable,"_priorParameters_in"),
#   minDate = "2012-01-01",
#   maxDate = as.character(as.Date(Sys.Date())),
#   namedLocationName = siteID_L0pull
# )
# curveIdentification <- restR::get.os.l0.by.namedLocation(
#   stack = "cert",
#   tab = paste0(DPID,":",ingestTable,"_curveIdentification_in"),
#   minDate = "2012-01-01",
#   maxDate = as.character(as.Date(Sys.Date())),
#   namedLocationName = siteID_L0pull
# )
#Subset curveIdentification data to current curveID, then subset the priors data to the survey that corresponds to the current curveID
curveIdentification <- curveIdentification[curveIdentification$curveID==currCurveID,]
priorParameters <- priorParameters[as.Date(priorParameters$endDate)==as.Date(curveIdentification$controlSurveyEndDateTime),]

#Set the control stage activations (may need manual input based on the # of controls)
c1Gauge<- as.numeric(priorParameters$priorActivationStage[priorParameters$controlNumber==1]) #control 1 stage activation from rating curve
c2Gauge<- as.numeric(priorParameters$priorActivationStage[priorParameters$controlNumber==2]) #control 2 stage activation from rating curve 
# baseFlowGauge<-as.numeric(c2Gauge) #baseflow gauge height - set to control 2 stage activation - a site with 3 controls 
# baseFlowGauge<-as.numeric(c1Gauge) #baseflow gauge height - set to control 1 stage activation - a site with 2 controls
baseFlowGauge<-0.67 #baseflow gauge height - manually entered
# bankfullGauge<-max(dsc1BankfullCalls$gaugeHeight) #bankfull gauge height - set to the DSC XS bankfull call
bankfullGauge<-mean(dsc1BankfullCalls$gaugeHeight) #bankfull gauge height - splitting the difference between the 2 XS bankfull calls
# bankfullGauge<-0.72 #bankfull gauge height - manually entered
peakStage<-1.7 #peak un-flagged stage from the continuous data - manually entered
stageRange<-bankfullGauge - baseFlowGauge

#Establish 10% bins between bankfullGauge and baseFlowGauge heights
bankfullMinus5<-bankfullGauge-(bankfullGauge*.05) # -5%
buckets_10per <- seq(baseFlowGauge, bankfullMinus5, ((bankfullMinus5-baseFlowGauge)*.1))

#Create a dataframe of the 10% bins
buckets10<-data.frame(matrix(nrow=length(buckets_10per),ncol=1))
names(buckets10)=c("tenPerBins")
buckets10$tenPerBins <- buckets_10per

print(nrow(buckets10))

#Calculate banfull width based on bankfull field calls
bankfullWidth<-abs((dischargePointsXS1$DistanceAdj[grepl("RBF",dischargePointsXS1$name)])-(dischargePointsXS1$DistanceAdj[grepl("LBF",dischargePointsXS1$name)]))

#Sets plot2 settings.  
xAxisTitle2<-list(title="Distance (m)",zeroline=FALSE, range=c(-22,22)) #change range as needed
yAxisTitle2<-list(title="Gauge Height  (m)",zeroline=FALSE)
font<-list(size=12,color='black')

#Plot the cross section by distance and gauge height.
(dischargePlot <- Q_XS%>%
    #add_trace(y= c1Gauge,name = 'Control 1 Gauge Height',mode='lines',line = list(width = 3, dash='dash')) %>%
    #add_trace(y= c2Gauge,name = 'Control 2 Gauge Height',mode='lines',line = list(width = 3, dash='dash')) %>%
    add_trace(y= baseFlowGauge,name = 'Baseflow Gauge Height',text = baseFlowGauge,mode='lines',line = list(width = 3, dash='dash')) %>%
    add_trace(y= bankfullGauge,name = 'Bankfull Gauge Height',text = bankfullGauge,mode='lines',line = list(width = 3, dash='dash')) %>%
    add_trace(y= peakStage,name = 'Peak Stage',text = bankfullGauge,mode='lines',line = list(width = 3, dash='dash')))
for(i in 1:nrow(buckets10)){
  (dischargePlot <- dischargePlot%>%
     add_trace(y= c(buckets_10per[i]),name = paste0('10% Bin ',i),mode='lines',line = list(width = 1, dash='dash',color='black')))
}
dischargePlot

### fills in siteStageRange csv #######################################################################################################################

#get stream order
streamOrderAll<-read.csv(paste0(BoxDir,"/L4-Discharge-Development-And-Testing/allSites/ratingCurveReviewReport/site_order_bh.csv"))
streamOrder<-streamOrderAll$Stream.Order[streamOrderAll$site==siteID]

#Creates a new dataframe that includes new measurement data.
newData<-data.frame(matrix(nrow=1,ncol=7))
names(newData)=c("Domain","Site", "baseFlowGaugeHeight", "bankFullGaugeHeight", "stageRangeM","bankfullWidthM","streamOrder")
newData$Domain<-domainID
newData$Site<-siteID
newData$baseFlowGaugeHeight<-baseFlowGauge
newData$bankFullGaugeHeight<-bankfullGauge
newData$stageRangeM<-stageRange
newData$bankfullWidthM<-bankfullWidth
newData$streamOrder<-streamOrder

#read in previous siteStageRange.csv
if(file.exists(paste0(BoxDir,"/L4-Discharge-Development-And-Testing/allSites/ratingCurveReviewReport/siteStageRange.csv"))){
  prevData<-read.csv(paste0(BoxDir,"/L4-Discharge-Development-And-Testing/allSites/ratingCurveReviewReport/siteStageRange.csv"))
  prevData <- prevData[prevData$Site!=siteID,]
  allData<-rbind(prevData,newData)
}else{
  allData <- newData
}

### write out csv and plot #######################################################################################################################

#writes out siteStageRange.csv  
write.csv(allData,paste0(BoxDir,"/L4-Discharge-Development-And-Testing/allSites/ratingCurveReviewReport/siteStageRange.csv"),row.names = F)

#writes out buckets10 dataframe  
write.csv(buckets10,paste0(BoxDir,"/L4-Discharge-Development-And-Testing/allSites/Discharge_XS/",siteID,"/",siteID,"_10percentBins.csv"),row.names = F)

#writes out discharge XS plot
wdir<-paste0(BoxDir,"/L4-Discharge-Development-And-Testing/allSites/Discharge_XS/",siteID)
setwd(wdir)
getwd()
htmlwidgets::saveWidget(as_widget(dischargePlot), paste(siteID,"_discharge_XS_",surveyDate1,".html",sep=""))

#Save the global environment as an .rda file.
save.image(file = paste0(siteID,"_DischargeOpt.RData"))

