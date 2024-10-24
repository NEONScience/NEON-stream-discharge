#Set DSCXS directory
XSDir<- "shiny-openFlow/src/data"
#getwd("C:/Users/mviggiano/Documents/Github/NEON-stream-discharge-NCC189/shiny-openFlow/src/data")

#setwd("C:/Users/mviggiano/Documents/Github/NEON-stream-discharge-NCC189/shiny-openFlow/src/data")
#CUPEXS raw file
CUPEXS<- readr::read_csv((base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge-NCC189/refs/heads/main/shiny-openFlow/src/data/D04_CUPE_surveyPts_20210408.csv")))
#CUPExs_test<- read_csv("D04_CUPE_surveyPts_20210408.csv")

#siteID<- "CUPE"
surveyDate1<-"20210408"


#Create dataframe of only points associated with mapCode 'Transect_DSC'
CUPE_dsc_pts_xs<-subset(CUPEXS, mapCode=="Transect_DSC")



#Create dataframe for staff gauge points
CUPE_SP_pts<-subset(CUPEXS, mapCode=="Gauge")

#manually select NorthStart and EastStat coordinates
unique(CUPE_dsc_pts_xs$name)
CUPE_dsc_pts_xs_NorthStart<-CUPE_dsc_pts_xs$N[CUPE_dsc_pts_xs$name%in%c("DSC_XS1", "DSC_XS")]
CUPE_dsc_pts_xs_EastStart<-CUPE_dsc_pts_xs$E[CUPE_dsc_pts_xs$name%in%c("DSC_XS1", "DSC_XS")]

#Assigns a raw Distance value to each point relative to the NorthStart and EastStart coordinates.
for(i in 1:(length(CUPE_dsc_pts_xs$name))){
  CUPE_XS1PointN<-CUPE_dsc_pts_xs$N[i]
  CUPE_XS1PointE<-CUPE_dsc_pts_xs$E[i]
  CUPE_dsc_pts_xs$DistanceRaw[i]<-sqrt(((CUPE_XS1PointN-CUPE_dsc_pts_xs_NorthStart)^2)+((CUPE_XS1PointE-CUPE_dsc_pts_xs_EastStart)^2))
}

#Manually select Reference Distance
CUPE_dsc_pts_xs_RefDistance<- CUPE_dsc_pts_xs$DistanceRaw[CUPE_dsc_pts_xs$name=="DSC_RB_PIN"]


#Sets Horizontal adjustment value based on reference point coordinate
CUPE_xs_HorizontalAdjust<-0-CUPE_dsc_pts_xs_RefDistance

#Transform raw distance to adjusted distance based on reference distance point
for( r in 1:(length(CUPE_dsc_pts_xs$name))){
  CUPE_dsc_pts_xs$DistanceAdj[r]<-CUPE_dsc_pts_xs$DistanceRaw[r]+CUPE_xs_HorizontalAdjust
}

#Create datafame of bankfull shots mapped in XS
CUPE_BFshot<- data.frame(matrix(nrow=2, ncol=3))
names(CUPE_BFshot)=c("name","DistanceAdj", "H")
CUPE_BFshot[1,1]<-CUPE_dsc_pts_xs$name[grepl("LBF", CUPE_dsc_pts_xs$name)]
CUPE_BFshot[1,2]<-CUPE_dsc_pts_xs$DistanceAdj[grepl("LBF", CUPE_dsc_pts_xs$name)]
CUPE_BFshot[1,3]<-CUPE_dsc_pts_xs$H[grepl("LBF", CUPE_dsc_pts_xs$name)]
CUPE_BFshot[2,1]<-CUPE_dsc_pts_xs$name[grepl("RBF", CUPE_dsc_pts_xs$name)]
CUPE_BFshot[2,2]<-CUPE_dsc_pts_xs$DistanceAdj[grepl("RBF", CUPE_dsc_pts_xs$name)]
CUPE_BFshot[2,3]<-CUPE_dsc_pts_xs$H[grepl("RBF", CUPE_dsc_pts_xs$name)]

#create csv for bankfull shots
#write.csv(CUPE_BFshot, "C:/Users/mviggiano/Documents/Github/NEON-stream-discharge-NCC189/shiny-openFlow/src/data/D04_CUPE_BF_shots.csv")


####plot DSCXS    #######################
#Sets plot1 settings.  
xAxisTitle1<-list(title="Distance (m)",zeroline=FALSE, range=c(-20,50))
yAxisTitle1<-list(title="H (m)",zeroline=FALSE)
font<-list(size=12,color='black')

(CUPE_XS.plot<-plot_ly(data=CUPE_dsc_pts_xs, x=~DistanceAdj, y=~H, name= 'Distance vs H', type='scatter',
                      mode='markers+lines', text=~name) %>%
  add_trace(data=CUPE_BFshot, x=~DistanceAdj, y=~H, name='Bankfull Field Calls', type='scatter', mode='markers', text=~name, marker=list(size=10,line=list(color='black',width=.5)))%>%
  layout(title =paste(siteID,"_",surveyDate1,sep=""), xaxis=xAxisTitle1, yaxis=yAxisTitle1))
  
print(CUPE_XS.plot)

###Adjusting H for gauge height ######

#Set meter mark where the staff gauge was shot in and name of the staff gauge point:
CUPE_staffGaugeMeterMark<- as.numeric(gsub("M","",gsub("[A-Z]{2}_","",CUPE_SP_pts$name)))
CUPEstaffGauge1Elevation<-CUPE_SP_pts$H

#Convert discharge XS transect point elevation to gauge height (rounded 2 digits)
CUPE_dsc_pts_xs$gaugeHeight<-CUPE_dsc_pts_xs$H -(CUPEstaffGauge1Elevation - CUPE_staffGaugeMeterMark)
CUPE_dsc_pts_xs$gaugeHeight<-round(CUPE_dsc_pts_xs$gaugeHeight, digits=2)

#Converts bankfull elevations to gauge height (rounded to 2 digits)
CUPE_BFshot$gaugeHeight<-CUPE_BFshot$H-(CUPEstaffGauge1Elevation - CUPE_staffGaugeMeterMark)
CUPE_BFshot$gaugeHeight<-round(CUPE_BFshot$gaugeHeight, digits=2)

#assign unique id for each point shot for viewing purposes
CUPE_dsc_pts_xs$ID<-c(1:length(CUPE_dsc_pts_xs$name))


#Sets plot2 settings.  
xAxisTitle2<-list(title="Distance (m)",zeroline=FALSE, range=c(-70,50))
yAxisTitle2<-list(title="Gauge Height  (m)",zeroline=FALSE)
font<-list(size=12,color='black')

#Plot the cross section by distance and gauge height. 
(CUPE_XS.plot<-plot_ly(data = CUPE_dsc_pts_xs, x=~DistanceAdj, y=~gaugeHeight, name='Distance vs Gauge Height', type = 'scatter', 
               mode='markers+lines', text=~name)%>%
    add_trace(data=CUPE_BFshot, x=~DistanceAdj, y=~gaugeHeight, name=~'Bankfull Field Calls', type='scatter', mode='markers', text=~name, marker=list(size=10,line=list(color='black', width=.5))) %>%
    layout(title=paste("CUPE","_", surveyDate1, sep=""), xaxis=xAxisTitle2, yaxis=yAxisTitle2))
print(CUPE_XS.plot)

#write.csv(CUPE_dsc_pts_xs, "C:/Users/mviggiano/Documents/Github/NEON-stream-discharge-NCC189/shiny-openFlow/src/data/D04_CUPE_Transect_DSC.csv")




#######L0 data

# Pull L0 prior parameters and curveIdentification data from restR
# if (!siteID%in%c("TKIN","TKOT")) {
#   DPID <- "DP0.00131.001"
#   ingestTable <- "geo" 
#   siteID_L0pull <- siteID
# }else{
#   DPID <- "DP0.00132.001"
#   ingestTable <- "bat"
#   siteID_L0pull <- "TOOK"
# }
DPID <- "DP0.00131.001"
ingestTable <- "geo" 
siteID_L0pull <- "CUPE"

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

#Subset curveIdentification data to current curveID, then subset the priors data to the survey that corresponds to the current curveID
currCurveID <- "CUPE.2020-2"
curveIdentification <- curveIdentification[curveIdentification$curveID==currCurveID,]
priorParameters <- priorParameters[as.Date(priorParameters$endDate)==as.Date(curveIdentification$controlSurveyEndDateTime),]

#Set the control stage activations (may need manual input based on the # of controls)
c1Gauge<- as.numeric(priorParameters$priorActivationStage[priorParameters$controlNumber==1]) #control 1 stage activation from rating curve
c2Gauge<- as.numeric(priorParameters$priorActivationStage[priorParameters$controlNumber==2]) #control 2 stage activation from rating curve 

baseFlowGauge<-0.4 #baseflow gauge height - manually entered based on the baseflow of the hydrograph
bankfullGauge<-max(CUPE_BFshot$gaugeHeight) #bankfull gauge height - set to the DSC XS bankfull call
# bankfullGauge<-0.72 #bankfull gauge height - manually entered
peakStage<-1.35 #peak un-flagged stage from the continuous data - manually entered
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
bankfullWidth<-abs((CUPE_dsc_pts_xs$DistanceAdj[grepl("RBF",CUPE_dsc_pts_xs$name)])-(CUPE_dsc_pts_xs$DistanceAdj[grepl("LBF",CUPE_dsc_pts_xs$name)]))

#Sets plot2 settings.  
xAxisTitle2<-list(title="Distance (m)",zeroline=FALSE, range=c(-22,22)) #change range as needed
yAxisTitle2<-list(title="Gauge Height  (m)",zeroline=FALSE)
font<-list(size=12,color='black')

#Plot the cross section by distance and gauge height.
(dischargePlot <- CUPE_XS.plot%>%
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
