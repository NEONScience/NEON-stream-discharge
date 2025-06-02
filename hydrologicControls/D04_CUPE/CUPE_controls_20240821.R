######################################################################################################################## 
#' @title Stage-Discharge Rating Curve Controls Script - D04 - CUPE

#' @author Bobby Hensley \email{hensley@battelleecology.org} \cr 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Nick Harrison \email{nharrison@battelleecology.org} \cr

#' @description This script generates the controls, uncertainties, and priors associated with the creation of a stage-
#' discharge rating curve for Rio Cupeyes for water years 2024-

#' @return This script produces four .csv files:
#' 'geo_controlInfo_in' contains control activation states
#' 'geo_controlType_in' contains the control types, their properties and their uncertainties 
#' 'geo_priorParameters_in' contains the model priors and their uncertainties
#' 'geo_dsc_distAdj_gaugHeight' contains the cross-section of the discharge transect

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Kaelin Cawley and Nick Harrison (2019-08-01)
#     Generic script created.
#   Bobby Hensley (2024-09-04)
#     Script for CUPE 2023-07-13 created.
#   Bobby Hensley (2025-02-03)
#     Modified script to be public facing.
######################################################################################################################## 
############################################ Format the survey points file ############################################

library(neonUtilities)
library(plotly)

siteID <- "CUPE"
domainID <- "D04"
streamMorphoDPID <- "DP4.00131.001"
filepath <- getwd()

#' Read in geomorph data including survey points file from NEON API 
dataFromAPI <- neonUtilities::loadByProduct(streamMorphoDPID,siteID,startdate="2024-08", enddate="2024-08",
              package="expanded",release="current",include.provisional=TRUE,check.size=FALSE)
list2env(dataFromAPI,.GlobalEnv)

#' Specify end date of the geomorphology survey (YYYY-MM-DD)
surveyDate<-'2024-08-21' 

#' Specify date when survey become active (YYYY-MM-DD. Default is start of water year)
surveyActiveDate <- "2023-10-01"

#' Specify survey ID (4-digit site code, underscore, and survey year. ex: HOPB_2017)
surveyID <- "CUPE_2024" 

#' Create dataframe of processed survey points associated with DSC transect for geomorph surveys
if(geo_surveySummary$surveyBoutTypeID=="geomorphology"){
  dsc_surveyPoints<-subset(geo_processedSurveyData,mapCode=="Transect_DSC")}

#' Create dataframe of relative survey points associated with DSC transect for AIS surveys
if(geo_surveySummary$surveyBoutTypeID=="AIS survey"){
  dsc_surveyPoints<-subset(geo_surveyPoints,mapCode=="Transect_DSC")
  dsc_surveyPoints$northing<-dsc_surveyPoints$relativeNorthing
  dsc_surveyPoints$easting<-dsc_surveyPoints$relativeEasting
  dsc_surveyPoints$elevation<-dsc_surveyPoints$relativeHeight}

#' Sorts DSC survey points by Northing
dsc_surveyPoints<-dsc_surveyPoints[order(dsc_surveyPoints$northing),]
rownames(dsc_surveyPoints)<-seq(length=nrow(dsc_surveyPoints)) 

#' Plot plan view of DSC transect
plot_ly(data=dsc_surveyPoints,x=~easting, y=~northing, name='DSC Plan View', type='scatter', mode='markers', text=~surveyPointID)%>%
  layout(title = siteID, xaxis=list(title="Easting (m)",zeroline=FALSE), yaxis=list(title="Northing (m)",zeroline=FALSE))

#' Manually identify the left-most survey point in the DSC transect
dscStart<-"DSC_LB_PIN"

#' Assigns a raw distance value to each point relative to the left-most survey point in the DSC transect.
for(i in 1:(length(dsc_surveyPoints$surveyPointID))){
  pointNorth<-dsc_surveyPoints$northing[i]
  pointEast<-dsc_surveyPoints$easting[i]
  dsc_surveyPoints$distanceRaw[i]<-sqrt(((pointNorth-dsc_surveyPoints$northing[dsc_surveyPoints$surveyPointID==dscStart])^2)
                                        +((pointEast-dsc_surveyPoints$easting[dsc_surveyPoints$surveyPointID==dscStart])^2))}

#' Manually identify the left pin to use as the reference distance 
dscReference <- dsc_surveyPoints$distanceRaw[dsc_surveyPoints$surveyPointID=="DSC_LB_PIN"]

#' Transforms raw distance to adjusted distance based on reference distance
for(i in 1:(length(dsc_surveyPoints$surveyPointID))){
  dsc_surveyPoints$distanceAdj[i]<-dsc_surveyPoints$distanceRaw[i]-dscReference}
dsc_surveyPoints <- dsc_surveyPoints[order(dsc_surveyPoints$distanceAdj),]

#Creates dataframe of staff gauge points
if(geo_surveySummary$surveyBoutTypeID=="geomorphology"){
  staffGaugePoints=subset(geo_processedSurveyData,geo_processedSurveyData$mapCode=="Gauge")}
if(geo_surveySummary$surveyBoutTypeID=="AIS survey"){
  staffGaugePoints=subset(geo_surveyPoints,geo_surveyPoints$mapCode=="Gauge")
  staffGaugePoints$elevation<-staffGaugePoints$relativeHeight}

#' Manually identify staff gauge point to use (some surveys may have multiple)
staffGaugeElevation <- staffGaugePoints$elevation[grepl("SP_0.60M_NEW",staffGaugePoints$surveyPointID)]  

#' Manually enter the staff gauge reading of the point used above (found in surveyPointID name)
staffGaugeMeterMark<-0.60

#' Converts elevations of survey points in DSC transect to gauge height (rounded to 2 digits).
dsc_surveyPoints$gaugeHeight<-round(dsc_surveyPoints$elevation - (staffGaugeElevation - staffGaugeMeterMark),digits=2)

#' Adjusts the cross section elevations so lowest point is equal to 0.00 meter mark of staff gauge
# ElevOff<-min(dsc_surveyPoints$elevation)-(staffGaugeElevation-staffGaugeMeterMark) #Determines the offset between the lowest elevation and elevation of 0.0 on staff gage 
# dsc_surveyPoints$gaugeHeight<-round(dsc_surveyPoints$gaugeHeight - ElevOff,digits=2) #Adjusts the cross section elevations by the offset

#' Assigns a unique to each measurement for plot viewing purposes.  
dsc_surveyPoints$ID<-c(1:length(dsc_surveyPoints$name))

#' Plot the cross section by distance and gauge height.  
plot_ly(data=dsc_surveyPoints,x=~distanceAdj, y=~gaugeHeight, name='DSC Cross-section View', type='scatter', mode='markers+lines', text=~surveyPointID)%>%
  add_trace(y= 0,name = 'Gauge Height = 0.00m',mode='lines',line = list(color = 'red', width = 2, dash='dash')) %>%
  layout(title = siteID, xaxis=list(title="Distance (m)",zeroline=FALSE), yaxis=list(title="Gauge Height  (m)",zeroline=FALSE))

#' Adds survey date and creates geo_dsc_distAdj_gaugeHeight Table
dsc_surveyPoints$surveyEndDate<-surveyDate 
geo_dsc_distAdj_gaugHeight<-dsc_surveyPoints[,c("siteID","surveyEndDate","surveyPointID","distanceAdj","gaugeHeight")]


########################################################################################################################
####################################### Now create the BaM control input tables #######################################

#' Specify the number of controls (Most common will be 3)
numControls <- 3


#' Generate "geo_controlInfo_in" Table
geo_controlInfo_in_names <- c("locationID","startDate","endDate","controlNumber","segmentNumber","controlActivationState")
geo_controlInfo_in <- data.frame(matrix(nrow = numControls*numControls, ncol = length(geo_controlInfo_in_names)))
names(geo_controlInfo_in) <- geo_controlInfo_in_names
geo_controlInfo_in$locationID <- siteID
geo_controlInfo_in$startDate <- surveyActiveDate
geo_controlInfo_in$endDate <- surveyActiveDate
geo_controlInfo_in$controlNumber <- rep(1:numControls,numControls)
geo_controlInfo_in <- geo_controlInfo_in[order(geo_controlInfo_in$controlNumber),]
geo_controlInfo_in$segmentNumber <- rep(1:numControls,numControls)
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==geo_controlInfo_in$segmentNumber] <- 1
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber>geo_controlInfo_in$segmentNumber] <- 0
#' Is control #1 still active when control #2 is activated (0 = No, 1 = Yes)?
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==1&geo_controlInfo_in$segmentNumber==2] <- 0
#' Is control #1 still active when control #3 is activated (0 = No, 1 = Yes)?
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==1&geo_controlInfo_in$segmentNumber==3] <- 0
#' Is control #2 still active when control #3 is activated (0 = No, 1 = Yes)?
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==2&geo_controlInfo_in$segmentNumber==3] <- 1


#' Generate "geo_controlType_in" Table
geo_controlType_in_names <- c("locationID","startDate","endDate","controlNumber","hydraulicControlType","controlLeft",
    "controlRight","rectangularWidth","rectangularWidthUnc","triangularAngle","triangularAngleUnc","parabolaWidth",
    "parabolaWidthUnc","parabolaHeight","parabolaHeightUnc","orificeArea","orificeAreaUnc","channelSlope","channelSlopeUnc",
    "manningCoefficient","manningCoefficientUnc","stricklerCoefficient","stricklerCoefficientUnc")
geo_controlType_in <- data.frame(matrix(nrow = numControls, ncol = length(geo_controlType_in_names)))
names(geo_controlType_in) <- geo_controlType_in_names
geo_controlType_in$locationID <- siteID
geo_controlType_in$startDate <- surveyActiveDate
geo_controlType_in$endDate <- surveyActiveDate
geo_controlType_in$controlNumber <- 1:numControls

#' Manually identify left and right start points of each control.
#' In most cases NEON uses a default of 1.0m for the control width uncertainty 
geo_controlType_in$hydraulicControlType[1] <- "Rectangular Weir"
geo_controlType_in$controlLeft[1] <- dsc_surveyPoints$distanceAdj[dsc_surveyPoints$surveyPointID == "DSC_LEW"]
geo_controlType_in$controlRight[1] <- dsc_surveyPoints$distanceAdj[dsc_surveyPoints$surveyPointID == "DSC_XS21"]
geo_controlType_in$rectangularWidth[1] <- geo_controlType_in$controlRight[1]-geo_controlType_in$controlLeft[1]
geo_controlType_in$rectangularWidthUnc[1] <- 1.0 
geo_controlType_in$hydraulicControlType[2] <- "Rectangular Channel"
geo_controlType_in$controlLeft[2] <- dsc_surveyPoints$distanceAdj[dsc_surveyPoints$surveyPointID == "DSC_LEW"]
geo_controlType_in$controlRight[2] <- dsc_surveyPoints$distanceAdj[dsc_surveyPoints$surveyPointID == "DSC_XS21"]
geo_controlType_in$rectangularWidth[2] <- geo_controlType_in$controlRight[2]-geo_controlType_in$controlLeft[2]
geo_controlType_in$rectangularWidthUnc[2] <- 1.0 
geo_controlType_in$hydraulicControlType[3] <- "Rectangular Channel"
geo_controlType_in$controlLeft[3] <- dsc_surveyPoints$distanceAdj[dsc_surveyPoints$surveyPointID == "DSC_XS21"]
geo_controlType_in$controlRight[3] <- dsc_surveyPoints$distanceAdj[dsc_surveyPoints$surveyPointID == "DSC_XS13"]
geo_controlType_in$rectangularWidth[3] <- geo_controlType_in$controlRight[3]-geo_controlType_in$controlLeft[3]
geo_controlType_in$rectangularWidthUnc[3] <- 1.0 

#Slope calculations
#No wetted edge or thalweg shots in AIS survey. Used slope from previous geomorph survey.
geo_controlType_in$channelSlope[2] <- 0.03
geo_controlType_in$channelSlopeUnc[2] <- 0.03 #Default slope uncertainty is equal to slope
geo_controlType_in$channelSlope[3] <- 0.03
geo_controlType_in$channelSlopeUnc[3] <- 0.03 #Default slope uncertainty is equal to slope

#' Specify Manning coefficient and uncertainty
geo_controlType_in$manningCoefficient[2] <- 0.05 # Cobble stream with some pools 
geo_controlType_in$manningCoefficientUnc[2] <- 0.025 # Default Mannings uncertainty equal 50%
geo_controlType_in$manningCoefficient[3] <- 0.1 # Trees and some brush 
geo_controlType_in$manningCoefficientUnc[3] <- 0.05 # Default Mannings uncertainty equal 50%

#' Calculates Strickler coefficient and uncertainty
geo_controlType_in$stricklerCoefficient[2] <- 1/geo_controlType_in$manningCoefficient[2]
geo_controlType_in$stricklerCoefficientUnc[2] <- geo_controlType_in$stricklerCoefficient[2]*(geo_controlType_in$manningCoefficientUnc[2]/geo_controlType_in$manningCoefficient[2])
geo_controlType_in$stricklerCoefficient[3] <- 1/geo_controlType_in$manningCoefficient[3]
geo_controlType_in$stricklerCoefficientUnc[3] <- geo_controlType_in$stricklerCoefficient[3]*(geo_controlType_in$manningCoefficientUnc[3]/geo_controlType_in$manningCoefficient[3])

#' Generate "geo_priorParameters_in" Table
geo_priorParameters_in <- data.frame(matrix(nrow = numControls, ncol = 10))
names(geo_priorParameters_in) <- c("locationID","startDate","endDate","controlNumber","priorExponent",
  "priorExponentUnc","priorCoefficient","priorCoefficientUnc","priorActivationStage","priorActivationStageUnc")

#' Manually identify activation stage for each control
#' In most cases NEON uses a default of 0.1m for the activation stage uncertainty 
geo_priorParameters_in$priorActivationStage[1] <- dsc_surveyPoints$gaugeHeight[dsc_surveyPoints$surveyPointID == "DSC_THL"]
geo_priorParameters_in$priorActivationStageUnc[1] <- 0.1
geo_priorParameters_in$priorActivationStage[2] <- dsc_surveyPoints$gaugeHeight[dsc_surveyPoints$surveyPointID == "DSC_XS25"]
geo_priorParameters_in$priorActivationStageUnc[2] <- 0.1 
geo_priorParameters_in$priorActivationStage[3] <- dsc_surveyPoints$gaugeHeight[dsc_surveyPoints$surveyPointID == "DSC_REW"]
geo_priorParameters_in$priorActivationStageUnc[3] <- 0.1 
geo_priorParameters_in$locationID <- siteID
geo_priorParameters_in$startDate <- surveyActiveDate
geo_priorParameters_in$endDate <- surveyActiveDate

#Loop through to calculate exponent and coefficients
for(i in 1:numControls){
  geo_priorParameters_in$controlNumber[i] <- i
  if(!geo_controlType_in$hydraulicControlType[i]%in%c("Rectangular Weir","Rectangular Channel")){
    stop("Control type not found in the list.")
  }
  switch(geo_controlType_in$hydraulicControlType[i],
         "Rectangular Weir" = {
           Cr <- 0.4
           Cr_unc <- 0.1
           Bw <- geo_controlType_in$rectangularWidth[geo_controlType_in$controlNumber == i] #meters wide
           Bw_unc <- geo_controlType_in$rectangularWidthUnc[geo_controlType_in$controlNumber == i]
           g <- 9.81 #metersPerSecondSquared
           g_unc <- 0.01
           geo_priorParameters_in$priorCoefficient[i] <- Cr * Bw * (2*g)**(1/2)
           geo_priorParameters_in$priorCoefficientUnc[i] <- geo_priorParameters_in$priorCoefficient[i] * ((Cr_unc/Cr)**2+(Bw_unc/Bw)**2)**(1/2) + 0.5*(g_unc)/(2*g)
           
           geo_priorParameters_in$priorExponent[i] <- 1.5 #Recommended by BaM
           geo_priorParameters_in$priorExponentUnc[i] <- 0.05 #Recommended by BaM
         },
         "Rectangular Channel" = {
           Ks <- geo_controlType_in$stricklerCoefficient[geo_controlType_in$controlNumber == i]
           Ks_unc <- geo_controlType_in$stricklerCoefficientUnc[geo_controlType_in$controlNumber == i]
           Bw <- geo_controlType_in$rectangularWidth[geo_controlType_in$controlNumber == i] #meters wide
           Bw_unc <- geo_controlType_in$rectangularWidthUnc[geo_controlType_in$controlNumber == i]
           slope <- geo_controlType_in$channelSlope[geo_controlType_in$controlNumber == i]
           slope_unc <- geo_controlType_in$channelSlopeUnc[geo_controlType_in$controlNumber == i]
           geo_priorParameters_in$priorCoefficient[i] <- Ks * Bw * (slope)**(1/2)
           geo_priorParameters_in$priorCoefficientUnc[i] <- geo_priorParameters_in$priorCoefficient[i] * ((Ks_unc/Ks)**2+(Bw_unc/Bw)**2)**(1/2) + 0.5*(slope_unc)/(slope)
           
           geo_priorParameters_in$priorExponent[i] <- 1.67 #Recommended by BaM
           geo_priorParameters_in$priorExponentUnc[i] <- 0.05 #Recommended by BaM
         }
  )
}

#Plot controls to double check
invisible(dev.new(noRStudioGD = TRUE))
plot(dsc_surveyPoints$distanceAdj,dsc_surveyPoints$gaugeHeight,main=paste(surveyID," Hydrologic Controls"),xlab="Distance (m)",ylab="Gauge Height (m)")
text(dsc_surveyPoints$distanceAdj,dsc_surveyPoints$gaugeHeight,labels=dsc_surveyPoints$name,pos=4)
lines(lines(dsc_surveyPoints$distanceAdj,dsc_surveyPoints$gaugeHeight,lty=3))
colorsForPlot <- c("blue","red","green","orange","purple")
for(i in 1:numControls){
  x <- c(geo_controlType_in$controlLeft[geo_controlType_in$controlNumber==i],
         geo_controlType_in$controlLeft[geo_controlType_in$controlNumber==i],
         geo_controlType_in$controlRight[geo_controlType_in$controlNumber==i],
         geo_controlType_in$controlRight[geo_controlType_in$controlNumber==i],
         geo_controlType_in$controlLeft[geo_controlType_in$controlNumber==i])
  
  #Determine ymax
  if(i == numControls){
    ymax <- max(dsc_surveyPoints$gaugeHeight)
  }else if(any(geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==i&geo_controlInfo_in$segmentNumber>i]==0)){
    overtakingControlNumber <- min(geo_controlInfo_in$segmentNumber[geo_controlInfo_in$controlNumber==i&
                                                                      geo_controlInfo_in$segmentNumber>i&
                                                                      geo_controlInfo_in$controlActivationState==0])
    ymax <- geo_priorParameters_in$priorActivationStage[geo_priorParameters_in$controlNumber == overtakingControlNumber]
  }else{
    ymax <- max(dsc_surveyPoints$gaugeHeight)
  }
  
  #Determine ymin if control overtakes others
  if(i == 1){
    ymin <- geo_priorParameters_in$priorActivationStage[geo_priorParameters_in$controlNumber==i]
  }else if(sum(geo_controlInfo_in$controlActivationState[geo_controlInfo_in$segmentNumber==i&geo_controlInfo_in$controlNumber<i])<(i-1)){
    #ymin <- min(geo_priorParameters_in$priorActivationStage[geo_priorParameters_in$controlNumber<i])
    ymin <- geo_priorParameters_in$priorActivationStage[geo_priorParameters_in$controlNumber==i]
  }else{
    ymin <- geo_priorParameters_in$priorActivationStage[geo_priorParameters_in$controlNumber==i]
  }
  
  y <- c(ymax,
         ymin,
         ymin,
         ymax,
         ymax)
  polygon(x,y, col = adjustcolor(colorsForPlot[i],alpha.f = 0.5))
}
dev.copy2pdf(file = paste(surveyID,"_siteControls.pdf"), width = 16, height = 9)



########################################################################################################################
##################################### Write output tables into working directory ######################################
write.csv(geo_dsc_distAdj_gaugHeight, file=(paste("geo_dsc_distAdj_gaugHeight_", surveyID, ".csv",sep = "")),
          quote = TRUE,row.names = FALSE,fileEncoding = "UTF-8")
write.csv(geo_controlInfo_in, file=(paste("geo_controlInfo_in_", siteID, ".csv",sep = "")),
          quote = TRUE,row.names = FALSE,fileEncoding = "UTF-8")
write.csv(geo_controlType_in, file=(paste("geo_controlType_in_", siteID, ".csv",sep = "")),
          quote = TRUE,row.names = FALSE,fileEncoding = "UTF-8")          
write.csv(geo_priorParameters_in, file=(paste("geo_priorParameters_in_", siteID, ".csv",sep = "")),
          quote = TRUE,row.names = FALSE,fileEncoding = "UTF-8") 


