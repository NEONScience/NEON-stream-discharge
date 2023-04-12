######################################################################################################################## 
#' @title Stage-Discharge Rating Curve Controls Script - D06 - MCDI

#' @author Bobby Hensley \email{hensley@battelleecology.org} \cr 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Nick Harrison \email{nharrison@battelleecology.org} \cr

#' @description This script generates the controls, uncertainties, and priors associated with the creation of a stage-
#' discharge rating curve for McDiffett Creek for water years 2021

#' @return This script produces three .csv files:
#' 'geo_controlInfo_in' contains information on the number of controls and their activations
#' 'geo_controlType_in' Defines the control type and reports parameters and uncertainties for each control
#' 'geo_priorParameters_in' reports the priors calculated in this script

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Kaelin Cawley and Nick Harrison (2019)
#     Generic script created
#   Bobby Hensley (12/09/2021)
#     Modified for MCDI 2021 survey
######################################################################################################################## 

#This reads in data using the API and pulls zip files from the ECS buckets
#load packages
library(neonUtilities)
library(plotly)

siteID <- "MCDI"
domainID <- "D06"
streamMorphoDPID <- "DP4.00131.001"
filepath <- "N:/Science/AQU/Controls/D06_MCDI_20211103"
URIpath <- paste(filepath,"filesToStack00131","stackedFiles",sep = "/")

# #Download data from CERT using restR
L0pull_site <- restR::get.os.l0.by.namedLocation(
  pullType = "startDate",
  stack = "cert",
  tab = 'DP0.00131.001:geo_AISsiteSurveyResultsFile_in',
  minDate = '2021-10-01',
  maxDate = '2021-12-01',
  namedLocationName = 'MCDI')
download.file(L0pull_site$rawDataFilePath,L0pull_site$rawDataFileName,mode="wb")
unzip(paste0("~/",L0pull_site$rawDataFileName),exdir="~")
surveyPtsDF <- read.table("~/D06_MCDI_surveyPts_20211103.csv",
                          sep = ",",
                          header = T,
                          stringsAsFactors = F,
                          encoding = "UTF-8")

#The end date of the geomorphology survey (YYYYMMDD)
surveyDate<-'20211103' 

#The date when this survey applies to the gauging record
surveyActiveDate <- "2020-10-01"

#Stipulate 4-digit site code, underscore, and survey year (ex: HOPB_2017)
surveyID <- "MCDI_2020" 

#Creates dataframe of all points associated with transect DSC.

dischargePointsXS1<-subset(surveyPtsDF,mapCode=="Transect_DSC")
dischargePointsXS1<-dischargePointsXS1[order(dischargePointsXS1$N),]
rownames(dischargePointsXS1)<-seq(length=nrow(dischargePointsXS1))

#Sets plot1 settings.  
xAxisTitle1<-list(title="Easting (m)",zeroline=FALSE)
yAxisTitle1<-list(title="Northing  (m)",zeroline=FALSE)
font<-list(size=12,color='black')

#Plot the cross section by easting and northing data for a sanity check
plot_ly(data=dischargePointsXS1,x=~E, y=~N, name='Easting vs Northing', type='scatter', mode='markers', text=~name)%>%
  layout(title = siteID, xaxis=xAxisTitle1, yaxis=yAxisTitle1)

#Manually select NorthStart and EastStart coordinates
dischargeXS1NorthStart<-dischargePointsXS1$N[dischargePointsXS1$name=="DSC_LB_PIN"]
dischargeXS1EastStart<-dischargePointsXS1$E[dischargePointsXS1$name=="DSC_LB_PIN"]

#Assigns a raw Distance value to each point relative to the NorthStart and EastStart coordinates.
for(i in 1:(length(dischargePointsXS1$name))){
  dischargeXS1PointN<-dischargePointsXS1$N[i]
  dischargeXS1PointE<-dischargePointsXS1$E[i]
  dischargePointsXS1$DistanceRaw[i]<-sqrt(((dischargeXS1PointN-dischargeXS1NorthStart)^2)+((dischargeXS1PointE-dischargeXS1EastStart)^2))
}

#To manually select ReferenceDistance:
dischargeXS1ReferenceDistance <- dischargePointsXS1$DistanceRaw[dischargePointsXS1$name=="DSC_LB_PIN"]

#Sets Horizontal adjustment value based on reference point coordinate.  
dischargeXS1HorizontalAdjust<-0-dischargeXS1ReferenceDistance

#Transforms raw distance to adjusted distance based on reference distance point.
for(i in 1:(length(dischargePointsXS1$name))){
  dischargePointsXS1$DistanceAdj[i]<-dischargePointsXS1$DistanceRaw[i]+dischargeXS1HorizontalAdjust
}

#Calculates the bankfull width
#DSCXS1Bankfull<-abs((dischargePointsXS1$DistanceAdj[grepl("RBF",dischargePointsXS1$name)])-
#                     (dischargePointsXS1$DistanceAdj[grepl("LBF",dischargePointsXS1$name)]))

#Creates dataframe of staff gauge points
staffGaugePoints=subset(surveyPtsDF,surveyPtsDF$mapCode=="Gauge")
staffGaugePoints<-staffGaugePoints[order(staffGaugePoints$N),]
rownames(staffGaugePoints)<-seq(length=nrow(staffGaugePoints))

#Set meter mark where the staff gauge was shot in and the name of the staff gauge point:
#Recorded in field data
staffGaugeMeterMark<-0.55
staffGaugeElevation <- staffGaugePoints$H 

#Converts discharge XS1 transect point elevations to gauge height (rounded to 2 digits).
dischargePointsXS1$gaugeHeight<-dischargePointsXS1$H - (staffGaugeElevation - staffGaugeMeterMark)
dischargePointsXS1$gaugeHeight<-round(dischargePointsXS1$gaugeHeight,digits=2)

#Assigns a unique to each measurement for plot viewing purposes.  
dischargePointsXS1$ID<-c(1:length(dischargePointsXS1$name))

dischargePointsXS1 <- dischargePointsXS1[order(dischargePointsXS1$DistanceAdj),]

#Sets plot2 settings.  
xAxisTitle2<-list(title="Distance (m)",zeroline=FALSE, range=c(0,30))
yAxisTitle2<-list(title="Gauge Height  (m)",zeroline=FALSE)
font<-list(size=12,color='black')

#Plot the cross section by distance and gauge height.  
plot_ly(data=dischargePointsXS1,x=~DistanceAdj, y=~gaugeHeight, name='Distance vs. Gauge Height', type='scatter', mode='markers+lines', text=~name)%>%
  add_trace(y= 0,name = 'Gauge Height = 0.00m',mode='lines',line = list(color = 'red', width = 2, dash='dash')) %>%
  layout(title = siteID, xaxis=xAxisTitle2, yaxis=yAxisTitle2)

#####################################################################################################################################################
#Adjusts the cross section elevations so lowest point is equal to 0.00 meter mark of staff gauge
#####################################################################################################################################################
#Determines the lowest elevation of the discharge cross-section
dischargeXSmin<-min(dischargePointsXS1$H)

#Determines elevation of 0.00 meter mark of staff gage
staffGaugeZero=staffGaugeElevation-staffGaugeMeterMark

#Determines the offset between the lowest elevation and gauge height 
ElevOff<-dischargeXSmin-staffGaugeZero

#Adjusts the cross section elevations by the offset and rounds to 2 decimals
dischargePointsXS1$gaugeHeight<-dischargePointsXS1$gaugeHeight - ElevOff
dischargePointsXS1$gaugeHeight<-round(dischargePointsXS1$gaugeHeight,digits=2)

#Replots the adjusted cross section  
xAxisTitle2<-list(title="Distance (m)",zeroline=FALSE, range=c(0,30))
yAxisTitle2<-list(title="Gauge Height  (m)",zeroline=FALSE)
font<-list(size=12,color='black')
plot_ly(data=dischargePointsXS1,x=~DistanceAdj, y=~gaugeHeight, name='Distance vs. Gauge Height', type='scatter', mode='markers+lines', text=~name)%>%
  add_trace(y= 0,name = 'Gauge Height = 0.00m',mode='lines',line = list(color = 'red', width = 2, dash='dash')) %>%
  layout(title = siteID, xaxis=xAxisTitle2, yaxis=yAxisTitle2)
#####################################################################################################################################################

##### Now create the actual controls to upload... #####

#First, the addition or replacement when controls are activated table "geo_controlInfo_in"
numControls <- 3
geo_controlInfo_in_names <- c("locationID","startDate","endDate","controlNumber","segmentNumber","controlActivationState")
geo_controlInfo_in <- data.frame(matrix(nrow = numControls*numControls, ncol = length(geo_controlInfo_in_names)))
names(geo_controlInfo_in) <- geo_controlInfo_in_names

geo_controlInfo_in$locationID <- siteID
geo_controlInfo_in$startDate <- surveyActiveDate
geo_controlInfo_in$endDate <- surveyActiveDate
geo_controlInfo_in$controlNumber <- rep(1:numControls,numControls)
geo_controlInfo_in <- geo_controlInfo_in[order(geo_controlInfo_in$controlNumber),]
geo_controlInfo_in$segmentNumber <- rep(1:numControls,numControls)

#Known control activation states
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==geo_controlInfo_in$segmentNumber] <- 1
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber>geo_controlInfo_in$segmentNumber] <- 0

#Setting control activation states that are user defined.
#Is control #1 still active when control #2 is activated? 1 = Yes
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==1&geo_controlInfo_in$segmentNumber==2] <- 0

#Is control #1 still active when control #3 is activated? 0 = No
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==1&geo_controlInfo_in$segmentNumber==3] <- 0

#Is control #2 still active when control #3 is activated? 0 = No
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==2&geo_controlInfo_in$segmentNumber==3] <- 1

#Second, create entries for "geo_controlType_in" table for control parameters
geo_controlType_in_names <- c("locationID",
                               "startDate",
                               "endDate",
                               "controlNumber",
                               "hydraulicControlType",
                               "controlLeft",
                               "controlRight",
                               "rectangularWidth",
                               "rectangularWidthUnc",
                               "triangularAngle",
                               "triangularAngleUnc",
                               "parabolaWidth",
                               "parabolaWidthUnc",
                               "parabolaHeight",
                               "parabolaHeightUnc",
                               "orificeArea",
                               "orificeAreaUnc",
                               "channelSlope",
                               "channelSlopeUnc",
                               "manningCoefficient",
                               "manningCoefficientUnc",
                               "stricklerCoefficient",
                               "stricklerCoefficientUnc")
geo_controlType_in <- data.frame(matrix(nrow = numControls, ncol = length(geo_controlType_in_names)))
names(geo_controlType_in) <- geo_controlType_in_names

geo_controlType_in$locationID <- siteID
geo_controlType_in$startDate <- surveyActiveDate
geo_controlType_in$endDate <- surveyActiveDate
geo_controlType_in$controlNumber <- 1:numControls

#Entries for Control #1
geo_controlType_in$hydraulicControlType[1] <- "Rectangular Weir"
geo_controlType_in$controlLeft[1] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_XS48"]
geo_controlType_in$controlRight[1] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_XS63"]
geo_controlType_in$rectangularWidth[1] <- geo_controlType_in$controlRight[1]-geo_controlType_in$controlLeft[1]
geo_controlType_in$rectangularWidthUnc[1] <- 1.0 #Combined uncertainty associated with survey and where actual control begins (1.0 m default)

#Entries for Control #2
geo_controlType_in$hydraulicControlType[2] <- "Rectangular Channel"
geo_controlType_in$controlLeft[2] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_LC_LEW"]
geo_controlType_in$controlRight[2] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_RC_LEW"]
geo_controlType_in$rectangularWidth[2] <- geo_controlType_in$controlRight[2]-geo_controlType_in$controlLeft[2]
geo_controlType_in$rectangularWidthUnc[2] <- 3.0 #Extra uncertainty because there are actually 2 channels

#Entries for Control #3
geo_controlType_in$hydraulicControlType[3] <- "Rectangular Channel"
geo_controlType_in$controlLeft[3] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_LB_PIN"]
geo_controlType_in$controlRight[3] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_LC_LEW"]
geo_controlType_in$rectangularWidth[3] <- geo_controlType_in$controlRight[3]-geo_controlType_in$controlLeft[3]
geo_controlType_in$rectangularWidthUnc[3] <- 1.0 #Combined uncertainty associated with survey and where actual control begins (1.0 m default)

#Slope calculations  
geo_controlType_in$channelSlope[2] <- 0.005 # Entered manually
geo_controlType_in$channelSlopeUnc[2] <- 0.005 # Default slope uncertainty is equal to slope
geo_controlType_in$channelSlope[3] <- 0.005 # Entered manually
geo_controlType_in$channelSlopeUnc[3] <- 0.005 # Default slope uncertainty is equal to slope

#chosen to represent stream conditions with higher roughness above bankfull
geo_controlType_in$manningCoefficient[2] <- 0.05
geo_controlType_in$manningCoefficientUnc[2] <- 0.025 # Default Mannings uncertainty equal 50%
geo_controlType_in$stricklerCoefficient[2] <- 1/geo_controlType_in$manningCoefficient[2]
geo_controlType_in$stricklerCoefficientUnc[2] <- geo_controlType_in$stricklerCoefficient[2]*(geo_controlType_in$manningCoefficientUnc[2]/geo_controlType_in$manningCoefficient[2])

geo_controlType_in$manningCoefficient[3] <- 0.05
geo_controlType_in$manningCoefficientUnc[3] <- 0.025 # Default Mannings uncertainty equal 50%
geo_controlType_in$stricklerCoefficient[3] <- 1/geo_controlType_in$manningCoefficient[3]
geo_controlType_in$stricklerCoefficientUnc[3] <- geo_controlType_in$stricklerCoefficient[3]*(geo_controlType_in$manningCoefficientUnc[3]/geo_controlType_in$manningCoefficient[3])

#Third,  use equations to populate "geo_priorParameters_in" table
geo_priorParameters_in <- data.frame(matrix(nrow = numControls, ncol = 10))
names(geo_priorParameters_in) <- c("locationID",
                                   "startDate",
                                   "endDate",
                                   "controlNumber",
                                   "priorExponent",
                                   "priorExponentUnc",
                                   "priorCoefficient",
                                   "priorCoefficientUnc",
                                   "priorActivationStage",
                                   "priorActivationStageUnc")

#Manually enter activation stages for controls
geo_priorParameters_in$priorActivationStage[1] <- dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC_XS54"]
geo_priorParameters_in$priorActivationStageUnc[1] <- 0.1 # Combined uncertainty associated with survey and actual activation stage (0.1 m default)

geo_priorParameters_in$priorActivationStage[2] <- dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC_XS61"]
geo_priorParameters_in$priorActivationStageUnc[2] <- 0.1 # Combined uncertainty associated with survey and actual activation stage (0.1 m default)

geo_priorParameters_in$priorActivationStage[3] <- dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC_XS76"]
geo_priorParameters_in$priorActivationStageUnc[3] <- 0.1 # Combined uncertainty associated with survey and actual activation stage (0.1 m default)

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
plot(dischargePointsXS1$DistanceAdj,dischargePointsXS1$gaugeHeight,main=paste(siteID,"Discharge XS1: Distance vs. Gauge Height"),xlab="Distance (m)",ylab="Gauge Height (m)")
text(dischargePointsXS1$DistanceAdj,dischargePointsXS1$gaugeHeight,labels=dischargePointsXS1$name,pos=4)
lines(lines(dischargePointsXS1$DistanceAdj,dischargePointsXS1$gaugeHeight,lty=3))
colorsForPlot <- c("blue","red","green","orange","purple")
for(i in 1:numControls){
  x <- c(geo_controlType_in$controlLeft[geo_controlType_in$controlNumber==i],
         geo_controlType_in$controlLeft[geo_controlType_in$controlNumber==i],
         geo_controlType_in$controlRight[geo_controlType_in$controlNumber==i],
         geo_controlType_in$controlRight[geo_controlType_in$controlNumber==i],
         geo_controlType_in$controlLeft[geo_controlType_in$controlNumber==i])
  
  #Determine ymax
  if(i == numControls){
    ymax <- max(dischargePointsXS1$gaugeHeight)
  }else if(any(geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==i&geo_controlInfo_in$segmentNumber>i]==0)){
    overtakingControlNumber <- min(geo_controlInfo_in$segmentNumber[geo_controlInfo_in$controlNumber==i&
                                                                      geo_controlInfo_in$segmentNumber>i&
                                                                      geo_controlInfo_in$controlActivationState==0])
    ymax <- geo_priorParameters_in$priorActivationStage[geo_priorParameters_in$controlNumber == overtakingControlNumber]
  }else{
    ymax <- max(dischargePointsXS1$gaugeHeight)
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
dev.copy2pdf(file = paste0(filepath,"/",siteID,"_siteControls.pdf"), width = 16, height = 9)

#Write out three tables for ingest to GitHub and testing location both
geo_controlInfo_in_output <- c("locationID",
                               "startDate",
                               "endDate",
                               "controlNumber",
                               "segmentNumber",
                               "controlActivationState")
geo_controlInfo_in <- geo_controlInfo_in[,names(geo_controlInfo_in)%in%geo_controlInfo_in_output]
write.csv(geo_controlInfo_in,
          paste0(filepath,"/geo_controlInfo_in.csv"),
          quote = TRUE,
          row.names = FALSE,
          fileEncoding = "UTF-8")

geo_controlType_in_output <- c("locationID",
                               "startDate",
                               "endDate",
                               "controlNumber",
                               "hydraulicControlType",
                               "rectangularWidth",
                               "rectangularWidthUnc",
                               "triangularAngle",
                               "triangularAngleUnc",
                               "parabolaWidth",
                               "parabolaWidthUnc",
                               "parabolaHeight",
                               "parabolaHeightUnc",
                               "orificeArea",
                               "orificeAreaUnc",
                               "channelSlope",
                               "channelSlopeUnc",
                               "manningCoefficient",
                               "manningCoefficientUnc",
                               "stricklerCoefficient",
                               "stricklerCoefficientUnc")
geo_controlType_in <- geo_controlType_in[,names(geo_controlType_in)%in%geo_controlType_in_output]
write.csv(geo_controlType_in,
          paste0(filepath,"/geo_controlType_in.csv"),
          quote = TRUE,
          row.names = FALSE,
          fileEncoding = "UTF-8")

geo_priorParameters_in_output <- c("locationID",
                                   "startDate",
                                   "endDate",
                                   "controlNumber",
                                   "priorExponent",
                                   "priorExponentUnc",
                                   "priorCoefficient",
                                   "priorCoefficientUnc",
                                   "priorActivationStage",
                                   "priorActivationStageUnc")
geo_priorParameters_in <- geo_priorParameters_in[,names(geo_priorParameters_in)%in%geo_priorParameters_in_output]
write.csv(geo_priorParameters_in,
          paste0(filepath,"/geo_priorParameters_in.csv"),
          quote = TRUE,
          row.names = FALSE,
          fileEncoding = "UTF-8")

