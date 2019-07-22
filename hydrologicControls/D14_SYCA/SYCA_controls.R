
#This reads in data using the API and pulls zip files from the ECS buckets
#load packages
library(neonUtilities)
library(plotly)

siteID <- "SYCA"
domainID <- "D14"
streamMorphoDPID <- "DP4.00131.001"
filepath <- "N:/Science/AQU/Controls/D14_SYCA"
URIpath <- paste(filepath,"filesToStack00131","stackedFiles",sep = "/")

# #Download data from API and store somewhere
# dataFromAPI <- neonUtilities::zipsByProduct(streamMorphoDPID,siteID,package="expanded",check.size=FALSE,savepath = filepath)
# neonUtilities::stackByTable(filepath=paste(filepath,"filesToStack00131",sep = "/"), folder = TRUE)
# neonUtilities::zipsByURI(filepath=URIpath, savepath = URIpath, pick.files=FALSE, unzip = TRUE, check.size = FALSE)

#Read in downloaded data
surveyPtsDF <- read.table(paste0(URIpath,"/NEON_D14_SYCA_GEOMORPH_20181221_L0_VE/SYCA_surveyPts_20181221.CSV"),
                          sep = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE)

#The end date of the geomorphology survey (YYYYMMDD)
surveyDate<-'20181221' 

#Stipulate 4-digit site code, underscore, and survey year (ex: HOPB_2017)
surveyID <- "SYCA_2018" 

#Creates dataframe of all points associated with transect DSC1.
names(surveyPtsDF) <- c("name","latitude","Longitude","northing","easting","elevation","mapCode","E","N","H")
dischargePointsXS1<-subset(surveyPtsDF,mapCode=="Transect_DSC1")
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
dischargeXS1NorthStart<-dischargePointsXS1$N[dischargePointsXS1$name=="DSC_XS1"]
dischargeXS1EastStart<-dischargePointsXS1$E[dischargePointsXS1$name=="DSC_XS1"]

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
staffGaugePoints=subset(surveyPtsDF,surveyPtsDF$name=="SP_1.50M")
staffGaugePoints<-staffGaugePoints[order(staffGaugePoints$N),]
rownames(staffGaugePoints)<-seq(length=nrow(staffGaugePoints))

#Set meter mark where the staff gauge was shot in and the name of the staff gauge point:
#Recorded in field data
staffGaugeMeterMark<-1.50
staffGaugeElevation <- staffGaugePoints$H 

#Converts discharge XS1 transect point elevations to gauge height (rounded to 2 digits).
dischargePointsXS1$gaugeHeight<-dischargePointsXS1$H - (staffGaugeElevation - staffGaugeMeterMark)
dischargePointsXS1$gaugeHeight<-round(dischargePointsXS1$gaugeHeight,digits=2)

#Assigns a unique to each measurement for plot viewing purposes.  
dischargePointsXS1$ID<-c(1:length(dischargePointsXS1$name))

dischargePointsXS1 <- dischargePointsXS1[order(dischargePointsXS1$DistanceAdj),]

#Sets plot2 settings.  
xAxisTitle2<-list(title="Distance (m)",zeroline=FALSE, range=c(-5,15))
yAxisTitle2<-list(title="Gauge Height  (m)",zeroline=FALSE)
font<-list(size=12,color='black')

#Plot the cross section by distance and gauge height.  
plot_ly(data=dischargePointsXS1,x=~DistanceAdj, y=~gaugeHeight, name='Distance vs. Gauge Height', type='scatter', mode='markers+lines', text=~name)%>%
  add_trace(y= 0,name = 'Gauge Height = 0.00m',mode='lines',line = list(color = 'red', width = 2, dash='dash')) %>%
  layout(title = siteID, xaxis=xAxisTitle2, yaxis=yAxisTitle2)

#Asseses whether negative stage is present in the discharge cross-section 
negativeStage<-any(dischargePointsXS1$gaugeHeight<0)

if(negativeStage==TRUE){
  execpart<-TRUE
}else{exectpart<-FALSE}

#This section will only run if there are negative values in the gauge height column.  
if(exectpart){
  
  #Determines the lowest elevation of the discharge cross-section, assumed to be the thalweg. 
  dischargeXS1THL<-min(dischargePointsXS1$H)
  
  #Calculates the elevation of the 0.00 meter mark of the staff gauge.  
  staffGaugeZeroElevation<-(staffGaugePoints$H[staffGaugePoints$name=="SP_0.5"])-staffGaugeMeterMark
  
  #Calculates the difference between the staff gauge 0.00m mark elevation and the discharge thalweg elevation.   
  gaugeZeroQElevDiff<--as.numeric(dischargeXS1THL-staffGaugeZeroElevation)
  
  #Offsets the elevation of the gauge heights by this difference, rounds to two digits.  
  dischargePointsXS1$gaugeOffsetElevation<-dischargePointsXS1$H + (gaugeZeroQElevDiff)
  dischargePointsXS1$gaugeOffsetElevation<-round(dischargePointsXS1$gaugeOffsetElevation,digits=2)
  
  #Offsets the gauge heights by the offset elevation, rounds to two digits.  
  dischargePointsXS1$gaugeHeightOffset<-dischargePointsXS1$gaugeOffsetElevation - (staffGaugeElevation - staffGaugeMeterMark)
  dischargePointsXS1$gaugeHeightOffset<-round(dischargePointsXS1$gaugeHeightOffset,digits=2)
  
  #Plots discharge XS1 transect point distances vs gaugeHeightOffset.  Red line should be at the thalweg.
  plot_ly(data=dischargePointsXS1,x=~DistanceAdj, y=~gaugeHeight, name='Distance vs. Gauge Height', type='scatter', mode='markers+lines', text=~name)%>%
    add_trace(y= 0,name = 'Gauge Height = 0.00m',mode='lines',line = list(color = 'red', width = 2, dash='dash')) %>%
    layout(title = siteID, xaxis=xAxisTitle2, yaxis=yAxisTitle2)
  
  
}else{"There are no negative gauge height values in discharge XS1.  There is no need for correction."}

##### Now create the actual controls to upload... #####

#First, the addition or replacement when controls are activated table "geo_controlInfo_in"
numControls <- 2
geo_controlInfo_in_names <- c("locationID","startDate","endDate","controlNumber","segmentNumber","controlActivationState")
geo_controlInfo_in <- data.frame(matrix(nrow = numControls*numControls, ncol = length(geo_controlInfo_in_names)))
names(geo_controlInfo_in) <- geo_controlInfo_in_names

geo_controlInfo_in$locationID <- siteID
geo_controlInfo_in$startDate <- surveyDate
geo_controlInfo_in$endDate <- surveyDate
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
#geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==1&geo_controlInfo_in$segmentNumber==3] <- 0

#Is control #2 still active when control #3 is activated? 0 = No
#geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==2&geo_controlInfo_in$segmentNumber==3] <- 0

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
geo_controlType_in$startDate <- surveyDate
geo_controlType_in$endDate <- surveyDate
geo_controlType_in$controlNumber <- 1:numControls

#Entries for Control #1
geo_controlType_in$hydraulicControlType[1] <- "Rectangular Weir"
geo_controlType_in$controlLeft[1] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_LBF"]
geo_controlType_in$controlRight[1] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_XS30"]
geo_controlType_in$rectangularWidth[1] <- geo_controlType_in$controlRight[1]-geo_controlType_in$controlLeft[1]
geo_controlType_in$rectangularWidthUnc[1] <- 0.05 #Uncertainty associated with AIS survey

#Entries for Control #2
geo_controlType_in$hydraulicControlType[2] <- "Rectangular Channel"
geo_controlType_in$controlLeft[2] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_XS6"]
geo_controlType_in$controlRight[2] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC_XS39"]
geo_controlType_in$rectangularWidth[2] <- geo_controlType_in$controlRight[2]-geo_controlType_in$controlLeft[2]
geo_controlType_in$rectangularWidthUnc[2] <- 0.05

#Entries for Control #3
#geo_controlType_in$hydraulicControlType[3] <- "Rectangular Channel"
#geo_controlType_in$controlLeft[3] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "LFW11"]
#geo_controlType_in$controlRight[3] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "RFW12"]
#geo_controlType_in$rectangularWidth[3] <- geo_controlType_in$controlRight[3]-geo_controlType_in$controlLeft[3]
#geo_controlType_in$rectangularWidthUnc[3] <- 0.05

#Slope calculations
colfunc <- colorRampPalette(c("cyan","deeppink"))
# Stream was dry during survey, no wetted edge
# Used thalweg instead
wettedEdgePoints=subset(surveyPtsDF,surveyPtsDF$mapCode%in%c("THL"))
wettedEdgePoints<-wettedEdgePoints[order(wettedEdgePoints$N),]
rownames(wettedEdgePoints)<-seq(length=nrow(wettedEdgePoints)) 
# invisible(dev.new(noRStudioGD = TRUE))
# plot(wettedEdgePoints$E,wettedEdgePoints$N,pch=19, col=colfunc(length(wettedEdgePoints$H))[order(wettedEdgePoints$H)],
#      main=paste(siteID,"\nSelect a point above and below the discharge cross-section"),xlab="Raw Easting",ylab="Raw Northing")
# legend(min(wettedEdgePoints$E),max(wettedEdgePoints$N),legend=c("highest elevation","lowest elevation","discharge cross-section"),col = c("deeppink","cyan","green"),bty="n",pch = c(19,19,1))
# points(dischargePointsXS1$E,dischargePointsXS1$N, col="green")
# ans <- identify(wettedEdgePoints$E,wettedEdgePoints$N, n = 2, pos = F, tolerance = 0.25)
# Sys.sleep(1)
# invisible(dev.off())

ans=c(609,642)

#Plot subsetted wetted edges by manually entering ans values for tracking
# Stream was dry during survey, no wetted edge
# Used thalweg instead
#wettedEdgePoints <- wettedEdgePoints[932:981,]
# invisible(dev.new(noRStudioGD = TRUE))
# plot(wettedEdgePoints$E,wettedEdgePoints$N,pch=19, col=colfunc(length(wettedEdgePoints$H))[order(wettedEdgePoints$H)],
#      main=paste(siteID,"\nSelect two points above and below the discharge cross-section"),xlab="Raw Easting",ylab="Raw Northing")
# legend(min(wettedEdgePoints$E),max(wettedEdgePoints$N),legend=c("highest elevation","lowest elevation","discharge cross-section"),col = c("deeppink","cyan","green"),bty="n",pch = c(19,19,1))
# points(dischargePointsXS1$E,dischargePointsXS1$N, col="green")
# csOne <- identify(wettedEdgePoints$E,wettedEdgePoints$N, n = 2, pos = F, tolerance = 0.1)
# csTwo <- identify(wettedEdgePoints$E,wettedEdgePoints$N, n = 2, pos = F, tolerance = 0.1)
# Sys.sleep(1)
# invisible(dev.off())

csOne=c(609,607)
csTwo=c(640,642)

rise <- abs(mean(wettedEdgePoints$H[csOne])-mean(wettedEdgePoints$H[csTwo]))
run <- sqrt((mean(wettedEdgePoints$E[csOne])-mean(wettedEdgePoints$E[csTwo]))**2+(mean(wettedEdgePoints$N[csOne])-mean(wettedEdgePoints$N[csTwo]))**2)
geo_controlType_in$channelSlope[2] <- rise/run
geo_controlType_in$channelSlopeUnc[2] <- 0.015

#chosen to represent stream conditions with higher roughness above bankfull
geo_controlType_in$manningCoefficient[2] <- 0.05
geo_controlType_in$manningCoefficientUnc[2] <- 0.001
geo_controlType_in$stricklerCoefficient[2] <- 1/geo_controlType_in$manningCoefficient[2]
geo_controlType_in$stricklerCoefficientUnc[2] <- geo_controlType_in$stricklerCoefficient[2]*(geo_controlType_in$manningCoefficientUnc[3]/geo_controlType_in$manningCoefficient[3])

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
geo_priorParameters_in$priorActivationStage[1] <- dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC_THL"]
geo_priorParameters_in$priorActivationStageUnc[1] <- 0.01

geo_priorParameters_in$priorActivationStage[2] <- dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC_XS30"]
geo_priorParameters_in$priorActivationStageUnc[2] <- 0.01

#geo_priorParameters_in$priorActivationStage[3] <- dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC6"]
#geo_priorParameters_in$priorActivationStageUnc[3] <- 0.01

geo_priorParameters_in$locationID <- siteID
geo_priorParameters_in$startDate <- surveyDate
geo_priorParameters_in$endDate <- surveyDate

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

