
# library(neonUtilities)
# #For use with the API functionality
# dataDir <- "API"
# siteID <- "HOPB"
# 
# streamMorphoDPID <- "DP4.00131.001"
# dataFromAPI <- zipsByProduct(streamMorphoDPID,site,package="expanded",check.size=FALSE)
# filepath <- "C:/Users/kcawley/Desktop/test/filesToStack00131/"
# stackByTable(filepath=filepath, folder = TRUE)

#Use the new function here once we have the data in the new ECS zip packages

#KING processing code
filepath <- "C:/Users/kcawley/Desktop/test/"
surveyPtsDF <- read.table(paste0(filepath,"KING_surveyPts_20171115.csv"),sep = ",",stringsAsFactors = FALSE, header = TRUE)
siteID <- "KING"
surveyDate <- "2017-11-15T00:00"
names(surveyPtsDF) <- c("name","latitude","longitude","northing","easting","elevation","mapCode","E","N","H")

#Creates dataframe of all points associated with transect DSC1.  
dischargePointsXS1<-subset(surveyPtsDF,mapCode=="Transect_DSC1")
dischargePointsXS1<-dischargePointsXS1[order(dischargePointsXS1$N),]
rownames(dischargePointsXS1)<-seq(length=nrow(dischargePointsXS1)) 

#Plot the cross section to choose where to start
plot(dischargePointsXS1$E,dischargePointsXS1$N,main=paste(siteID,"Discharge XS1: E vs. N"),xlab="Raw Easting",ylab="Raw Northing")
text(dischargePointsXS1$E,dischargePointsXS1$N,labels=dischargePointsXS1$name,pos=4)

#To manually select NorthStart and EastStart coordinates
dischargeXS1NorthStart<-dischargePointsXS1$N[11]
dischargeXS1EastStart<-dischargePointsXS1$E[11]

#Assigns a raw Distance value to each point relative to the NorthStart and EastStart coordinates.
for(i in 1:(length(dischargePointsXS1$name))){
  dischargeXS1PointN<-dischargePointsXS1$N[i]
  dischargeXS1PointE<-dischargePointsXS1$E[i]
  dischargePointsXS1$DistanceRaw[i]<-sqrt(((dischargeXS1PointN-dischargeXS1NorthStart)^2)+((dischargeXS1PointE-dischargeXS1EastStart)^2))
}

#To manually select ReferenceDistance:
dischargeXS1ReferenceDistance<-dischargePointsXS1$DistanceRaw[37]

#Sets Horizontal adjustment value based on reference point coordinate.  
dischargeXS1HorizontalAdjust<-0-dischargeXS1ReferenceDistance

#Transforms raw distance to adjusted distance based on reference distance point.
for(i in 1:(length(dischargePointsXS1$name))){
  dischargePointsXS1$DistanceAdj[i]<-dischargePointsXS1$DistanceRaw[i]+dischargeXS1HorizontalAdjust
}

#Calculates the bankfull width.
DSCXS1Bankfull<-abs((dischargePointsXS1$DistanceAdj[grepl("RBF",dischargePointsXS1$name)])-(dischargePointsXS1$DistanceAdj[grepl("LBF",dischargePointsXS1$name)]))

#Creates dataframe of staff gauge points.
staffGaugePoints=subset(surveyPtsDF,surveyPtsDF$mapCode=="Gauge")
staffGaugePoints<-staffGaugePoints[order(staffGaugePoints$N),]
rownames(staffGaugePoints)<-seq(length=nrow(staffGaugePoints))

#Set meter mark where the staff gauge was shot in and the name of the staff gauge point:
#Recorded in field data
#No staff gauge present
staffGaugeMeterMark<-0.5
staffGaugeElevation <- staffGaugePoints$H[grepl("SP_0.5",staffGaugePoints$name)]  

#Converts discharge XS1 transect point Hs to gauge height (rounded to 2 digits).
dischargePointsXS1$gaugeHeight<-dischargePointsXS1$H - (staffGaugeElevation - staffGaugeMeterMark)
dischargePointsXS1$gaugeHeight<-round(dischargePointsXS1$gaugeHeight,digits=2)

#Plots discharge XS1 transect point distances vs gauge height with labels. Red line should be at or below the thalweg
invisible(dev.new(noRStudioGD = TRUE))
dischargePointsXS1 <- dischargePointsXS1[order(dischargePointsXS1$DistanceAdj),]
plot(dischargePointsXS1$DistanceAdj,dischargePointsXS1$gaugeHeight,main=paste(siteID,"Discharge XS1: Distance vs. Gauge Height"),xlab="Distance (m)",ylab="Gauge Height (m)")
text(dischargePointsXS1$DistanceAdj,dischargePointsXS1$gaugeHeight,labels=dischargePointsXS1$name,pos=4)
lines(lines(dischargePointsXS1$DistanceAdj,dischargePointsXS1$gaugeHeight,lty=3))
abline(h=0, lty=4,col="red")

#Calculates gaugeHeight at LB and RB bankfull:
gaugeHeightLBF<-dischargePointsXS1$gaugeHeight[grepl("LBF",dischargePointsXS1$name)]
gaugeHeightRBF<-dischargePointsXS1$gaugeHeight[grepl("RBF",dischargePointsXS1$name)]

##### Now create the actual controls to upload... #####
#First the addition or replacement when controls are activated table "geo_controlInfo_in"
numControls <- 3
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

#Setting control activation states that are user defined
#Is control #1 still active when control #2 is activated? 1 = Yes
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==1&geo_controlInfo_in$segmentNumber==2] <- 0
#Is control #1 still active when control #3 is activated? 0 = No
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==1&geo_controlInfo_in$segmentNumber==3] <- 0
#Is control #2 still active when control #3 is activated? 0 = No
geo_controlInfo_in$controlActivationState[geo_controlInfo_in$controlNumber==2&geo_controlInfo_in$segmentNumber==3] <- 1

#Second create entries for "geo_controlType_in" table for control parameters
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
geo_controlType_in$controlLeft[1] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC20"]
geo_controlType_in$controlRight[1] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC5"]
geo_controlType_in$rectangularWidth[1] <- geo_controlType_in$controlRight[1]-geo_controlType_in$controlLeft[1]
geo_controlType_in$rectangularWidthUnc[1] <- 0.05 #Uncertainty associated with AIS survey

#Entries for Control #2
geo_controlType_in$hydraulicControlType[2] <- "Rectangular Weir"
geo_controlType_in$controlLeft[2] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "DSC28"]
geo_controlType_in$controlRight[2] <- (dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "REW1"]+dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "RBF1"])/2
geo_controlType_in$rectangularWidth[2] <- geo_controlType_in$controlRight[2]-geo_controlType_in$controlLeft[2]
geo_controlType_in$rectangularWidthUnc[2] <- 0.05

#Entries for Control #3
geo_controlType_in$hydraulicControlType[3] <- "Rectangular Channel"
geo_controlType_in$controlLeft[3] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "LFW1"]
geo_controlType_in$controlRight[3] <- dischargePointsXS1$DistanceAdj[dischargePointsXS1$name == "LBF1"]
geo_controlType_in$rectangularWidth[3] <- geo_controlType_in$controlRight[3]-geo_controlType_in$controlLeft[3]
geo_controlType_in$rectangularWidthUnc[3] <- 0.05
#Slope calculations
colfunc <- colorRampPalette(c("cyan","deeppink"))
wettedEdgePoints=subset(surveyPtsDF,surveyPtsDF$mapCode%in%c("THL"))
wettedEdgePoints<-wettedEdgePoints[order(wettedEdgePoints$N),]
rownames(wettedEdgePoints)<-seq(length=nrow(wettedEdgePoints)) 
invisible(dev.new(noRStudioGD = TRUE))
plot(wettedEdgePoints$E,wettedEdgePoints$N,pch=19, col=colfunc(length(wettedEdgePoints$H))[order(wettedEdgePoints$H)],
     main=paste(siteID,"\nSelect a point above and below the discharge cross-section"),xlab="Raw Easting",ylab="Raw Northing")
legend(min(wettedEdgePoints$E),max(wettedEdgePoints$N),legend=c("highest elevation","lowest elevation","discharge cross-section"),col = c("deeppink","cyan","green"),bty="n",pch = c(19,19,1))
points(dischargePointsXS1$E,dischargePointsXS1$N, col="green")
ans <- identify(wettedEdgePoints$E,wettedEdgePoints$N, n = 2, pos = F, tolerance = 0.25)
#ans = 447, 427
Sys.sleep(1)
invisible(dev.off())
#Plot subsetted wetted edges by manually entering ans values for tracking
wettedEdgePoints <- wettedEdgePoints[min(ans):max(ans),]
invisible(dev.new(noRStudioGD = TRUE))
plot(wettedEdgePoints$E,wettedEdgePoints$N,pch=19, col=colfunc(length(wettedEdgePoints$H))[order(wettedEdgePoints$H)],
     main=paste(siteID,"\nSelect two points above and below the discharge cross-section"),xlab="Raw Easting",ylab="Raw Northing")
legend(min(wettedEdgePoints$E),max(wettedEdgePoints$N),legend=c("highest elevation","lowest elevation","discharge cross-section"),col = c("deeppink","cyan","green"),bty="n",pch = c(19,19,1))
points(dischargePointsXS1$E,dischargePointsXS1$N, col="green")
csOne <- identify(wettedEdgePoints$E,wettedEdgePoints$N, n = 1, pos = F, tolerance = 0.1)
#csOne = 12
csTwo <- identify(wettedEdgePoints$E,wettedEdgePoints$N, n = 1, pos = F, tolerance = 0.1)
#csTwo = 10
Sys.sleep(1)
invisible(dev.off())
rise <- abs(mean(wettedEdgePoints$H[csOne])-mean(wettedEdgePoints$H[csTwo]))
run <- sqrt((mean(wettedEdgePoints$E[csOne])-mean(wettedEdgePoints$E[csTwo]))**2+(mean(wettedEdgePoints$N[csOne])-mean(wettedEdgePoints$N[csTwo]))**2)
geo_controlType_in$channelSlope[3] <- rise/run
cat("Calculated slope:",rise/run)
geo_controlType_in$channelSlopeUnc[3] <- 0.005
#chosen to represent stream conditions with higher roughness above bankfull
geo_controlType_in$manningCoefficient[3] <- 0.05
geo_controlType_in$manningCoefficientUnc[3] <- 0.001
geo_controlType_in$stricklerCoefficient[3] <- 1/geo_controlType_in$manningCoefficient[3]
geo_controlType_in$stricklerCoefficientUnc[3] <- geo_controlType_in$stricklerCoefficient[3]*(geo_controlType_in$manningCoefficientUnc[3]/geo_controlType_in$manningCoefficient[3])

#Third use equations to populate "geo_priorParameters_in" table
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
geo_priorParameters_in$priorActivationStage[1] <- dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC19"]
geo_priorParameters_in$priorActivationStageUnc[1] <- 0.01
geo_priorParameters_in$priorActivationStage[2] <- (dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC4"]+dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC27"])/2
geo_priorParameters_in$priorActivationStageUnc[2] <- 0.01
geo_priorParameters_in$priorActivationStage[3] <- (dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "LBF1"]+dischargePointsXS1$gaugeHeight[dischargePointsXS1$name == "DSC_LB"])/2
geo_priorParameters_in$priorActivationStageUnc[3] <- 0.01

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
dev.copy2pdf(file = paste0(siteID,"_siteControls.pdf"), width = 16, height = 9)

#Write out three tables for ingest to GitHub and testing location both
geo_controlInfo_in_output <- c("locationID",
                               "startDate",
                               "endDate",
                               "controlNumber",
                               "segmentNumber",
                               "controlActivationState")
geo_controlInfo_in <- geo_controlInfo_in[,names(geo_controlInfo_in)%in%geo_controlInfo_in_output]
write.csv(geo_controlInfo_in,
          "geo_controlInfo_in.csv",
          quote = TRUE,
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(geo_controlInfo_in,
          "H:/controlTesting/geo_controlInfo_in.csv",
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
          "geo_controlType_in.csv",
          quote = TRUE,
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(geo_controlType_in,
          "H:/controlTesting/geo_controlType_in.csv",
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
          "geo_priorParameters_in.csv",
          quote = TRUE,
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(geo_priorParameters_in,
          "H:/controlTesting/geo_priorParameters_in.csv",
          quote = TRUE,
          row.names = FALSE,
          fileEncoding = "UTF-8")

