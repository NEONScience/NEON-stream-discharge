applyRegtoL0 <- function(regressionData = regressionData, L0PressureData = waterElevationDF, site = site, startDate = startDate, endDate = endDate, session)
{
  #L0PressureData <- waterElevationDF
  # site = "LECO"
  # startDate = "2024-07-31"
  # endDate = "2024-07-31"
  updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 80, title = "Applying regression to water column height")
  regressionData <- regressionData %>% filter(regressionData$siteID == site)
  #FIX
  regressionData_dateFiltered <- regressionData[startDate <= regressionData$regressionStartDate & regressionData$regressionEndDate >= endDate,]
  
  #Set data types for regression data
  regressionData$regressionStartDate <- as.POSIXct(regressionData$regressionStartDate, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  regressionData$regressionEndDate <- as.POSIXct(regressionData$regressionEndDate, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  regressionData$regressionSlope <- as.numeric(regressionData$regressionSlope)
  regressionData$regressionIntercept <- as.numeric(regressionData$regressionIntercept)

  #Add regression ID to the L0 pressure data
  if (nrow(regressionData_dateFiltered) != 0) {
    for (i in 1:nrow(L0PressureData)) {
      for (j in 1:nrow(regressionData)) {
          L0PressureData$regressionID[i] <- regressionData$regressionID[j]
      }
    }
  }else{
    #If using a previous water year's rating curve, use the most recent regression equation employed in the previous water year
    L0PressureData$regressionID <- regressionData$regressionID[regressionData$regressionEndDate==max(regressionData$regressionEndDate)]
  }
  
  #Calculate stage
  for (i in 1:nrow(L0PressureData)) {
    for (j in 1:nrow(regressionData)) {
      if (L0PressureData$regressionID[i]==regressionData$regressionID[j]) {
        L0PressureData$calculatedStage[i] <- (L0PressureData$waterColumnHeight[i]*regressionData$regressionSlope[j])+regressionData$regressionIntercept[j]
      }
    }
  }
  
  #Add curve ID to the L0 pressure data
  #L0PressureData$curveID <- ratingCurveID
  
  if (!is.null(L0PressureData$trollPressure)) {
    # Get gauge-pressure relationship data from the OS system and calculate and add systematic uncertainty
    
    gaugePressRelData <- try(queryDischargeData())
      
    gaugePressRelData$diffBtwnMeasAndCalcStage <- abs(as.numeric(gaugePressRelData$calculatedStage)-as.numeric(gaugePressRelData$gaugeHeight))
    regressionsInRelationship <- unique(gaugePressRelData$regressionID)
    for (i in 1:length(regressionsInRelationship)) {
      L0PressureData$systematicUnc[L0PressureData$regressionID==regressionsInRelationship[i]] <- mean(gaugePressRelData$diffBtwnMeasAndCalcStage[gaugePressRelData$regressionID==regressionsInRelationship[i]],na.rm = T)
    }
  }else{
    L0PressureData$systematicUnc <- NA
  }
  updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 95, title = "Finished applying regression to water column height")
  
  return(L0PressureData)
}
