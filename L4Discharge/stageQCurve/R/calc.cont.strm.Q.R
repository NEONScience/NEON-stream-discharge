##############################################################################################
#' @title YOUR TITLE

#' @author
#' YOUR NAME \email{EMAIL@battelleecology.org} \cr

#' @description BRIEF DESCRIPTION

#' @return OUTPUT DESCRIPTION

# changelog and author contributions / copyrights
#   YOUR NAME (YYYY-MM-DD)
#     original creation
##############################################################################################
calc.cont.strm.Q <- function(DIRPATH = Sys.getenv("DIRPATH"),
                             BAMFOLD = Sys.getenv("BAMFOLD"),
                             BAMFILE = Sys.getenv("BAMFILE"),
                             DATAWS = Sys.getenv("DATAWS"),
                             BAMWS = Sys.getenv("BAMWS"),
                             startDate = Sys.getenv("STARTDATE"),
                             endDate = Sys.getenv("ENDDATE")
                             site = Sys.getenv("SITE")){

  # String constants
  nameRegex <- "^\"|\" .*"
  configRegex <- "^\'|\' .*"
  predRegex <- "[0-9]{1,}"
  osDateFormat <- "%Y-%m-%dT%H:%M:%S"
  osPubDateFormat <- "%Y-%m-%dT%H:%MZ"
  isDateFormat <- "%Y-%m-%dT%H:%M:%S.000Z"

  # Numeric Constants
  secondsInMinute <- 60
  secondsInDay <- 60*60*24
  secondsIn10Min <- 10*60
  secondsInYear <- 60*60*24*365
  minutesInDay <- 60*24
  #gaugeSurveyLoc <- 0.8#meters
  convKPatoPa <- 1000 #Pa per 1 kPa
  roe = 999 #kg/m^3 density of water
  grav = 9.80665 #m/s^2 gravity constant

  # Additional path information
  Config_BaM <- readLines(paste0(DIRPATH,BAMFOLD,"/Config_BaM.txt"))
  RunOptionsName <- gsub(nameRegex,"",Config_BaM[2])
  ModelName <- gsub(nameRegex,"",Config_BaM[3])
  ControlMatrixName <- gsub(nameRegex,"",Config_BaM[4])
  DataName <- gsub(nameRegex,"",Config_BaM[5])
  cookedMCMCName <- gsub(nameRegex,"",readLines(paste0(DIRPATH,BAMWS,gsub(nameRegex,"",Config_BaM[8])))[1])

  # Now for when I need it
  now <- as.POSIXct(Sys.time())

  # Parse BaM configuration files
  PredMasterName <- gsub(nameRegex,"",Config_BaM[11])
  ConfigPredictions <- readLines(paste0(DIRPATH,BAMWS,PredMasterName))
  Config_Pred_Maxpost <- readLines(paste0(DIRPATH,BAMWS,gsub(configRegex,"",ConfigPredictions[2])))
  Config_Pred_hU <- readLines(paste0(DIRPATH,BAMWS,gsub(configRegex,"",ConfigPredictions[3])))
  Config_Pred_TotalU <- readLines(paste0(DIRPATH,BAMWS,gsub(configRegex,"",ConfigPredictions[4])))
  stageSeries <- gsub(configRegex,"",Config_Pred_Maxpost[1])
  stageSpaghettis <- gsub(configRegex,"",Config_Pred_hU[1])
  QMaxpostSpagName <- gsub(configRegex,"",Config_Pred_Maxpost[7])
  QGaugeUncSpagName <- gsub(configRegex,"",Config_Pred_hU[7])
  QGaugeUncEnvName <- gsub(configRegex,"",Config_Pred_hU[10])
  QTotalUncSpagName <- gsub(configRegex,"",Config_Pred_TotalU[7])
  QTotalUncEnvName <- gsub(configRegex,"",Config_Pred_TotalU[10])
  dataPath <- gsub(configRegex,"",readLines(paste0(DIRPATH,BAMWS,DataName))[1])
  
  # Metadata
  searchIntervalStartDate <- as.POSIXct(startDate, tz = "UTC")
  searchIntervalEndDate <- as.POSIXct(endDate, tz = "UTC")+secondsInDay # Add one day
  WYsearchIntervalStartDate <- as.POSIXct(format(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate)[["startDate"]], format = "%Y-%m-%d"))
  WYsearchIntervalEndDate <- as.POSIXct(format(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate)[["endDate"]], format = "%Y-%m-%d"))+secondsInDay
  downloadedDataPath <- DATAWS
  
  # Get continuous discharge data from the OS system
  # If data has been downloaded using neonUtilities::zipsByProduct() and saved to DATAWS
  if(file.exists(paste0(downloadedDataPath,"filesToStack00130"))){
    # Stack the tables if they have not been already
    if (!file.exists(paste0(downloadedDataPath,"filesToStack00130/stackedFiles"))) {
      neonUtilities::stackByTable(paste0(downloadedDataPath,"filesToStack00130"))
    }
    continuousData  <- try(read.csv(paste(downloadedDataPath,"filesToStack00130","stackedFiles","csd_continuousDischarge.csv", sep = "/")),silent = T)
    gaugePressureData <- try(read.csv(paste(downloadedDataPath,"filesToStack00130","stackedFiles","sdrc_gaugePressureRelationship.csv", sep = "/")),silent = T)
    regressionData <- try(read.csv(paste(downloadedDataPath,"filesToStack00130","stackedFiles","geo_gaugeWaterColumnRegression.csv", sep = "/")),silent = T)
  }else{
    # If data has been directly downloaded from the NEON data portal and saved to DATAWS
    if(file.exists(paste0(downloadedDataPath,"NEON_discharge-continuous.zip"))|
       file.exists(paste0(downloadedDataPath,"NEON_discharge-continuous"))){
      # Stack the tables if they have not been already
      if(file.exists(paste0(downloadedDataPath,"NEON_discharge-continuous.zip"))){
        neonUtilities::stackByTable(paste0(downloadedDataPath,"NEON_discharge-continuous.zip"))
      }
      continuousData  <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-continuous","stackedFiles","csd_continuousDischarge.csv", sep = "/")),silent = T)
      gaugePressureData <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-continuous","stackedFiles","sdrc_gaugePressureRelationship.csv", sep = "/")),silent = T)
      regressionData <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-continuous","stackedFiles","geo_gaugeWaterColumnRegression.csv", sep = "/")),silent = T)
    }else{
      # If the individual files are available in DATAWS
      availableFiles <- list.files(downloadedDataPath)
      continuousData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("csd_continuousDischarge",availableFiles)], sep = "/")),silent = T))
      gaugePressureData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("sdrc_gaugePressureRelationship",availableFiles)], sep = "/")),silent = T))
      regressionData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("geo_gaugeWaterColumnRegression",availableFiles)], sep = "/")),silent = T))
    }
  }
  
  # Error handling if no discharge or curve identification data can be found
  if(attr(continuousData, "class") == "try-error"){
    failureMessage <- paste0("Data could not be retrieved from sdrc_continuousDischarge_pub. Ensure the required input data are stored in ",downloadedDataPath)
    stop(failureMessage)
  }
  if(attr(gaugePressureData, "class") == "try-error"){
    failureMessage <- paste0("Data could not be retrieved from sdrc_gaugePressureRelationship. Ensure the required input data are stored in ",downloadedDataPath)
    stop(failureMessage)
  }
  if(attr(regressionData, "class") == "try-error"){
    failureMessage <- paste0("Data could not be retrieved from geo/bat_gaugeWaterColumnRegression. Ensure the required input data are stored in ",downloadedDataPath)
    stop(failureMessage)
  }

  # Subset to the metadata and calibrated pressure data for the time range specified
  continuousData$CalPress <- continuousData$calibratedPressure
  continuousData$validCalFlag <- continuousData$dischargeValidCalQF
  continuousData$date <- as.POSIXct(continuousData$endDate, format = osPubDateFormat)
  L0PressureData <- continuousData[names(continuousData)%in%c("siteID","namedLocation","stationHorizontalID","startDate","date","nonSystematicUnc","CalPress","validCalFlag")]
  L0PressureData <- L0PressureData[L0PressureData$date>=searchIntervalStartDate&L0PressureData$date<searchIntervalEndDate,]
  
  #Convert pressure to water column height above troll
  L0PressureData$waterColumnHeight <- (L0PressureData$CalPress/(roe * grav)) * convKPatoPa
  
  # Get location data
  trollLocData <- try(geoNEON::getLocBySite(site,type = "AQU",history = T),silent = T)
  # Error handling if the GET was unsuccessful
  if(attr(trollLocData, "class") == "try-error"){
    failureMessage <- "Troll location history could not be retrieved"
    stop(failureMessage)
  }
  trollLocData <- trollLocData[trollLocData$`2`%in%unique(L0PressureData$namedLocation),]
  #Error handling if there is no data
  if(nrow(trollLocData)<1){
    failureMessage <- "Zero (0) troll location history records were retrieved"
    stop(failureMessage)
  }

  #Set data types
  #Add an endDate to avoid NA and null situations
  if(is.null(trollLocData$locationEndDate)){
    trollLocData$locationEndDate[is.null(trollLocData$locationEndDate)] <- format(as.POSIXct(now),format = osDateFormat)}
  if(any(is.na(trollLocData$locationEndDate))){
    trollLocData$locationEndDate[is.na(trollLocData$locationEndDate)] <- format(as.POSIXct(now),format = osDateFormat)}
  trollLocData$locationStartDate <-  as.POSIXct(trollLocData$locationStartDate,format = osDateFormat)
  trollLocData$locationEndDate <- as.POSIXct(trollLocData$locationEndDate,format = osDateFormat)
  trollLocData$zOffset <- as.numeric(trollLocData$zOffset)
  trollLocData$elevation <- as.numeric(trollLocData$elevation)
  trollLocData <- trollLocData[order(trollLocData$locationEndDate),]
  trollLocData$refElevPlusZ <- trollLocData$elevation + trollLocData$zOffset
  #Apply troll elevation offsets to the calculated water column height in regressionData
  for(n in 1:length(unique(trollLocData$`2`))){
    trollLocData_temp <- trollLocData[trollLocData$`2`==unique(trollLocData$`2`)[n]]
    trollLocData_temp <- trollLocData_temp[order(trollLocData$locationEndDate),]
    for(i in 1:length(trollLocData_temp$endDate)){
      locStart <- trollLocData_temp$locationStartDate[i]
      locEnd <-trollLocData_temp$locationEndDate[i]
      dataToApplyOffset <- L0PressureData$waterColumnHeight[L0PressureData$date>=locStart&
                                                            L0PressureData$date<locEnd]
      if(i==1){
        locOffset <- 0
        L0PressureData$waterColumnHeight[L0PressureData$date>=locStart&
                                                L0PressureData$date<locEnd] <-
          dataToApplyOffset + as.numeric(locOffset)
        }else{
          locOffset <- trollLocData_temp$refElevPlusZ[i] - trollLocData_temp$refElevPlusZ[1]
          L0PressureData$waterColumnHeight[L0PressureData$date>=locStart&
                                                  L0PressureData$date<locEnd] <-
            dataToApplyOffset + as.numeric(locOffset)
        }
    }
  }

  #Set data types for regression data
  regressionData$regressionStartDate <- as.POSIXct(regressionData$regressionStartDate, format = osPubDateFormat)
  regressionData$regressionEndDate <- as.POSIXct(regressionData$regressionEndDate, format = osPubDateFormat)
  regressionData$regressionSlope <- as.numeric(regressionData$regressionSlope)
  regressionData$regressionIntercept <- as.numeric(regressionData$regressionIntercept)  

  #Add regression ID to the L0 pressure data
  L0PressureData$regressionID <- NA
  for (i in 1:nrow(regressionData)) {
    L0PressureData$regressionID[L0PressureData$date>=regressionData$regressionStartDate[i]&L0PressureData$date<=regressionData$regressionEndDate[i]] <- regressionData$regressionID[i]
  }
  L0PressureData$regressionID[is.na(L0PressureData$regressionID)&L0PressureData$date>max(regressionData$regressionEndDate)] <- regressionData$regressionID[regressionData$regressionEndDate==max(regressionData$regressionEndDate)]

  # Calculate stage
  L0PressureData$calculatedStage <- NA
  for (i in 1:nrow(regressionData)) {
    L0PressureData$calculatedStage[L0PressureData$regressionID==regressionData$regressionID[i]] <- (L0PressureData$waterColumnHeight[L0PressureData$regressionID==regressionData$regressionID[i]]*regressionData$regressionSlope[i])+regressionData$regressionIntercept[i]
  }
  
  # Calculate systematic uncertainty
  L0PressureData$systematicUnc <- NA
  gaugePressureData$diffBtwnMeasAndCalcStage <- abs(as.numeric(gaugePressureData$calculatedStage)-as.numeric(gaugePressureData$gaugeHeight))
  for (i in 1:length(unique(gaugePressureData$regressionID))) {
    currRegID <- unique(gaugePressureData$regressionID)[i]
    L0PressureData$systematicUnc[L0PressureData$regressionID==currRegID] <- mean(gaugePressureData$diffBtwnMeasAndCalcStage[gaugePressureData$regressionID==currRegID],na.rm=T)
  }

  #Get rating curve from the OS system
  # If data has been downloaded using neonUtilities::zipsByProduct() and saved to DATAWS
  if(file.exists(paste0(downloadedDataPath,"filesToStack00133"))){
    # Stack the tables if they have not been already
    if (!file.exists(paste0(downloadedDataPath,"filesToStack00133/stackedFiles"))) {
      neonUtilities::stackByTable(paste0(downloadedDataPath,"filesToStack00133"))
    }
    dischargeData  <- try(read.csv(paste(downloadedDataPath,"filesToStack00133","stackedFiles","sdrc_gaugeDischargeMeas.csv", sep = "/")),silent = T)
    curveIdentification <- try(read.csv(paste(downloadedDataPath,"filesToStack00133","stackedFiles","sdrc_stageDischargeCurveInfo.csv", sep = "/")),silent = T)
    spagData <- try(read.csv(paste(downloadedDataPath,"filesToStack00133","stackedFiles","sdrc_sampledParameters.csv", sep = "/")),silent = T)
  }else{
    # If data has been directly downloaded from the NEON data portal and saved to DATAWS
    if(file.exists(paste0(downloadedDataPath,"NEON_discharge-rating-curves.zip"))|
       file.exists(paste0(downloadedDataPath,"NEON_discharge-rating-curves"))){
      # Stack the tables if they have not been already
      if(file.exists(paste0(downloadedDataPath,"NEON_discharge-rating-curves.zip"))){
        neonUtilities::stackByTable(paste0(downloadedDataPath,"NEON_discharge-rating-curves.zip"))
      }
      dischargeData  <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles","sdrc_gaugeDischargeMeas.csv", sep = "/")),silent = T)
      curveIdentification <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles","sdrc_stageDischargeCurveInfo.csv", sep = "/")),silent = T)
      spagData <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles","sdrc_sampledParameters.csv", sep = "/")),silent = T)
    }else{
      # If the individual files are available in DATAWS
      availableFiles <- list.files(downloadedDataPath)
      dischargeData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("sdrc_gaugeDischargeMeas",availableFiles)], sep = "/")),silent = T))
      curveIdentification  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("sdrc_stageDischargeCurveInfo",availableFiles)], sep = "/")),silent = T))
      spagData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("sdrc_sampledParameters",availableFiles)], sep = "/")),silent = T))
    }
  }
  
  # Add curve ID
  curveIdentification$startDate <- as.POSIXct(curveIdentification$startDate, format=osPubDateFormat)
  curveIdentification$endDate <- as.POSIXct(curveIdentification$endDate, format=osPubDateFormat)
  L0PressureData$curveID <- NA
  for (i in 1:nrow(curveIdentification)) {
    L0PressureData$curveID[L0PressureData$date>=curveIdentification$startDate[i]&L0PressureData$date<=curveIdentification$endDate[i]] <- curveIdentification$curveID[i]
  }
  # What curves are represented in this date range?
  numCurves <- unique(L0PressureData$curveID)
  
  # Run the BaM executable for each curve active during this time
  for(i in 1:length(numCurves)){
    i=1
    currCurveID <- numCurves[i]
    
    # Need to remove any null timestamps
    dataForBaM <- L0PressureData[L0PressureData$curveID==currCurveID,]
    actualDataIdx <- which(!is.na(dataForBaM$calculatedStage))
    dataForBaM <- L0PressureData$calculatedStage[actualDataIdx]
    
    if(length(dataForBaM) < 1){
      # Skip the whole BaM thing since there isn't any data
      # Add in the columns that come from BaM, though
      columnsFromBaM <- c(
        'maxpostDischarge',
        'withParaUncQMedian',
        'withParaUncQlower2Std',
        'withParaUncQupper2Std',
        'withParaUncQlower1Std',
        'withParaUncQupper1Std',
        'withParaUncQMean',
        'withParaUncQStdDev',
        'withRemnUncQMedian',
        'withRemnUncQlower2Std',
        'withRemnUncQUpper2Std',
        'withRemnUncQlower1Std',
        'withRemnUncQupper1Std',
        'withRemnUncQMean',
        'withRemnUncQStdDev'
      )
      compiledOutputs <- L0PressureData
      finalNamesWithoutRunningBam <- c(names(compiledOutputs),columnsFromBaM)
      compiledOutputs[,(ncol(compiledOutputs)+1):(ncol(compiledOutputs)+length(columnsFromBaM))] <- NA
      names(compiledOutputs) <- finalNamesWithoutRunningBam
    }else{
      # Get spaghettis for the full date range to cut down on calls to the database
      spagDataForBaM <- spagData[spagData$curveID==currCurveID,]
      spagOutPath <- paste0(DIRPATH, BAMWS, cookedMCMCName)
      stageQCurve::txt.out.spag.data(spagDataIn = spagData,spagOutPath = spagOutPath)

      
      
      
    }

    
    
    
  }
  
  
  #  
  
  
  
  
  
  
  
  
}

