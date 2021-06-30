##############################################################################################
#' @title Function to calculate Continuous Discharge

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description This script uses the BaM executable to predict continuous stage and flow series
#' with associated uncertainties for a site in a given month using NEON data.

#' @importFrom neonUtilities stackByTable
#' @importFrom geoNEON getLocBySite

#' @param DIRPATH An environment variable that contains the location of the files in
#' the Docker container [string]
#' @param BAMFOLD An environment variable that contains the folder name of the BaM exe in
#' the Docker container [string]
#' @param BAMFILE An environment variable that contains the name of the BaM exe in
#' the Docker container [string]
#' @param DATAWS An environment variable that contains the location of the downloaded NEON
#' zip files in the Docker container [string]
#' @param BAMWS An environment variable that contains the location of the BaM config
#' files in the Docker container [string]
#' @param startDate A date in the format of "YYYY-MM-DD", entered as env variable in
#' run command [string]
#' @param site A four letter site code for a NEON site, entered as env variable in
#' run command [string]

#' @return This function results in text files being written to a directory that contain
#' the continuous data.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Zachary L. Nickerson (2021-06-28)
#     original creation
##############################################################################################
calc.cont.strm.Q <- function(DIRPATH = Sys.getenv("DIRPATH"),
                             BAMFOLD = Sys.getenv("BAMFOLD"),
                             BAMFILE = Sys.getenv("BAMFILE"),
                             DATAWS = Sys.getenv("DATAWS"),
                             BAMWS = Sys.getenv("BAMWS"),
                             startDate = Sys.getenv("STARTDATE"),
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

  # Reconfigure master configuration file for continuous discharge prediction
  PredMasterName <- gsub(nameRegex,"",Config_BaM[11])
  ConfigPredictions <- readLines(paste0(DIRPATH,BAMWS,PredMasterName))[1:4]
  ConfigPredictions[1] <- gsub("[0-9] ","3 ",ConfigPredictions[1])
  ConfigPredictions[2] <- gsub("'.*'","'Config_Pred_Maxpost.txt'",ConfigPredictions[2])
  ConfigPredictions[3] <- gsub("'.*'","'Config_Pred_hU.txt'",ConfigPredictions[3])
  ConfigPredictions[4] <- gsub("'.*'","'Config_Pred_TotalU.txt'",ConfigPredictions[4])
  writeLines(ConfigPredictions,paste0(DIRPATH,BAMWS,PredMasterName))

  # Parse BaM configuration files
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
  WYsearchIntervalStartDate <- as.POSIXct(format(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate)[["startDate"]], format = "%Y-%m-%d"))
  WYsearchIntervalEndDate <- as.POSIXct(format(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate)[["endDate"]], format = "%Y-%m-%d"))+secondsInDay
  yearMonth <- format(searchIntervalStartDate,format="%Y-%m")
  year <- format(searchIntervalStartDate,format="%Y")
  month <- format(searchIntervalStartDate,format="%m")
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
    regressionData <- try(read.csv(paste(downloadedDataPath,"filesToStack00130","stackedFiles",paste0(mod,"_gaugeWaterColumnRegression.csv"), sep = "/")),silent = T)
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
      regressionData <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-continuous","stackedFiles",paste0(mod,"_gaugeWaterColumnRegression.csv"), sep = "/")),silent = T)
    }else{
      # If the individual files are available in DATAWS
      availableFiles <- list.files(downloadedDataPath)
      continuousData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("csd_continuousDischarge",availableFiles)], sep = "/")),silent = T))
      gaugePressureData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("sdrc_gaugePressureRelationship",availableFiles)], sep = "/")),silent = T))
      regressionData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl(paste0(mod,"_gaugeWaterColumnRegression"),availableFiles)], sep = "/")),silent = T))
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
  L0PressureData <- L0PressureData[grepl(yearMonth,L0PressureData$date),]
  # L0PressureData <- L0PressureData[L0PressureData$date>=searchIntervalStartDate&L0PressureData$date<searchIntervalEndDate,]

  # Error handling is there is no data
  if(nrow(L0PressureData)<1){
    failureMessage <- paste0("Zero (0) records available from DP4.00130.001 for ",site," ",year,"-",month)
    stop(failureMessage)
  }

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
    curveIdentification <- try(read.csv(paste(downloadedDataPath,"filesToStack00133","stackedFiles",paste0(mod,"_curveIdentification.csv"), sep = "/")),silent = T)
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
      curveIdentification <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles",paste0(mod,"_curveIdentification.csv"), sep = "/")),silent = T)
      spagData <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles","sdrc_sampledParameters.csv", sep = "/")),silent = T)
    }else{
      # If the individual files are available in DATAWS
      availableFiles <- list.files(downloadedDataPath)
      dischargeData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("sdrc_gaugeDischargeMeas",availableFiles)], sep = "/")),silent = T))
      curveIdentification  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl(paste0(mod,"_curveIdentification"),availableFiles)], sep = "/")),silent = T))
      spagData  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("sdrc_sampledParameters",availableFiles)], sep = "/")),silent = T))
    }
  }

  # Add curve ID
  curveIdentification$curveStartDate <- as.POSIXct(curveIdentification$curveStartDate, format=osPubDateFormat)
  curveIdentification$curveEndDate <- as.POSIXct(curveIdentification$curveEndDate, format=osPubDateFormat)
  L0PressureData$curveID <- NA
  for (i in 1:nrow(curveIdentification)) {
    L0PressureData$curveID[L0PressureData$date>=curveIdentification$curveStartDate[i]&L0PressureData$date<=curveIdentification$curveEndDate[i]] <- curveIdentification$curveID[i]
  }
  # What curves are represented in this date range?
  numCurves <- unique(L0PressureData$curveID)

  l=1

  # Run the BaM executable for each curve active during this time
  for(c in 1:length(numCurves)){
    currCurveID <- numCurves[c]

    # Need to remove any null timestamps
    L0PressureDataForBaM <- L0PressureData[L0PressureData$curveID==currCurveID,]
    actualDataIdx <- which(!is.na(L0PressureDataForBaM$calculatedStage))
    dataForBaM <- L0PressureDataForBaM$calculatedStage[actualDataIdx]

    if(length(dataForBaM) < 1){
      # Skip the whole BaM thing since there isn't any data
      # Add in the columns that come from BaM, though
      columnsFromBaM <- c(
        'maxpostDischarge',
        'withParaUncQMedian',
        'withParaUncQLower2Std',
        'withParaUncQUpper2Std',
        'withParaUncQLower1Std',
        'withParaUncQUpper1Std',
        'withParaUncQMean',
        'withParaUncQStdDev',
        'withRemnUncQMedian',
        'withRemnUncQLower2Std',
        'withRemnUncQUpper2Std',
        'withRemnUncQLower1Std',
        'withRemnUncQUpper1Std',
        'withRemnUncQMean',
        'withRemnUncQStdDev'
      )
      compiledOutputs <- L0PressureDataForBaM
      finalNamesWithoutRunningBam <- c(names(compiledOutputs),columnsFromBaM)
      compiledOutputs[,(ncol(compiledOutputs)+1):(ncol(compiledOutputs)+length(columnsFromBaM))] <- NA
      names(compiledOutputs) <- finalNamesWithoutRunningBam
    }else{
      # Write out configuration files for controls
      stageQCurve::txt.out.run.opts(runType = "pred", RunOptionsPath = paste0(DIRPATH, BAMWS, RunOptionsName))
      controlMatrixPath <- paste0(DIRPATH, BAMWS, ControlMatrixName)
      priorParamsPath <- paste0(DIRPATH, BAMWS, ModelName)
      controlSurveyDate <- format(as.POSIXct(curveIdentification$controlSurveyEndDateTime[curveIdentification$curveID==currCurveID]),"%Y-%m-%d")
      numCtrls <- try(stageQCurve::txt.out.ctrl.and.prior.parm.ext(site = site,
                                                                   controlSurveyDate = controlSurveyDate,
                                                                   controlMatrixPath = controlMatrixPath,
                                                                   priorParamsPath = priorParamsPath,
                                                                   downloadedDataPath = downloadedDataPath),silent = T)
      if(!is.null(attr(numCtrls, "class")) && attr(numCtrls, "class") == "try-error"){
        failureMessage <- "Prior parameters or control matrix error; check inputs"
        stop(failureMessage)
      }

      # Get spaghettis for the full date range to cut down on calls to the database
      spagDataForBaM <- spagData[spagData$curveID==currCurveID,]
      spagDataForBaM[spagDataForBaM==0] <- 0.0001
      spagOutPath <- paste0(DIRPATH, BAMWS, cookedMCMCName)
      stageQCurve::txt.out.spag.data(spagDataIn = spagDataForBaM,spagOutPath = spagOutPath)

      #Write out input data files for predictions
      #Write out the single spaghetti for the maxPost timeseries
      write.table(dataForBaM,paste0(DIRPATH,BAMFOLD,stageSeries), row.names = F, col.names = F)

      #Write out the set of spaghetti for the hu and Totalu
      numSpag <- as.numeric(gsub(" {1,}!.*","",Config_Pred_TotalU[3]))
      kMean <- as.numeric(dataForBaM)
      kStd <- abs(as.numeric(L0PressureData$systematicUnc[actualDataIdx])) + abs(as.numeric(L0PressureData$nonSystematicUnc[actualDataIdx]))
      #Need to loop through these to the whole list
      stage_noisy <- matrix(NA, ncol = numSpag, nrow = length(dataForBaM))
      for(j in 1:length(dataForBaM)){
        stage_noisy[j,] <- stats::rnorm(numSpag,mean = kMean[j],sd = kStd[j])
      }
      #Write out the "noisy" file
      write.table(stage_noisy,paste0(DIRPATH,BAMFOLD,stageSpaghettis), row.names = F, col.names = F)

      #Update number of observations for each of the prediction files
      Config_Pred_Maxpost[2] <- gsub(predRegex,length(dataForBaM),Config_Pred_Maxpost[2])
      #Config_Pred_Maxpost[9] <- gsub(".true.",".false.",Config_Pred_Maxpost[9])
      writeLines(Config_Pred_Maxpost,paste0(DIRPATH,BAMWS,gsub(configRegex,"",ConfigPredictions[2])))

      Config_Pred_hU[2] <- gsub(predRegex,length(dataForBaM),Config_Pred_hU[2])
      writeLines(Config_Pred_hU,paste0(DIRPATH,BAMWS,gsub(configRegex,"",ConfigPredictions[3])))

      Config_Pred_TotalU[2] <- gsub(predRegex,length(dataForBaM),Config_Pred_TotalU[2])
      writeLines(Config_Pred_TotalU,paste0(DIRPATH,BAMWS,gsub(configRegex,"",ConfigPredictions[4])))

      #Write out the gaugings for the data used for the curve
      gaugingsStageData <- dischargeData[dischargeData$curveID==currCurveID,]

      #Write out the formatted data file to data folder for BaM
      gagNam <- c('H','uH','bH','bHindx','Q','uQ','bQ','bQindx')
      gaugings <- data.frame(matrix(data=NA, ncol=length(gagNam), nrow=length(gaugingsStageData$startDate)))
      names(gaugings) <- gagNam

      #Zeroes below assume no uncertainty, may want to change that
      gaugings$H <- gaugingsStageData$gaugeHeight #Stream stage values (m)
      gaugings$uH <- 0.00 #May include in the future
      gaugings$bH <- 0.00
      gaugings$bHindx <- 0.00
      gaugings$Q <- as.numeric(gaugingsStageData$streamDischarge)/1000#Convert to CMS for BaM
      gaugings$uQ <- as.numeric(gaugingsStageData$streamDischargeUnc)/1000#Convert to CMS for BaM
      gaugings$bQ <- 0.00
      gaugings$bQindx <- 0.00

      write.table(gaugings,
                  paste0(DIRPATH, BAMFOLD, dataPath),
                  sep = "\t",
                  row.names = F,
                  quote = F)

      #Write configuration and data files to the BaM folder for the water year
      Config_Data <- readLines(paste0(DIRPATH, BAMWS, DataName))
      Config_Data[3] <- gsub("[0-9]{1,6}",nrow(gaugings),Config_Data[3]) #Replace the existing value with the current value
      writeLines(Config_Data, paste0(DIRPATH, BAMWS, DataName)) #Specifies the calibration data

      #R doesn't like the trailing slash
      BaM_path <- paste0(DIRPATH,gsub("/$","",BAMFOLD))
      if(!file.exists(BaM_path)){
        failureMessage <- "Path to BaM executable not found"
        stop(failureMessage)
      }
      #Run the exe
      setwd(BaM_path)
      system2(BAMFILE)

      #Read in results files and rbind with the previous ones
      #Read in and Format the output data of the MCMC Results using the gaugings
      Qt_Maxpost_spag <- read.table(paste0(DIRPATH, BAMWS, QMaxpostSpagName), header = F)
      Qt_hU_env <- read.table(paste0(DIRPATH, BAMWS, QGaugeUncEnvName), header = T)
      Qt_TotalU_env <- read.table(paste0(DIRPATH, BAMWS, QTotalUncEnvName), header = T)

      #Change the names so there aren't duplicates
      names(Qt_Maxpost_spag) <- 'maxpostDischarge'
      names(Qt_hU_env) <- c('withParaUncQMedian',
                            'withParaUncQLower2Std',
                            'withParaUncQUpper2Std',
                            'withParaUncQLower1Std',
                            'withParaUncQUpper1Std',
                            'withParaUncQMean',
                            'withParaUncQStdDev')
      names(Qt_TotalU_env) <- c('withRemnUncQMedian',
                                'withRemnUncQLower2Std',
                                'withRemnUncQUpper2Std',
                                'withRemnUncQLower1Std',
                                'withRemnUncQUpper1Std',
                                'withRemnUncQMean',
                                'withRemnUncQStdDev')

      #Function to convert everything to liters per second
      convertToLPS <- function(x){
        x <- x*1000
        return(x)
      }
      #Convert everything to liters per second
      Qt_Maxpost_spag <- as.data.frame(apply(Qt_Maxpost_spag,c(1,2),convertToLPS))
      Qt_hU_env <- as.data.frame(apply(Qt_hU_env,c(1,2),convertToLPS))
      Qt_TotalU_env <- as.data.frame(apply(Qt_TotalU_env,c(1,2),convertToLPS))

      #Need to put the data back where it belongs since there could have been NAs stripped before modeling
      compiledOutputs <- L0PressureDataForBaM
      namesToadd <- c(names(Qt_Maxpost_spag),names(Qt_hU_env),names(Qt_TotalU_env))
      finalNames <- c(names(compiledOutputs),namesToadd)
      compiledOutputs[,(ncol(compiledOutputs)+1):(ncol(compiledOutputs)+length(namesToadd))] <- NA
      names(compiledOutputs) <- finalNames

      compiledOutputs[actualDataIdx,names(Qt_Maxpost_spag)] <- Qt_Maxpost_spag
      compiledOutputs[actualDataIdx,names(Qt_hU_env)] <- Qt_hU_env
      compiledOutputs[actualDataIdx,names(Qt_TotalU_env)] <- Qt_TotalU_env
    }

    # Clean up NULL timestamp values by removing nonsystematic and systematic uncertainty
    compiledOutputs$nonSystematicUnc[is.na(compiledOutputs$CalPress)] <- NA
    compiledOutputs$systematicUnc[is.na(compiledOutputs$CalPress)] <- NA

    # Format table like to match publication table
    continuousDischarge_names <- c(
    'siteID',
    'stationHorizontalID',
    'namedLocation',
    'endDate',
    'curveID',
    'calibratedPressure',
    'regressionID',
    'equivalentStage',
    'nonSystematicUnc',
    'systematicUnc',
    'stageUnc',
    'maxpostDischarge',
    'withParaUncQMedian',
    'withParaUncQLower2Std',
    'withParaUncQUpper2Std',
    'withParaUncQLower1Std',
    'withParaUncQUpper1Std',
    'withParaUncQMean',
    'withParaUncQStdDev',
    'withRemnUncQMedian',
    'withRemnUncQLower2Std',
    'withRemnUncQUpper2Std',
    'withRemnUncQLower1Std',
    'withRemnUncQUpper1Std',
    'withRemnUncQMean',
    'withRemnUncQStdDev')

    csd_continuousDischarge_pub <- compiledOutputs[names(compiledOutputs)%in%continuousDischarge_names]
    csd_continuousDischarge_pub$endDate <- compiledOutputs$date
    csd_continuousDischarge_pub$calibratedPressure <- compiledOutputs$CalPress
    csd_continuousDischarge_pub$equivalentStage <- compiledOutputs$calculatedStage
    csd_continuousDischarge_pub$stageUnc <- abs(csd_continuousDischarge_pub$nonSystematicUnc)*1.96 + abs(csd_continuousDischarge_pub$systematicUnc)*1.96
    csd_continuousDischarge_pub <- csd_continuousDischarge_pub[continuousDischarge_names]

    #If there is no namedLocation field due to null timestamps, add the field with NA's to POST to IS database
    if(!"namedLocation"%in%names(csd_continuousDischarge_pub)&
        length(dataForBaM)<1){
      csd_continuousDischarge_pub$namedLocation <- NA
    }

    if (length(numCurves)>1) {
      if (c==1) {
        csd_continuousDischarge_pub_c1 <- csd_continuousDischarge_pub
       }
      if (c==2) {
        csd_continuousDischarge_pub_c2 <- csd_continuousDischarge_pub
        csd_continuousDischarge_pub <- rbind(csd_continuousDischarge_pub_c1,csd_continuousDischarge_pub_c2)
        csd_continuousDischarge_pub <- csd_continuousDischarge_pub[order(csd_continuousDischarge_pub$endDate),]
      }
    }
    if (l==1) {
      csd_continuousDischarge_pubForL4 <- csd_continuousDischarge_pub
    }
    if (l>1) {
      csd_continuousDischarge_pubForL4 <- rbind(csd_continuousDischarge_pubForL4,csd_continuousDischarge_pub)
    }
  }

  write.csv(csd_continuousDischarge_pubForL4,paste0(DIRPATH,BAMWS,"continuousDischarge_",site,"_",year,"_",month,".csv"),row.names = FALSE)
}
