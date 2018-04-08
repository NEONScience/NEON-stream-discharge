##############################################################################################
#' @title Function to calculate Stage-Discharge Rating Curve

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This script uses the BaM executable to calculate the parameters that define 
#' the stage-discharge relationship for a site and water year using NEON data.

#' @importFrom neonDataStackR stackByTable

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
#' 
#' @return This function results in text files being written to a directory that contain 
#' the posterior parameters and residuals for a stage-discharge rating curve.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
##############################################################################################
calc.stag.Q.curv <- function(
  DIRPATH = Sys.getenv("DIRPATH"),
  BAMFOLD = Sys.getenv("BAMFOLD"),
  BAMFILE = Sys.getenv("BAMFILE"),
  DATAWS = Sys.getenv("DATAWS"),
  BAMWS = Sys.getenv("BAMWS"),
  startDate = Sys.getenv("STARTDATE"),
  site = Sys.getenv("SITE")
){
  
  #User inputs for site and date
  #If you enter a searchIntervalStart data that is not YYY-10-01, the script will determine the 
  #water year that started immediately before the date entered here and use it as the 
  #searchIntervalStartDate
  searchIntervalStartDate <- as.POSIXct(startDate, tz="UTC")
  downloadedDataPath <- DATAWS
  
  #Numeric Constants
  secondsInDay <- 86400
  secondsInYear <- 31536000
  
  #String Constants
  osDateFormat <- "%Y-%m-%dT%H:%M:%S.000Z"
  nameRegex <- "^\"|\" .*"
  spagRegex <- " ! .*"
  
  #Additional path information
  Config_BaM <- readLines(paste0(DIRPATH,BAMFOLD,"Config_BaM.txt"))
  
  #Check that the Config workspace matches container BaM workspace
  configWS <- paste0(BAMFOLD,gsub(nameRegex,"",Config_BaM[1]))
  if(BAMWS != configWS){
    failureMessage <- "Configuration and container workspace mis-match"
    stop(failureMessage)
  }
  
  #Parse BaM configuration files
  RunOptionsName <- gsub(nameRegex,"",Config_BaM[2])
  ModelName <- gsub(nameRegex,"",Config_BaM[3])
  ControlMatrixName <- gsub(nameRegex,"",Config_BaM[4])
  DataName <- gsub(nameRegex,"",Config_BaM[5])
  #RemnantSigmaName <- gsub(nameRegex,"",Config_BaM[6])
  MCMCName <- gsub(nameRegex,"",Config_BaM[7])
  CookingName <- gsub(nameRegex,"",Config_BaM[8])
  SummaryName <- gsub(nameRegex,"",Config_BaM[9])
  ResidualsName <- gsub(nameRegex,"",Config_BaM[10])
  
  dataPath <- gsub("^\'|\' .*","",readLines(paste0(DIRPATH,BAMWS,DataName))[1])
  
  MCMCInfo <- readLines(paste0(DIRPATH,BAMWS,MCMCName))
  cookingInfo <- readLines(paste0(DIRPATH,BAMWS,CookingName))
  
  cookedResultsName <- gsub(nameRegex,"",cookingInfo[1])
  SummaryResultsName <- gsub(nameRegex,"",readLines(paste0(DIRPATH,BAMWS,SummaryName))[1])
  ResidualResultsName <- gsub(nameRegex,"",readLines(paste0(DIRPATH,BAMWS,ResidualsName))[1])
  
  #Calculate number of spaghettis
  burnFactor <- as.numeric(gsub(spagRegex,"",cookingInfo[2]))
  Nslim <- as.numeric(gsub(spagRegex,"",cookingInfo[3]))
  NAdapt <- as.numeric(gsub(spagRegex,"",MCMCInfo[2]))
  Ncycles <- as.numeric(gsub(spagRegex,"",MCMCInfo[3]))
  numSpag <- NAdapt*Ncycles/Nslim*burnFactor
  
  #DPIDs
  l4MorphoPID <- "NEON.DOM.SITE.DP4.00131.001"
  l0DischargeDPID <- "NEON.DOM.SITE.DP0.20048.001"
  l1DischargeDPID <- "NEON.DOM.SITE.DP1.20048.001"
  l4DischargeDPID <- "NEON.DOM.SITE.DP4.00133.001"
  l4ContinuousDPID <- "NEON.DOM.SITE.DP4.00130.001"
  
  #Metadata
  waterYearDate <- def.calc.WY.strt.end.date(searchIntervalStartDate)
  searchIntervalStartDate <- waterYearDate$startDate
  #Because database queries do not include the last date add one day to searchIntervalEndDate
  searchIntervalEndDate <- waterYearDate$endDate+secondsInDay #Add one day  
  
  #Write configuration files for parameters
  txt.out.run.opts(runType = "param", RunOptionsPath = paste0(DIRPATH, BAMWS, RunOptionsName))
  controlMatrixPath <- paste0(DIRPATH, BAMWS, ControlMatrixName)
  priorParamsPath <- paste0(DIRPATH, BAMWS, ModelName)
  
  #Write out configuration files for controls
  numCtrls <- try(txt.out.ctrl.and.prior.parm.ext(site = site,
                                                  searchIntervalStartDate = searchIntervalStartDate, 
                                                  searchIntervalEndDate = searchIntervalEndDate,
                                                  controlMatrixPath = controlMatrixPath,
                                                  priorParamsPath = priorParamsPath,
                                                  downloadedDataPath = downloadedDataPath))
  if(!is.null(attr(numCtrls, "class")) && attr(numCtrls, "class") == "try-error"){
    failureMessage <- "Prior parameters or control matrix error, see transition object message"
    stop(failureMessage)
  }
  
  
  #Get stage and discharge data from the OS system
  stackByTable(dpID = "DP1.20048.001",paste0(downloadedDataPath,"NEON_discharge-stream.zip"))
  dischargeData  <- read.csv(paste(downloadedDataPath,"NEON_discharge-stream","stackedFiles","dsc_individualFieldData.csv", sep = "/"))
  stageData <- read.csv(paste(downloadedDataPath,"NEON_discharge-stream","stackedFiles","dsc_fieldData.csv", sep = "/"))
  
  #Filter stage and discharge data to just include the site of interest
  dischargeData <- dischargeData[dischargeData$siteID == site,]
  stageData <- stageData[stageData$siteID == site,]
  
  if(length(stageData)<1){
    stop(paste0("Zero records available from ", l1DischargeDPID,":dsc_fieldData_pub"))
  }
  if(length(dischargeData)<1){
    stop(paste0("Zero records available from ", l1DischargeDPID,":dsc_individualFieldData_pub"))
  }
  if(attr(dischargeData, "class") == "try-error"){
    failureMessage <- "Discharge point data could not be retrieved"
    stop(failureMessage)
  }
  if(attr(stageData, "class") == "try-error"){
    failureMessage <- "Stage data could not be retrieved"
    stop(failureMessage)
  }
  
  #Convert units and re-calculate discharge
  stageData <- try(conv.calc.Q(stageData = stageData, dischargeData = dischargeData))
  if(attr(stageData, "class") == "try-error"){
    failureMessage <- "Error while recalculating flow from point measurements, check L1 field data"
    stop(failureMessage)
  }
  stageData$eventID <- paste(stageData$siteID, gsub("-|T.*","",stageData$startDate), sep = ".")
  
  #Apply staff gauge offsets
  gaugeLocData <- read.csv(paste0(downloadedDataPath,"locationHistories.csv"))
  
  #Just keep data for site of interest
  gaugeLocData <- gaugeLocData[gaugeLocData$site == site,]
  
  #Remove NA rows
  gaugeLocData <- gaugeLocData[!is.na(gaugeLocData$startDate),]
  
  if(length(gaugeLocData)<1){
    failureMessage <- "Zero (0) gauge location history records were retrieved"
    stop(failureMessage)
  }
  if(attr(gaugeLocData, "class") == "try-error"){
    failureMessage <- "Gauge location history could not be retrieved"
    stop(failureMessage)
  }
  
  gaugeLocData$endDate <- as.POSIXct(gaugeLocData$endDate, tz = "UTC")
  gaugeLocData$startDate <- as.POSIXct(gaugeLocData$startDate, tz = "UTC")
  gaugeLocData$xOffset <- as.numeric(gaugeLocData$xOffset)
  #Need to test both null and na since if there is only one row it comes back as null, >1 NA
  #Add an endDate to avoid NA and null situations
  if(is.null(gaugeLocData$endDate)){
    gaugeLocData$endDate[is.null(gaugeLocData$endDate)] <- Sys.Date()}
  if(any(is.na(gaugeLocData$endDate))){
    gaugeLocData$endDate[is.na(gaugeLocData$endDate)] <- Sys.Date()}
  
  #Keep the location information that overlaps the WY or extends before and after the WY
  gaugeLocData <- gaugeLocData[(gaugeLocData$startDate > searchIntervalStartDate & gaugeLocData$startDate < searchIntervalEndDate)|
                                 (gaugeLocData$endDate > searchIntervalStartDate & gaugeLocData$endDate < searchIntervalEndDate)|
                                 (gaugeLocData$startDate < searchIntervalStartDate & gaugeLocData$endDate > searchIntervalEndDate),]
  
  #Apply offsets for all ranges of gauge data
  stageData$gaugeHeightOffset <- NA
  for(i in 1:length(gaugeLocData$startDate)){
    locStart <- gaugeLocData$startDate[i]
    locEnd <- gaugeLocData$endDate[i]
    locOffset <- gaugeLocData$zOffset[i]
    dataToApplyOffset <- stageData$streamStage[stageData$startDate>=locStart & stageData$endDate<locEnd]
    
    # Need to add error handling here once we can have gaps in the location data 
    # to make sure the data is from when we had a staff gauge installed
    
    stageData$streamStage[stageData$startDate>=locStart & stageData$endDate<locEnd] <- 
      dataToApplyOffset + as.numeric(locOffset)
    stageData$gaugeHeightOffset[stageData$startDate>=locStart & stageData$endDate<locEnd] <- 
      as.numeric(locOffset)
  }
  
  #Now loop through each curve to determine the date ranges
  numCurves <- length(gaugeLocData$startDate[gaugeLocData$xOffset > 0])
  numCurvesToRemove <- 0
  combinePrevCurve <- FALSE
  beforeCurveStartDate <- min(gaugeLocData$startDate) - 1
  curveNumberForID <- 0
  
  if(numCurves == 0){
    numCurves <- 1
    combinePrevCurve <- TRUE
  }
  
  curveData_Names <- c(
    'allEventID',
    'curveID',
    'curveStartDate',
    'curveEndDate',
    'minQ',
    'maxQ',
    'minStage',
    'maxStage'
  )
  curveData <- data.frame(matrix(data=NA, ncol=length(curveData_Names), nrow=numCurves))
  names(curveData) <- curveData_Names
  
  for(i in 1:numCurves){
    #For when you need to combine with the previous time range
    #Use the previous curveStartDate, but move curveIdx to the next one
    if(combinePrevCurve && numCurves > 1){
      curveIdx <- which(gaugeLocData$startDate == min(gaugeLocData$startDate[gaugeLocData$startDate >= beforeCurveStartDate]))
    }else if(combinePrevCurve && numCurves == 1){
      curveStartDate <- searchIntervalStartDate
      curveIdx <- which(gaugeLocData$startDate == min(gaugeLocData$startDate[gaugeLocData$startDate >= beforeCurveStartDate]))
    }else{
      #Curve start date is the later of the earliest gauge loc startDate or searchIntervalStartDate
      ifelse(searchIntervalStartDate > min(gaugeLocData$startDate[gaugeLocData$startDate >= beforeCurveStartDate]),
             curveStartDate <- as.POSIXct(searchIntervalStartDate, tz = "UTC"),
             curveStartDate <- min(gaugeLocData$startDate[gaugeLocData$startDate >= beforeCurveStartDate]))
      curveIdx <- which(gaugeLocData$startDate == curveStartDate|
                          gaugeLocData$startDate == min(gaugeLocData$startDate[gaugeLocData$startDate >= beforeCurveStartDate]))
    }
    
    #Find the curve end date, combine the locations with offsets for the water year
    curveEndDate <- gaugeLocData$endDate[curveIdx]
    if(length(gaugeLocData$endDate[gaugeLocData$endDate > curveEndDate]) > 0){
      for(j in 1:length(gaugeLocData$startDate)){
        #If there is a later date
        if(length(gaugeLocData$endDate[gaugeLocData$endDate > curveEndDate]) > 0){
          nextEndDate <- min(gaugeLocData$endDate[gaugeLocData$endDate > curveEndDate], na.rm = T)
          endDateIdx <- which(gaugeLocData$endDate == nextEndDate)
        }
        #Don't combine gauge readings for a time range if xOffset is > 0
        if(gaugeLocData$xOffset[endDateIdx] > 0){
          break
        }
        curveEndDate <- nextEndDate
      }
    }
    
    #Use searchIntervalEndDate if it's earlier than the curveEndDate
    if(searchIntervalEndDate < curveEndDate){
      curveEndDate <- searchIntervalEndDate - secondsInDay
    }
    
    #If gaugeLocData$xOffset > 0, major move requiring new curve
    if(gaugeLocData$xOffset[curveIdx] > 0 && (curveStartDate != searchIntervalStartDate || combinePrevCurve)){
      print("Initial gauge location or undetermined offset from previous gauge location and mid-water year change in gauge location, using only recent data")
      inclPrevData <- FALSE
    }else{
      #Get the previous rating curve from the OS system
      oneYearAgo <- searchIntervalStartDate-secondsInYear
      lastWaterYear <- def.calc.WY.strt.end.date(oneYearAgo)
      stackByTable(dpID = "DP4.00133.001",paste0(downloadedDataPath,"NEON_discharge-stream-rating-curve.zip"))
      prevRatingCurve  <- try(read.csv(paste(downloadedDataPath,
                                             "NEON_discharge-stream-rating-curve",
                                             "stackedFiles",
                                             "sdrc_stageDischargeCurveInfo.csv", 
                                             sep = "/")))
      prevRatingCurve <- prevRatingCurve[prevRatingCurve$siteID == site,]
      if(length(prevRatingCurve)<1){
        print("Zero (0) previous rating curve records retrieved, using only recent data")
        inclPrevData <- FALSE
      }else if(attr(prevRatingCurve, "class") == "try-error"){
        print("Previous rating curve data could not be retrieved, using only recent data")
        inclPrevData <- FALSE
      }else{
        print("Previous rating curve data retrieved")
        inclPrevData <- TRUE
      }
    }
    
    #Just use stageData up until end of the curve
    stageDataForCurve <- stageData[stageData$startDate >= curveStartDate & stageData$collectDate <= curveEndDate,]
    
    if(length(stageDataForCurve$startDate) < 1){
      print(paste0("No additional stage records for ", curveStartDate, " to ", curveEndDate, " combining with next time period"))
      combinePrevCurve <- TRUE
      beforeCurveStartDate <- curveStartDate + 1
      numCurvesToRemove = numCurvesToRemove + 1
      next
    }
    
    #Reset the combine for the next loop now that we are proceeding with this curve
    combinePrevCurve <- FALSE
    curveNumberForID <- curveNumberForID + 1
    
    #Find the most recent rating curve and all the eventIDs of data that went into it
    #Combine the previous rating curve with the new recalculated data for gaugings 
    #only, stageData gets put into the database and we don't want to duplicate the old data
    if(inclPrevData){
      mostRecentCurveID <- prevRatingCurve$curveID[
        which(prevRatingCurve$endDate == 
                max(prevRatingCurve$endDate[prevRatingCurve$endDate < searchIntervalEndDate], na.rm = T))]
      
      #Use real dates instead of strings
      gaugeStartDate <- unique(prevRatingCurve$startDate[prevRatingCurve$curveID == mostRecentCurveID])
      gaugeStartDate <- as.POSIXct(gaugeStartDate, tz = "UTC")
      gaugeEndDate <- unique(prevRatingCurve$endDate[prevRatingCurve$curveID == mostRecentCurveID])
      gaugeEndDateSearch <- as.POSIXct(gaugeEndDate) + secondsInDay
      
      if(length(gaugeStartDate)>1|length(gaugeEndDateSearch)>1){
        failureMessage <- "More that one previous rating curve present, i.e. duplicates, for the latest time period"
        stop(failureMessage)
      }
      
      prevGaugeData  <- try(read.csv(paste(downloadedDataPath,
                                           "NEON_discharge-stream-rating-curve",
                                           "stackedFiles",
                                           "sdrc_gaugeDischargeMeas.csv", 
                                           sep = "/")))
      #Make sure you're using only the data for the current site
      prevGaugeData <- prevGaugeData[prevGaugeData$siteID == site,]
      
      #Make sure you aren't creating duplicates with a re-run of the same rating curve
      allPrevEventID <- strsplit(prevRatingCurve$allEventID[prevRatingCurve$curveID == mostRecentCurveID],"\\|")
      prevGaugeData <- prevGaugeData[prevGaugeData$eventID %in% allPrevEventID[[1]],]
      prevGaugeData <- prevGaugeData[!(prevGaugeData$eventID %in% stageDataForCurve$eventID),]
      
      if(length(prevGaugeData)<1){
        failureMessage <- "Zero (0) records for previous year's rating curve gauge data retrieved"
        stop(failureMessage)
      }
      if(attr(prevGaugeData, "class") == "try-error"){
        failureMessage <- "Previous year's rating curve gauge data could not be retrieved"
        stop(failureMessage)
      }
      
      
      #Merge the data frames
      names(prevGaugeData)[which(names(prevGaugeData) == "streamDischarge")] <- "calcQ"
      names(prevGaugeData)[which(names(prevGaugeData) == "gaugeHeight")] <- "streamStage"
      
      gaugingsStageData <- merge(stageDataForCurve, 
                                 prevGaugeData, 
                                 by = intersect(names(stageData), names(prevGaugeData)), 
                                 all = TRUE)
      allEventID <- paste(gaugingsStageData$eventID, collapse = "|")
    }else{
      gaugingsStageData <- stageDataForCurve
      allEventID <- paste(stageDataForCurve$eventID, collapse = "|")
    }
    
    #Write out the formatted data file to data folder for BaM
    gagNam <- c('H','uH','bH','bHindx','Q','uQ','bQ','bQindx')
    gaugings <- data.frame(matrix(data=NA, ncol=length(gagNam), nrow=length(gaugingsStageData$startDate)))
    names(gaugings) <- gagNam
    
    #Zeroes below assume no uncertainty, may want to change that
    gaugings$H <- gaugingsStageData$streamStage #Stream stage values (m)
    gaugings$uH <- 0.00 #May include in the future
    gaugings$bH <- 0.00
    gaugings$bHindx <- 0.00
    gaugings$Q <- gaugingsStageData$calcQ #Stream discharge values (lps), re-calculated
    gaugings$uQ <- as.numeric(gaugingsStageData$calcQ) * 0.1
    gaugings$bQ <- 0.00
    gaugings$bQindx <- 0.00
    
    write.table(gaugings,
                paste0(DIRPATH, BAMFOLD, dataPath),
                sep = "\t",
                row.names = F,
                quote = F)
    
    #print(paste0("Gaugings.txt exists: ", file.exists(paste0(DIRPATH, BAMFOLD, dataPath))))
    
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
    
    #Check for the output files
    if(!file.exists(paste0(DIRPATH, BAMWS, cookedResultsName))){
      failureMessage <- "Cooked Results file was not created by BaM"
      stop(failureMessage)
    }
    if(!file.exists(paste0(DIRPATH, BAMWS, SummaryResultsName))){
      failureMessage <- "Results Summary file was not created by BaM"
      stop(failureMessage)
    }
    if(!file.exists(paste0(DIRPATH, BAMWS, ResidualResultsName))){
      failureMessage <- "Results Residuals file was not created by BaM"
      stop(failureMessage)
    }
    
    #Read in and Format the output data of the MCMC Results using the gaugings
    Results_MCMC_Cooked_in <- read.table(paste0(DIRPATH, BAMWS, cookedResultsName), header = T)
    Results_Residuals_in <- read.table(paste0(DIRPATH, BAMWS, ResidualResultsName), header = T)
    Results_Residuals_in$eventID <- gaugingsStageData$eventID
    Results_Summary_in <- read.table(paste0(DIRPATH, BAMWS, SummaryResultsName), header = T)
    
    #Assign curveIDs
    curveIDString <- paste(site,format(searchIntervalEndDate, format = "%Y"),curveNumberForID,sep = ".")
    Results_MCMC_Cooked_in$curveID <- curveIDString
    Results_Residuals_in$curveID <- curveIDString
    Results_Summary_in$curveID <- curveIDString
    
    #Add curve start and end dates
    Results_MCMC_Cooked_in$curveStartDate <- curveStartDate
    Results_Residuals_in$curveStartDate <- curveStartDate
    Results_Summary_in$curveStartDate <- curveStartDate
    Results_MCMC_Cooked_in$curveEndDate <- curveEndDate
    Results_Residuals_in$curveEndDate <- curveEndDate
    Results_Summary_in$curveEndDate <- curveEndDate
    
    #Add spaghetti number
    Results_MCMC_Cooked_in$parameterNumber <- row.names(Results_MCMC_Cooked_in)
    
    #Fill in record in 
    curveData$allEventID[i] <- allEventID
    curveData$curveID[i] <- curveIDString
    curveData$minQ[i] <- min(as.numeric(gaugings$Q))
    curveData$maxQ[i] <- max(as.numeric(gaugings$Q))
    curveData$minStage[i] <- min(as.numeric(gaugings$H))
    curveData$maxStage[i] <- max(as.numeric(gaugings$H))
    curveData$curveStartDate[i] <- format(curveStartDate, format = osDateFormat)
    curveData$curveEndDate[i] <- format(curveEndDate, format = osDateFormat)
    
    if(exists("Results_MCMC_Cooked") && exists("Results_Residuals") && exists("Results_Summary")){
      Results_MCMC_Cooked <- rbind(Results_MCMC_Cooked, Results_MCMC_Cooked_in)
      Results_Residuals <- rbind(Results_Residuals, Results_Residuals_in)
      Results_Summary <- rbind(Results_Summary, Results_Summary_in)
      rm("Results_MCMC_Cooked_in")
      rm("Results_Residuals_in")
      rm("Results_Summary_in")
    }else{
      Results_MCMC_Cooked <- Results_MCMC_Cooked_in
      Results_Residuals <- Results_Residuals_in
      Results_Summary <- Results_Summary_in
    }
    
    beforeCurveStartDate <- curveEndDate
  }
  
  #Remove NA values from the uuid list and curveData
  curveData <- curveData[!is.na(curveData$curveID),]
  
  #Create the transition metadata list
  domain <- unique(gaugeLocData$domain)
  namedLocationName <- site
  numCurves <- numCurves - numCurvesToRemove
  startDateFormatted <- format(searchIntervalStartDate, format=osDateFormat)
  endDateFormatted <- format(waterYearDate$endDate, format=osDateFormat)
  searchIntervalEndDateFormatted <- format(searchIntervalEndDate, format=osDateFormat)
  waterYear <- format(searchIntervalEndDate, format = "%Y")
  tran.metadata <- list(domain,
                        site,
                        startDateFormatted,
                        endDateFormatted,
                        searchIntervalEndDateFormatted,
                        namedLocationName,
                        numCtrls,
                        numCurves,
                        numSpag,
                        waterYear)
  names(tran.metadata) <- c("domain",
                            "site",
                            "startDate",
                            "endDate",
                            "searchIntervalEndDate",
                            "namedLocationName",
                            "numCtrls",
                            "numCurves",
                            "numSpag",
                            "waterYear")
  
  #Format results for transition object to fit into NEON tables
  #Doesn't need curve specific information, just start and end dates that match the water year
  print("Formatting gaugeDischargeMeas")
  gaugeDischargeMeas_outputDF <- frmt.gaug.disc.mea.file(dataFrame = stageData, 
                                                         metadata = tran.metadata)
  write.csv(gaugeDischargeMeas_outputDF,paste0(DIRPATH,BAMWS,"gaugeDischargeMeas.csv"),row.names = FALSE)
  
  #Format results for transition object
  print("Formatting posteriorParameters")
  posteriorParameters_outputDF <- frmt.post.parm.file(dataFrame = Results_Summary, 
                                                      metadata = tran.metadata)
  write.csv(posteriorParameters_outputDF,paste0(DIRPATH,BAMWS,"posteriorParameters.csv"),row.names = FALSE)
  
  #Format results for transition object
  print("Formatting stageDischargeCurveInfo")
  stageDischargeCurveInfo_outputDF <- frmt.stag.disc.curv.info.file(dataFrame = curveData, 
                                                                    metadata = tran.metadata)
  write.csv(stageDischargeCurveInfo_outputDF,paste0(DIRPATH,BAMWS,"stageDischargeCurveInfo.csv"),row.names = FALSE)
  
  #Format results for transition object
  print("Formatting sampledParameters")
  sampledParameters_outputDF <- frmt.samp.parm.file(dataFrame = Results_MCMC_Cooked, 
                                                    metadata = tran.metadata)
  write.csv(sampledParameters_outputDF,paste0(DIRPATH,BAMWS,"sampledParameters.csv"),row.names = FALSE)
  
  #Format results for transition object
  print("Formatting resultsResiduals")
  resultsResiduals_outputDF <- frmt.rslt.resd.file(dataFrame = Results_Residuals, 
                                                   metadata = tran.metadata)
  write.csv(resultsResiduals_outputDF,paste0(DIRPATH,BAMWS,"resultsResiduals.csv"),row.names = FALSE)

}
  

  
  
