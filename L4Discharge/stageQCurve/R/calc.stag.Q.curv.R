##############################################################################################
#' @title Function to calculate Stage-Discharge Rating Curve

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description This script uses the BaM executable to calculate the parameters that define
#' the stage-discharge relationship for a site and water year using NEON data.

#' @importFrom neonUtilities stackByTable

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
#   Zachary L. Nickerson (2021-04-05)
#     Major updates to overall workflow to match internal NEON rating curve development workflow
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

  # User inputs for site and date
  # If you enter a searchIntervalStartDate that is not YYYY-10-01, the script will determine the water year (10-01 - 09-30) that started immediately before the date entered here and use it as the startDate
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
  l4BathDPID <- "NEON.DOM.SITE.DP4.00132.001"
  l1DischargeDPID <- "NEON.DOM.SITE.DP1.20048.001"
  l4DischargeDPID <- "NEON.DOM.SITE.DP4.00133.001"

  #Metadata
  waterYearDate <- stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate)
  searchIntervalStartDate <- waterYearDate$startDate
  #Because database queries do not include the last date add one day to searchIntervalEndDate
  searchIntervalEndDate <- waterYearDate$endDate+secondsInDay #Add one day

  #Get stage and discharge data from the OS system
  if(file.exists(paste0(downloadedDataPath,"NEON_discharge-rating-curves"))){
    # neonUtilities::stackByTable(dpID = "DP4.00133.001",paste0(downloadedDataPath,"NEON_discharge-rating-curves.zip"))
    dischargeData  <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles","sdrc_gaugeDischargeMeas.csv", sep = "/")),silent = T)
    curveIdentification <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles","geo_curveIdentification.csv", sep = "/")),silent = T)
  }else{
    availableFiles <- list.files(downloadedDataPath)
    dischargeData  <- try(read.csv(paste(downloadedDataPath,availableFiles[grepl("sdrc_gaugeDischargeMeas",availableFiles)], sep = "/")),silent = T)
    curveIdentification  <- try(read.csv(paste(downloadedDataPath,availableFiles[grepl("geo_curveIdentification",availableFiles)], sep = "/")),silent = T)
  }

  # Error handling if no discharge or curve identification data can be found
  if(attr(curveIdentification, "class") == "try-error"){
    failureMessage <- "Data could not be retrieved from geo/bat_curveIdentification_pub"
    stop(failureMessage)
  }
  if(attr(dischargeData, "class") == "try-error"){
    failureMessage <- "Data could not be retrieved from sdrc_gaugeDischargeMeas_pub"
    stop(failureMessage)
  }

  # Remove any curve IDs that shouldn't be included in this site or water year
  curveIdentification <- curveIdentification[grepl(format(waterYearDate$endDate,"%Y"),curveIdentification$curveID)&curveIdentification$siteID==site,]
  curveIdentification <- curveIdentification[order(curveIdentification$curveID),]
  curveIDSegment <- unique(curveIdentification$curveID)[order(unique(curveIdentification$curveID))]
  numCurves <- length(curveIDSegment)

  curveData_Names <- c(
    'allEventID',
    'curveID',
    'curveSegmentID',
    'curveStartDate',
    'curveEndDate',
    'minQ',
    'maxQ',
    'minStage',
    'maxStage'
  )
  curveData <- data.frame(matrix(data=NA, ncol=length(curveData_Names), nrow=numCurves))
  names(curveData) <- curveData_Names

  # Run the BaM executable and generate all the tables needed for the stage-discharge rating curves data product
  for(i in 1:numCurves){
    i=1
    curveIDString <- curveIDSegment[i]

    # Write configuration files for parameters
    stageQCurve::txt.out.run.opts(runType = "param", RunOptionsPath = paste0(DIRPATH, BAMWS, RunOptionsName))
    controlMatrixPath <- paste0(DIRPATH, BAMWS, ControlMatrixName)
    priorParamsPath <- paste0(DIRPATH, BAMWS, ModelName)

    # Write out configuration files for controls
    controlSurveyDate <- format(as.POSIXct(curveIdentification$controlSurveyEndDateTime[curveIdentification$curveID==curveIDString]),"%Y-%m-%d")
    numCtrls <- try(txt.out.ctrl.and.prior.parm.ext(site = site,
                                                    controlSurveyDate = controlSurveyDate,
                                                    controlMatrixPath = controlMatrixPath,
                                                    priorParamsPath = priorParamsPath,
                                                    downloadedDataPath = downloadedDataPath),silent = T)
    if(!is.null(attr(numCtrls, "class")) && attr(numCtrls, "class") == "try-error"){
      failureMessage <- "Prior parameters or control matrix error; check inputs"
      stop(failureMessage)
    }

    # Work only with the gauge and discharge records within this rating segment
    dischargeDataForCurve <- dischargeData[dischargeData$curveID%in%curveIDString,]

    # Set the curve start and end date and get list of all gauge event IDs
    curveStartDate <- curveIdentification$curveStartDate[curveIdentification$curveID==curveIDString][1]
    curveEndDate <- curveIdentification$curveEndDate[curveIdentification$curveID==curveIDString][1]
    allEventID <- paste(dischargeDataForCurve$gaugeEventID, collapse = "|")

    # Write out the formatted data file to data folder for BaM
    gagNam <- c('H','uH','bH','bHindx','Q','uQ','bQ','bQindx')
    gaugings <- data.frame(matrix(data=NA, ncol=length(gagNam), nrow=length(dischargeDataForCurve$startDate)))
    names(gaugings) <- gagNam

    # Zeroes below assume no uncertainty, may want to change that
    gaugings$H <- dischargeDataForCurve$gaugeHeight #Stream stage values (m)
    gaugings$uH <- 0.00 #May include in the future
    gaugings$bH <- 0.00
    gaugings$bHindx <- 0.00
    # Stream discharge values (lps), re-calculated divided by 1000 to convert to m^3/s for BaM executable
    gaugings$Q <- as.numeric(dischargeDataForCurve$streamDischarge)/1000
    # 10% uncertainty (half-length of 95 % confiddence interval), divided by 1.96 to get to one std dev
    gaugings$uQ <- gaugings$Q * 0.1 / 1.96
    gaugings$bQ <- 0.00
    gaugings$bQindx <- 0.00

    # Write out gaugings to input to BaM executable
    write.table(gaugings,
                paste0(DIRPATH, BAMFOLD, dataPath),
                sep = "\t",
                row.names = F,
                quote = F)

    # Write configuration and data files to the BaM folder for the water year
    Config_Data <- readLines(paste0(DIRPATH, BAMWS, DataName))
    Config_Data[3] <- gsub("[0-9]{1,6}",nrow(gaugings),Config_Data[3]) #Replace the existing value with the current value
    writeLines(Config_Data, paste0(DIRPATH, BAMWS, DataName)) #Specifies the calibration data

    # R doesn't like the trailing slash
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
    Results_Residuals_in$eventID <- dischargeDataForCurve$eventID
    Results_Summary_in <- read.table(paste0(DIRPATH, BAMWS, SummaryResultsName), header = T)

    #Assign curveIDs
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

    # Combine stage data frames to writing to L4 table. This will ensure any stage records used in >1 curve segment will have a unique record per curve
    if (i == 1) {
      dischargeDataForL4 <- dischargeDataForCurve
    }
    if (i > 1) {
      dischargeDataForL4 <- rbind(dischargeDataForL4,dischargeDataForCurve)
    }
  }

  #If any ratings have segments, add a record to curveData and change date ranges in remaining L4 tables
  #Get a list of ratings that are segmented
  segmentedRatingCurves <- unique(curveIdentification$curveID[!is.na(curveIdentification$curveSegmentID)])
  if (length(segmentedRatingCurves)>=1) {
    for (i in 1:length(segmentedRatingCurves)) {
      curveIDSegment <- segmentedRatingCurves[i]
      #Add a duplicated row of data to the L4 curveData table and add segment IDs to the L4 curveData table
      curveDataSegmentRow <- curveData[curveData$curveID==curveIDSegment,]
      curveData$curveSegmentID[curveData$curveID==curveIDSegment] <- curveIdentification$curveSegmentID[curveIdentification$curveID==curveIDSegment&curveIdentification$curveEndDate==curveData$curveEndDate[curveData$curveID==curveIDSegment]]
      curveDataSegmentRow$curveSegmentID <- unique(curveIdentification$curveSegmentID[!is.na(curveIdentification$curveSegmentID)&curveIdentification$curveSegmentID!=curveData$curveSegmentID[curveData$curveID==curveIDSegment]])
      curveDataSegmentRow$curveStartDate <- curveIdentification$curveStartDate[curveIdentification$curveID==curveIDSegment&curveIdentification$curveSegmentID==curveDataSegmentRow$curveSegmentID]
      curveDataSegmentRow$curveEndDate <- curveIdentification$curveEndDate[curveIdentification$curveID==curveIDSegment&curveIdentification$curveSegmentID==curveDataSegmentRow$curveSegmentID]
      curveData <- rbind(curveData,curveDataSegmentRow)
      # Extend curveStartDate and curveEndDate for the remaining L4 table to span the length of the curve
      Results_MCMC_Cooked$curveStartDate[Results_MCMC_Cooked$curveID==curveIDSegment] <- min(curveData$curveStartDate[curveData$curveID==curveIDSegment])
      Results_MCMC_Cooked$curveEndDate[Results_MCMC_Cooked$curveID==curveIDSegment] <- max(curveData$curveEndDate[curveData$curveID==curveIDSegment])
      Results_Residuals$curveStartDate[Results_Residuals$curveID==curveIDSegment] <- min(curveData$curveStartDate[curveData$curveID==curveIDSegment])
      Results_Residuals$curveEndDate[Results_Residuals$curveID==curveIDSegment] <- max(curveData$curveEndDate[curveData$curveID==curveIDSegment])
      Results_Summary$curveStartDate[Results_Summary$curveID==curveIDSegment] <- min(curveData$curveStartDate[curveData$curveID==curveIDSegment])
      Results_Summary$curveEndDate[Results_Summary$curveID==curveIDSegment] <- max(curveData$curveEndDate[curveData$curveID==curveIDSegment])
    }
  }

  #Create the transition metadata list
  domain <- unique(curveIdentification$domainID)
  namedLocationName <- site
  numCurves <- numCurves
  startDateFormatted <- format(min(dischargeData$startDate), format=osDateFormat)
  endDateFormatted <- format(max(dischargeData$startDate), format=osDateFormat)
  searchIntervalEndStartFormatted <- format(searchIntervalStartDate, format=osDateFormat)
  searchIntervalEndDateFormatted <- format(searchIntervalEndDate, format=osDateFormat)
  waterYear <- format(searchIntervalEndDate, format = "%Y")
  tran.metadata <- list(domain,
                        site,
                        startDateFormatted,
                        endDateFormatted,
                        searchIntervalEndStartFormatted,
                        searchIntervalEndDateFormatted,
                        curveStartDate,
                        curveEndDate,
                        namedLocationName,
                        numCtrls,
                        numCurves,
                        numSpag,
                        waterYear)
  names(tran.metadata) <- c("domain",
                            "site",
                            "startDate",
                            "endDate",
                            "searchIntervalStartDate",
                            "searchIntervalEndDate",
                            "curveStartDate",
                            "curveEndDate",
                            "namedLocationName",
                            "numCtrls",
                            "numCurves",
                            "numSpag",
                            "waterYear")

  #Format results for transition object to fit into NEON tables
  #Doesn't need curve specific information, just start and end dates that match the water year
  print("Formatting gaugeDischargeMeas")
  gaugeDischargeMeas_outputDF <- stageQCurve::frmt.gaug.disc.mea.file(dataFrame = dischargeDataForL4,
                                                                         metadata = tran.metadata,
                                                                         curveIDData = curveIdentification)
  write.csv(gaugeDischargeMeas_outputDF,paste0(DIRPATH,BAMWS,"gaugeDischargeMeas.csv"),row.names = FALSE)

  #Format results for transition object
  print("Formatting posteriorParameters")
  posteriorParameters_outputDF <- stageQCurve::frmt.post.parm.file(dataFrame = Results_Summary,
                                                      metadata = tran.metadata)
  write.csv(posteriorParameters_outputDF,paste0(DIRPATH,BAMWS,"posteriorParameters.csv"),row.names = FALSE)

  #Format results for transition object
  print("Formatting stageDischargeCurveInfo")
  stageDischargeCurveInfo_outputDF <- stageQCurve::frmt.stag.disc.curv.info.file(dataFrame = curveData,
                                                                    metadata = tran.metadata)
  write.csv(stageDischargeCurveInfo_outputDF,paste0(DIRPATH,BAMWS,"stageDischargeCurveInfo.csv"),row.names = FALSE)

  #Format results for transition object
  print("Formatting sampledParameters")
  sampledParameters_outputDF <- stageQCurve::frmt.samp.parm.file(dataFrame = Results_MCMC_Cooked,
                                                    metadata = tran.metadata)
  write.csv(sampledParameters_outputDF,paste0(DIRPATH,BAMWS,"sampledParameters.csv"),row.names = FALSE)

  #Format results for transition object
  print("Formatting resultsResiduals")
  resultsResiduals_outputDF <- stageQCurve::frmt.rslt.resd.file(dataFrame = Results_Residuals,
                                                   metadata = tran.metadata)
  write.csv(resultsResiduals_outputDF,paste0(DIRPATH,BAMWS,"resultsResiduals.csv"),row.names = FALSE)

}




