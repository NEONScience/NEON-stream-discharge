##############################################################################################
#' Download and Wrangle DP4.00130.001 and DP4.00133.001 Data from NEON Portal API

#' @name get.cont.Q.NEON.API

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr
#' James M. Ross \email{ross.james94@gmail.com} \cr

#' @description  This function will download data from the NEON Portal API using neonUtilities
#' based on user input and wrangle the data to create a summary table smoothed to 20 min
#' temporal resolution.

#' @param site.id Required: NEON AQU site ID selected by the shiny app user [string]
#' @param start.date Required: Search interval start date (YYYY-MM-DD) selected by the shiny
#' app user [string]
#' @param end.date Required: Search interval end date (YYYY-MM-DD) selected by the shiny app
#' user [string]
#' @param api.token Defaults to NA: NEON API personal access token. Learn about API tokens at
#' https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-tokens-tutorial [string]
#' @param include.q.stats Defaults to FALSE: Include values for 3x median discharge and 25-75%
#' flow in the outputs. Statistics are calculated from the time range selected by the user and
#' exclude records that contain a science review quality flag (dischargeFinalQFSciRvw) [boolean]

#' @return Returns a list of:
#' 1) 'continuousDischarge_sum' is a data frame that contains continuous stage and discharge at
#' 20 min intervals, discrete stage and discharge, and quality flags (finalDischargeQF and
#' dischargeFinalQFSciRvw from DP4.00130.001),
#' 2) 'curveIDs' is a vector of all the unique rating curve IDs that are used to build the
#' timeseries data,
#' 4) 'histMedQYearRange' is a list of 2 items: the min and max year from which the historic
#' median discharge is calculated, and
#' 3) 'dischargeStats' is either a list of 3 values (3x median discharge, 25% discharge, 75%
#' discharge) if include.q.stats = TRUE, or NA if include.q.stats = FALSE.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export get.cont.Q.NEON.API

# changelog and author contributions / copyrights
#   Zachary L. Nickerson (2022-06-20)
#     original creation
#   James M. Ross (2022-06-29)
#     added NEON API Token functionality
##############################################################################################
# # Source packages and set options
options(stringsAsFactors = F)

get.cont.Q.NEON.API <-function(site.id,start.date,end.date,api.token=NA,include.q.stats=F){

  if(missing(site.id)){
    stop('must provide site.id for neonUtilities pull')
  }
  if(missing(start.date)){
    stop('must provide start.date for neonUtilities pull')
  }
  if(missing(end.date)){
    stop('must provide end.date for neonUtilities pull')
  }

  # Rating curve data queries need to span an entire water year to ensure we are getting all the appropriate data
  searchIntervalStartDate <- base::as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = start.date)$startDate)
  searchIntervalEndDate <- base::as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = end.date)$endDate)

  # Set site variables (special considerations for TOOK)
  if (stringr::str_detect(site.id,"TOOK")) {
    site <- "TOOK"
  }else{
    site <- site.id
  }

  # Get continuous discharge data from the NEON API
  DP4.00130.001 <- neonUtilities::loadByProduct(
    dpID="DP4.00130.001",
    package = "expanded",
    check.size = F,
    site = site,
    startdate = base::format(base::as.POSIXct(start.date),"%Y-%m"),
    enddate = base::format(base::as.POSIXct(end.date),"%Y-%m"),
    token = api.token)

  # Get rating curve data from the NEON API
  if(site!="TOMB"){
    DP4.00133.001 <- neonUtilities::loadByProduct(
      dpID="DP4.00133.001",
      package = "basic",
      check.size = F,
      site = site,
      tabl = "sdrc_gaugeDischargeMeas",
      token = api.token)

  #precipitation data from the NEON API
  DP1.00006.001 <- loadByProduct(
    dpID="DP1.00006.001",
    package = "basic",
    check.size = F,
    site = site,
    startdate = base::format(base::as.POSIXct(start.date),"%Y-%m"),
    enddate = base::format(base::as.POSIXct(end.date),"%Y-%m"),
    token = api.token)

    # Format gauge-discharge measurement data
    sdrc_gaugeDischargeMeas <- DP4.00133.001$sdrc_gaugeDischargeMeas
    if (site.id=="TOOK_inlet") {
      sdrc_gaugeDischargeMeas <- sdrc_gaugeDischargeMeas%>%
        dplyr::filter(stringr::str_detect(curveID,"TKIN"))
    }else{
      if(site.id=="TOOK_outlet"){
        sdrc_gaugeDischargeMeas <- sdrc_gaugeDischargeMeas%>%
          dplyr::filter(stringr::str_detect(curveID,"TKOT"))
      }
    }
    sdrc_gaugeDischargeMeas <- sdrc_gaugeDischargeMeas%>%
      tidyr::separate(gaugeEventID,c("site","date"),5,remove = F)%>%
      dplyr::mutate(date=base::paste0(base::as.Date(date,format="%Y%m%d")," 20:00:00"))%>%
      dplyr::select(date,gaugeHeight,streamDischarge)%>%
      dplyr::distinct()%>%
      dplyr::filter(date>=start.date&date<=end.date)
    sdrc_gaugeDischargeMeas$date <- base::as.character(sdrc_gaugeDischargeMeas$date)

    # Format continuous discharge data
    csd_continuousDischarge <- DP4.00130.001$csd_continuousDischarge
    csd_continuousDischarge$date <- lubridate::round_date(csd_continuousDischarge$endDate, "20 mins")
    if (site.id=="TOOK_inlet") {
      csd_continuousDischarge <- csd_continuousDischarge%>%
        dplyr::filter(stringr::str_detect(curveID,"TKIN"))
    }else{
      if(site.id=="TOOK_outlet"){
        csd_continuousDischarge <- csd_continuousDischarge%>%
          dplyr::filter(stringr::str_detect(curveID,"TKOT"))
      }
    }

    # Format gauge-pressure relationship data
    sdrc_gaugePressureRelationship <- DP4.00130.001$sdrc_gaugePressureRelationship
    if(!base::is.null(sdrc_gaugePressureRelationship)){
      if (site.id=="TOOK_inlet") {
        sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
          dplyr::filter(stringr::str_detect(regressionID,"TKIN"))
      }else{
        if(site.id=="TOOK_outlet"){
          sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
            dplyr::filter(stringr::str_detect(regressionID,"TKOT"))
        }
      }
      if(base::nrow(sdrc_gaugePressureRelationship)>0){
        sdrc_gaugePressureRelationship$date <- base::paste0(base::as.Date(sdrc_gaugePressureRelationship$gaugeCollectDate)," 20:00:00")
        sdrc_gaugePressureRelationship$date <- base::as.character(sdrc_gaugePressureRelationship$date)
        sdrc_gaugePressureRelationship$gauge_Height <- sdrc_gaugePressureRelationship$gaugeHeight
        sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
          dplyr::select(gauge_Height, date)
      }else{
        sdrc_gaugePressureRelationship <- NULL
      }
    }

    # Creating summary table for variables and  uncertainties to be included
    continuousDischarge_sum <- csd_continuousDischarge%>%
      dplyr::group_by(date)%>%
      dplyr::summarize(meanQ=base::mean(maxpostDischarge,na.rm = T),
                       meanH=base::mean(equivalentStage,na.rm = T),
                       meanHUnc=base::mean(stageUnc,na.rm = T),
                       meanURemnUnc=base::mean(withRemnUncQUpper2Std,na.rm = T),
                       meanLRemnUnc=base::mean(withRemnUncQLower2Std,na.rm = T),
                       meanUParaUnc=base::mean(withParaUncQUpper2Std,na.rm = T),
                       meanLParaUnc=base::mean(withParaUncQLower2Std,na.rm = T),
                       dischargeFinalQF=base::sum(dischargeFinalQF,na.rm = T),
                       dischargeFinalQFSciRvw=base::sum(dischargeFinalQFSciRvw,na.rm = T))%>%
      dplyr::mutate(meanLHUnc=meanH-meanHUnc,
                    meanUHUnc=meanH+meanHUnc)
    continuousDischarge_sum$date <- base::as.character(continuousDischarge_sum$date)

    # Mutate the QF fields for plotting - QF will only be plotted if >20% records in mean are flagged
    continuousDischarge_sum$dischargeFinalQF[continuousDischarge_sum$dischargeFinalQF<4] <- 0
    continuousDischarge_sum$dischargeFinalQF[continuousDischarge_sum$dischargeFinalQF>=4] <- base::max(continuousDischarge_sum$meanURemnUnc,na.rm = T)
    continuousDischarge_sum$dischargeFinalQFSciRvw[continuousDischarge_sum$dischargeFinalQFSciRvw<4] <- 0
    continuousDischarge_sum$dischargeFinalQFSciRvw[continuousDischarge_sum$dischargeFinalQFSciRvw>=4] <- base::max(continuousDischarge_sum$meanURemnUnc,na.rm = T)

    # Joining gauge discharge vars to continuous summary table
    continuousDischarge_sum <- dplyr::full_join(continuousDischarge_sum, sdrc_gaugeDischargeMeas, by="date")

    # Joining guagepressure to  continuoussummary table
    if(!base::is.null(sdrc_gaugePressureRelationship)){
      continuousDischarge_sum <- dplyr::full_join(continuousDischarge_sum, sdrc_gaugePressureRelationship, by="date")
    }else{
      continuousDischarge_sum$gauge_Height <- NA
    }

    # Subset the summary data frame to only those records in the selected date range
    continuousDischarge_sum <- continuousDischarge_sum%>%
      dplyr::filter(date>=start.date&date<=end.date)

    # Create a vector of unique rating curve IDs
    curveIDs <- base::unique(csd_continuousDischarge$curveID)

  }else{
    # Format continuous discharge for TOMB
    csd_continuousDischarge <- DP4.00130.001$csd_continuousDischargeUSGS
    csd_continuousDischarge$date <- lubridate::round_date(csd_continuousDischarge$endDate, "20 mins")
    continuousDischarge_sum <- csd_continuousDischarge%>%
      dplyr::group_by(date)%>%
      dplyr::summarize(meanQ=base::mean(usgsDischarge,na.rm = T),
                       meanH=NA,
                       meanLHUnc=NA,
                       meanUHUnc=NA,,
                       meanURemnUnc=NA,
                       meanLRemnUnc=NA,
                       meanUParaUnc=base::mean(withRegressionUncQUpper2Std,na.rm = T),
                       meanLParaUnc=base::mean(withRegressionUncQLower2Std,na.rm = T),
                       dischargeFinalQF=NA,
                       streamDischarge=NA,
                       gaugeHeight=NA,
                       gauge_Height=NA)
    curveIDs <- NA
  }


  #DO1 HOPB yes primary
  #DO2 LEWI no primary

  #checks if primary precipitation data exist if not use secondary
  #lubridate from highest available resolution data to 20 mins
  #add data to summary table
  isPrimaryPtp <- NULL
  gaugeID <- NULL
  if(!is.null(DP1.00006.001$PRIPRE_5min)){
    ptp <- DP1.00006.001$PRIPRE_5min
    gaugeID<-ptp$siteID[1]
    ptp$date <- lubridate::round_date(ptp$endDateTime, "20 mins")
    ptp <- ptp%>%
      dplyr::group_by(date)%>%
      dplyr::summarize(priPrecipBulk=base::mean(priPrecipBulk,na.rm = T),
                       priPrecipExpUncert=base::mean(priPrecipExpUncert,na.rm = T),
                       priPrecipFinalQF=base::sum(priPrecipFinalQF,na.rm = T)) %>%
      dplyr::mutate(priPrecipBulkLoUnc=priPrecipBulk-priPrecipExpUncert,
                    priPrecipBulkUpUnc=priPrecipBulk+priPrecipExpUncert)

    # Mutate the QF fields for plotting - QF will only be plotted if >20% records in mean are flagged
    ptp$priPrecipFinalQF[ptp$priPrecipFinalQF<=1] <- 0
    ptp$priPrecipFinalQF[ptp$priPrecipFinalQF>=2] <- base::max(continuousDischarge_sum$meanURemnUnc,na.rm = T)/2

    isPrimaryPtp <- TRUE
  }
  else{
    ptp <- DP1.00006.001$SECPRE_1min
    gaugeID<-ptp$siteID[1]
    ptp$date <- lubridate::round_date(ptp$endDateTime, "20 mins")
    ptp <- ptp%>%
      dplyr::group_by(date)%>%
      dplyr::summarize(secPrecipBulk=base::mean(secPrecipBulk,na.rm = T),
                       secPrecipExpUncert=base::mean(secPrecipExpUncert,na.rm = T),
                       secPrecipSciRvwQF=base::sum(secPrecipSciRvwQF,na.rm = T)) %>%
      dplyr::mutate(secPrecipBulkLoUnc=secPrecipBulk-secPrecipExpUncert,
                    secPrecipBulkUpUnc=secPrecipBulk+secPrecipExpUncert)


    # Mutate the QF fields for plotting - QF will only be plotted if >20% records in mean are flagged
    ptp$secPrecipSciRvwQF[ptp$secPrecipSciRvwQF<=1] <- 0
    ptp$secPrecipSciRvwQF[ptp$secPrecipSciRvwQF>=2] <- base::max(continuousDischarge_sum$meanURemnUnc,na.rm = T)

    isPrimaryPtp <- FALSE
  }

  #filtering ptp date to match continuousDischarge_sum date
  ptp$date <- base::as.character(ptp$date)
  ptp <- ptp%>%
    dplyr::filter(date>=start.date&date<=end.date)

  continuousDischarge_sum <- full_join(continuousDischarge_sum,ptp)

  precipitationSite <- list(gaugeID,isPrimaryPtp)

  # Add historic median Q to the summary table
  histMedQ <- base::readRDS(base::url("https://storage.neonscience.org/neon-test-geobath-files/NEON_MEDIAN_Q_SHINY_APP_THROUGH_WY2020_VB.rds","rb"))
  histMedQ <- histMedQ%>%
    dplyr::filter(siteID==site.id)
  continuousDischarge_sum$monthDay <- base::gsub("[0-9]{4}\\-","",continuousDischarge_sum$date)
  continuousDischarge_sum$histMedQ <- NA
  for(i in 1:base::nrow(continuousDischarge_sum)){
    continuousDischarge_sum$histMedQ[i] <- histMedQ$medianQ[histMedQ$monthDay==continuousDischarge_sum$monthDay[i]]
  }
  minYear <- base::unique(histMedQ$minYear)
  maxYear <- base::unique(histMedQ$maxYear)
  histMedQYearRange <- base::list(minYear,maxYear)

  # 3x median 
  if(include.q.stats){
    # Remove SRQF data
    csd_continuousDischarge <- csd_continuousDischarge%>%
      dplyr::mutate(dischargeFinalQFSciRvw = base::ifelse(is.na(dischargeFinalQFSciRvw), 0, dischargeFinalQFSciRvw))
    csd_continuousDischarge <- csd_continuousDischarge%>%
      dplyr::filter(dischargeFinalQFSciRvw == 0)
    if(site!="TOMB"){
      # Calculate the parameters
      medQ <- 3*stats::median(csd_continuousDischarge$maxpostDischarge,na.rm = T)
      twentyFiveQ <- stats::quantile(csd_continuousDischarge$maxpostDischarge,0.25,na.rm = T)
      seventyFiveQ <- stats::quantile(csd_continuousDischarge$maxpostDischarge,0.75,na.rm = T)
    }else{
      # Calculate the parameters
      medQ <- 3*stats::median(csd_continuousDischarge$usgsDischarge,na.rm = T)
      twentyFiveQ <- stats::quantile(csd_continuousDischarge$usgsDischarge,0.25,na.rm = T)
      seventyFiveQ <- stats::quantile(csd_continuousDischarge$usgsDischarge,0.75,na.rm = T)
    }
    # Make an output list
    dischargeStats <- list(medQ,
                           twentyFiveQ,
                           seventyFiveQ)
    names(dischargeStats) <- c("medQ",
                               "twentyFiveQ",
                               "seventyFiveQ")
  }else{
    dischargeStats <- NA
  }

  # Make an output list
  continuousDischarge_list <- base::list(continuousDischarge_sum,
                                         curveIDs,
                                         histMedQYearRange,
                                         dischargeStats,
										 precipitationSite)
  names(continuousDischarge_list) <- c("continuousDischarge_sum",
                                       "curveIDs",
                                       "histMedQYearRange",
                                       "dischargeStats",
									   "precipitationSite")

  return(continuousDischarge_list)

}
