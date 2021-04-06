##############################################################################################
#' @title Format Stage-Discharge File

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description This function takes a dataframe and .

#' @param dataFrame A dataframe containing data to be formatted [dataframe]
#' @param metadata A list containing the transition metadata that includes: domain,site,
#' startDateFormatted,endDateFormatted,namedLocationName,numCtrls,numCurves,waterYear [list]
#' @param curveIDData A dataframe containing rating curve-specific start and end datetimes [dataframe]

#' @return This function returns a dataframe formatted for integration into the transition object

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-04-05)
#     Code updates to match internal NEON rating curve development workflow
##############################################################################################
frmt.gaug.disc.mea.file <- function(
  dataFrame,
  metadata,
  curveIDData
  ){

  stageData_Names <- c(
    'domainID',
    'siteID',
    'namedLocation',
    'startDate',
    'endDate',
    'curveID',
    'gaugeHeight',
    'gaugeHeightUnc',
    'gaugeHeightOffset',
    'streamDischarge',
    'streamDischargeUnc',
    'gaugeEventID',
    'includedInRatingCurve',
    'recalculatedL1QF',
    'L1DataQF',
    'dataQF'
  )
  outputDF <- data.frame(matrix(data=NA, ncol=length(stageData_Names), nrow=length(dataFrame$startDate)))
  names(outputDF) <- stageData_Names

  #Add in data for a few remaining pub fields
  outputDF$domainID <- metadata$domain
  outputDF$siteID <- metadata$site
  outputDF$namedLocation <- metadata$namedLocationName
  #outputDF$startDate <- dataFrame$startDate
  #outputDF$endDate <- dataFrame$endDate
  outputDF$curveID <- dataFrame$curveID
  outputDF$gaugeHeight <- dataFrame$streamStage
  outputDF$gaugeHeightUnc <- 0
  outputDF$gaugeHeightOffset <- dataFrame$gaugeHeightOffset
  outputDF$streamDischarge <- dataFrame$calcQ
  outputDF$streamDischargeUnc <- dataFrame$calcQ * 0.1
  outputDF$gaugeEventID <- dataFrame$eventID
  outputDF$includedInRatingCurve <- "true"
  outputDF$recalculatedL1QF <- dataFrame$recalculatedL1QF
  outputDF$L1DataQF <- dataFrame$L1DataQF
  outputDF$dataQF <- dataFrame$dataQF

  for (i in 1:nrow(outputDF)) {
    for (j in 1:nrow(curveIDData)) {
      if (outputDF$curveID[i]==curveIDData$curveID[j]) {
        outputDF$startDate[i] <- min(curveIDData$curveStartDate[curveIDData$curveID==curveIDData$curveID[j]])
        outputDF$endDate[i] <- max(curveIDData$curveEndDate[curveIDData$curveID==curveIDData$curveID[j]])
      }
    }
  }

  return(outputDF)
}
