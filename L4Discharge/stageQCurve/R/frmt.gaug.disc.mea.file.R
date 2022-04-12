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

#' @return This function returns a dataframe formatted identical to the similar publication
#' table in the Stage-discharge rating curve (DP4.00133.001) data product.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-04-27)
#     update to which field in the input data frame references re-calculated discharge
#   Zachary L. Nickerson (2022-04-11)
#     add gaugeCollectDate field
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
    'gaugeCollectDate',
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
  outputDF$streamDischarge <- as.numeric(dataFrame$finalDischarge)
  outputDF$streamDischargeUnc <- as.numeric(outputDF$streamDischarge) * 0.1
  outputDF$gaugeCollectDate <- dataFrame$endDate 
  outputDF$gaugeEventID <- dataFrame$eventID
  outputDF$includedInRatingCurve <- "true"
  #outputDF$recalculatedL1QF <- dataFrame$recalculatedL1QF
  outputDF$L1DataQF <- dataFrame$L1DataQF
  outputDF$dataQF <- dataFrame$dataQF

  if (metadata$site=="TOOK") {
    outputDF$startDate <- metadata$curveStartDate
    outputDF$endDate <- metadata$curveEndDate
  }else{
    for (i in 1:nrow(outputDF)) {
      for (j in 1:nrow(curveIDData)) {
        if (outputDF$curveID[i]==curveIDData$curveID[j]) {
          outputDF$startDate[i] <- min(curveIDData$curveStartDate[curveIDData$curveID==curveIDData$curveID[j]])
          outputDF$endDate[i] <- max(curveIDData$curveEndDate[curveIDData$curveID==curveIDData$curveID[j]])
        }
      }
    }
  }

  return(outputDF)
}
