##############################################################################################
#' @title Format Curve Info File

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description This function takes a dataframe and .

#' @param dataFrame A dataframe containing data to be formatted [dataframe]
#' @param metadata A list containing the transition metadata that includes: domain,site,
#' startDateFormatted,endDateFormatted,namedLocationName,numCtrls,numCurves,waterYear [list]

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
frmt.stag.disc.curv.info.file <- function(
  dataFrame,
  metadata
  ){

  curveInfo_Names <- c(
    'domainID',
    'siteID',
    'namedLocation',
    'startDate',
    'endDate',
    'allEventID',
    'curveID',
    'curveSegmentID',
    # 'curveStartDate',
    # 'curveEndDate',
    'waterYear',
    'minQ',
    'maxQ',
    'minStage',
    'maxStage',
    'dataQF'
  )
  outputDF <- data.frame(matrix(data=NA, ncol=length(curveInfo_Names), nrow=nrow(dataFrame)))
  names(outputDF) <- curveInfo_Names

  outputDF$domainID <- metadata$domain
  outputDF$siteID <- metadata$site
  outputDF$namedLocation <- metadata$namedLocationName
  #outputDF$startDate <- metadata$startDate
  #outputDF$endDate <- metadata$endDate
  outputDF$startDate <- format(dataFrame$curveStartDate, format = "%Y-%m-%dT%H:%M:%S.000Z")
  outputDF$endDate <- format(dataFrame$curveEndDate, format = "%Y-%m-%dT%H:%M:%S.000Z")
  outputDF$allEventID <- dataFrame$allEventID
  outputDF$curveID <- dataFrame$curveID
  outputDF$curveSegmentID <- dataFrame$curveSegmentID
  #outputDF$curveStartDate <- format(dataFrame$curveStartDate, format = "%Y-%m-%dT%H:%M:%S.000Z")
  #outputDF$curveEndDate <- format(dataFrame$curveEndDate, format = "%Y-%m-%dT%H:%M:%S.000Z")
  outputDF$waterYear <- metadata$waterYear
  outputDF$minQ <- dataFrame$minQ*1000#convert CMS output from BaM to LPS for publication
  outputDF$maxQ <- dataFrame$maxQ*1000#convert CMS output from BaM to LPS for publication
  outputDF$minStage <- dataFrame$minStage
  outputDF$maxStage <- dataFrame$maxStage
  #outputDF$dataQF

  return(outputDF)
}
