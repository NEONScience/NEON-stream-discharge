##############################################################################################
#' @title Format Residual File

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description This function takes a dataframe and .

#' @param dataFrame A dataframe containing data to be formatted [dataframe]
#' @param metadata A list containing the transition metadata that includes: domain,site,
#' startDateFormatted,endDateFormatted,namedLocationName,numCtrls,numCurves,waterYear [list]

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
##############################################################################################
frmt.rslt.resd.file <- function(
  dataFrame,
  metadata
  ){

  residuals_Names <- c(
    'domainID',
    'siteID',
    'namedLocation',
    'startDate',
    'endDate',
    'curveID',
    # 'curveStartDate',
    # 'curveEndDate',
    'gaugeEventID',
    'X1observed',
    'X1true',
    'Y1observed',
    'Y1unbiased',
    'Y1simulated',
    'Y1residual',
    'Y1stdresidual',
    'dataQF'
  )
  residuals_outputDF <- data.frame(matrix(data=NA, ncol=length(residuals_Names), nrow=length(dataFrame$X1_obs)))
  names(residuals_outputDF) <- residuals_Names

  residuals_outputDF$domainID <- metadata$domain
  residuals_outputDF$siteID <- metadata$site
  residuals_outputDF$namedLocation <- metadata$namedLocationName
  #residuals_outputDF$startDate <- metadata$startDate
  #residuals_outputDF$endDate <- metadata$endDate
  residuals_outputDF$startDate <- format(dataFrame$curveStartDate, format = "%Y-%m-%dT%H:%M:%S.000Z")
  residuals_outputDF$endDate <- format(dataFrame$curveEndDate, format = "%Y-%m-%dT%H:%M:%S.000Z")
  residuals_outputDF$curveID <- dataFrame$curveID
  #residuals_outputDF$curveStartDate <- format(dataFrame$curveStartDate, format = "%Y-%m-%dT%H:%M:%S.000Z")
  #residuals_outputDF$curveEndDate <- format(dataFrame$curveEndDate, format = "%Y-%m-%dT%H:%M:%S.000Z")
  residuals_outputDF$gaugeEventID <- dataFrame$eventID
  residuals_outputDF$X1observed <- dataFrame$X1_obs
  residuals_outputDF$X1true <- dataFrame$X1_true
  residuals_outputDF$Y1observed <- dataFrame$Y1_obs*1000#convert CMS output from BaM to LPS for publication
  residuals_outputDF$Y1unbiased <- dataFrame$Y1_unbiased*1000#convert CMS output from BaM to LPS for publication
  residuals_outputDF$Y1simulated <- dataFrame$Y1_sim*1000#convert CMS output from BaM to LPS for publication
  residuals_outputDF$Y1residual <- dataFrame$Y1_res*1000#convert CMS output from BaM to LPS for publication
  residuals_outputDF$Y1stdresidual <- dataFrame$Y1_stdres*1000#convert CMS output from BaM to LPS for publication
  #residuals_outputDF$dataQF

  return(residuals_outputDF)
}
