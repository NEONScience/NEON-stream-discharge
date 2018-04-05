##############################################################################################
#' @title Format Summary File

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function takes a dataframe and formats it for integration into a transition object.

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
##############################################################################################
frmt.post.parm.file <- function(
  dataFrame,
  metadata
  ){
  
  numCtrls <- metadata$numCtrls
  numCurves <- metadata$numCurves
  
  Smry_Names <- c(
    'domainID',
    'siteID',
    'namedLocation',
    'startDate',
    'endDate',
    'curveID',
    'curveStartDate',
    'curveEndDate',
    'controlNumber',
    'maxPostZeroFlowOffset',
    'maxPostExponent',
    'maxPostCoefficient',
    'maxPostActivationStage',
    'maxPostGamma1',
    'maxPostGamma2',
    'stdDevZeroFlowOffset',
    'stdDevExponent',
    'stdDevCoefficient',
    'stdDevActivationStage',
    'stdDevGamma1',
    'stdDevGamma2',
    'dataQF'
  )
  Smry_outputDF <- data.frame(matrix(data=NA, ncol=length(Smry_Names), nrow=(numCtrls*numCurves)))
  names(Smry_outputDF) <- Smry_Names
  
  Smry_outputDF$domainID <- metadata$domain
  Smry_outputDF$siteID <- metadata$site
  Smry_outputDF$namedLocation <- metadata$namedLocationName
  Smry_outputDF$startDate <- metadata$startDate
  Smry_outputDF$endDate <- metadata$endDate
  Smry_outputDF$controlNumber <- rep(1:numCtrls, numCurves)
  
  MaxPostRow <- which(grepl("MaxPost",row.names(dataFrame)))
  StdevRow <- which(grepl("St.Dev.",row.names(dataFrame)))
  for(j in 1:numCurves){
    maxPostRowIdx <- MaxPostRow[j]
    StdevRowIdx <- StdevRow[j]
    
    startIdx <- 1+(j-1)*numCtrls
    endIdx <- j*numCtrls
    Smry_outputDF$curveID[startIdx:endIdx] <- dataFrame$curveID[maxPostRowIdx]
    Smry_outputDF$curveStartDate[startIdx:endIdx] <- format(dataFrame$curveStartDate[maxPostRowIdx], format = "%Y-%m-%dT%H:%M:%S.000Z")
    Smry_outputDF$curveEndDate[startIdx:endIdx] <- format(dataFrame$curveEndDate[maxPostRowIdx], format = "%Y-%m-%dT%H:%M:%S.000Z")
    Smry_outputDF$maxPostGamma1[startIdx:endIdx] <- dataFrame$Y1_gamma1[maxPostRowIdx]
    Smry_outputDF$maxPostGamma2[startIdx:endIdx] <- dataFrame$Y1_gamma2[maxPostRowIdx]
    Smry_outputDF$stdDevGamma1[startIdx:endIdx] <- dataFrame$Y1_gamma1[StdevRowIdx]
    Smry_outputDF$stdDevGamma2[startIdx:endIdx] <- dataFrame$Y1_gamma2[StdevRowIdx]
    
    for(i in 1:numCtrls){
      loopIdx <- i+(j-1)*numCtrls
      Smry_outputDF$maxPostZeroFlowOffset[loopIdx] <- dataFrame[maxPostRowIdx, which(names(dataFrame) == paste0("b",Smry_outputDF$controlNumber[i]))]
      Smry_outputDF$maxPostExponent[loopIdx] <- dataFrame[maxPostRowIdx, which(names(dataFrame) == paste0("c",Smry_outputDF$controlNumber[i]))]
      Smry_outputDF$maxPostCoefficient[loopIdx] <- dataFrame[maxPostRowIdx, which(names(dataFrame) == paste0("a",Smry_outputDF$controlNumber[i]))]
      Smry_outputDF$maxPostActivationStage[loopIdx] <- dataFrame[maxPostRowIdx, which(names(dataFrame) == paste0("k",Smry_outputDF$controlNumber[i]))]
      Smry_outputDF$stdDevZeroFlowOffset[loopIdx] <- dataFrame[StdevRowIdx, which(names(dataFrame) == paste0("b",Smry_outputDF$controlNumber[i]))]
      Smry_outputDF$stdDevExponent[loopIdx] <- dataFrame[StdevRowIdx, which(names(dataFrame) == paste0("c",Smry_outputDF$controlNumber[i]))]
      Smry_outputDF$stdDevCoefficient[loopIdx] <- dataFrame[StdevRowIdx, which(names(dataFrame) == paste0("a",Smry_outputDF$controlNumber[i]))]
      Smry_outputDF$stdDevActivationStage[loopIdx] <- dataFrame[StdevRowIdx, which(names(dataFrame) == paste0("k",Smry_outputDF$controlNumber[i]))]
    }
  }
  return(Smry_outputDF)
}
