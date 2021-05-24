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
  curveIDs <- unique(dataFrame$curveID[order(dataFrame$curveID)])

  Smry_Names <- c(
    'domainID',
    'siteID',
    'namedLocation',
    'startDate',
    'endDate',
    'curveID',
    # 'curveStartDate',
    # 'curveEndDate',
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
  Smry_outputDF <- data.frame(matrix(data=NA, ncol=length(Smry_Names), nrow=sum(numCtrls)))
  names(Smry_outputDF) <- Smry_Names

  Smry_outputDF$domainID <- metadata$domain
  Smry_outputDF$siteID <- metadata$site
  Smry_outputDF$namedLocation <- metadata$namedLocationName

  for (i in 1:numCurves) {
    if (i==1) {
      startIdx <- i
      endIdx <- numCtrls[i]
    }else{
      startIdx <- min(which(is.na(Smry_outputDF$curveID)))
      endIdx <- (startIdx-1)+numCtrls[i]
    }

    maxPostRowIdx <- which(grepl("MaxPost",row.names(dataFrame))&dataFrame$curveID==curveIDs[i])
    stdDevRowIdx <- which(grepl("St.Dev.",row.names(dataFrame))&dataFrame$curveID==curveIDs[i])

    Smry_outputDF$controlNumber[startIdx:endIdx] <- 1:numCtrls[i]
    Smry_outputDF$curveID[startIdx:endIdx] <- curveIDs[i]
    Smry_outputDF$startDate[startIdx:endIdx] <- format(unique(dataFrame$curveStartDate[dataFrame$curveID==curveIDs[i]]), format = "%Y-%m-%dT%H:%M:%S.000Z")
    Smry_outputDF$endDate[startIdx:endIdx] <- format(unique(dataFrame$curveEndDate[dataFrame$curveID==curveIDs[i]]), format = "%Y-%m-%dT%H:%M:%S.000Z")
    Smry_outputDF$maxPostGamma1[startIdx:endIdx] <- dataFrame$Y1_gamma1[maxPostRowIdx]
    Smry_outputDF$maxPostGamma2[startIdx:endIdx] <- dataFrame$Y1_gamma2[maxPostRowIdx]
    Smry_outputDF$stdDevGamma1[startIdx:endIdx] <- dataFrame$Y1_gamma1[stdDevRowIdx]
    Smry_outputDF$stdDevGamma2[startIdx:endIdx] <- dataFrame$Y1_gamma2[stdDevRowIdx]
    for(k in startIdx:endIdx){
      Smry_outputDF$maxPostZeroFlowOffset[k] <- dataFrame[maxPostRowIdx, which(names(dataFrame) == paste0("b",Smry_outputDF$controlNumber[k]))]
      Smry_outputDF$maxPostExponent[k] <- dataFrame[maxPostRowIdx, which(names(dataFrame) == paste0("c",Smry_outputDF$controlNumber[k]))]
      Smry_outputDF$maxPostCoefficient[k] <- dataFrame[maxPostRowIdx, which(names(dataFrame) == paste0("a",Smry_outputDF$controlNumber[k]))]
      Smry_outputDF$maxPostActivationStage[k] <- dataFrame[maxPostRowIdx, which(names(dataFrame) == paste0("k",Smry_outputDF$controlNumber[k]))]
      Smry_outputDF$stdDevZeroFlowOffset[k] <- dataFrame[stdDevRowIdx, which(names(dataFrame) == paste0("b",Smry_outputDF$controlNumber[k]))]
      Smry_outputDF$stdDevExponent[k] <- dataFrame[stdDevRowIdx, which(names(dataFrame) == paste0("c",Smry_outputDF$controlNumber[k]))]
      Smry_outputDF$stdDevCoefficient[k] <- dataFrame[stdDevRowIdx, which(names(dataFrame) == paste0("a",Smry_outputDF$controlNumber[k]))]
      Smry_outputDF$stdDevActivationStage[k] <- dataFrame[stdDevRowIdx, which(names(dataFrame) == paste0("k",Smry_outputDF$controlNumber[k]))]
    }
  }
  return(Smry_outputDF)
}
