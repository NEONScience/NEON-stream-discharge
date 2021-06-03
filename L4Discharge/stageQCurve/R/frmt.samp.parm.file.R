##############################################################################################
#' @title Format MCMC cooked results File

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
frmt.samp.parm.file <- function(
  dataFrame,
  metadata
  ){

  MCMC_Names <- c(
    'domainID',
    'siteID',
    'namedLocation',
    'startDate',
    'endDate',
    'curveID',
    # 'curveStartDate',
    # 'curveEndDate',
    'controlNumber',
    'parameterNumber',
    'spagZeroFlowOffset',
    'spagExponent',
    'spagCoefficient',
    'spagActivationStage',
    'spagGamma1',
    'spagGamma2',
    'spagLogPost',
    'dataQF'
  )
  numCtrls <- metadata$numCtrls
  numCurves <- metadata$numCurves
  numSpag <- metadata$numSpag
  curveIDs <- unique(dataFrame$curveID[order(dataFrame$curveID)])

  MCMC_outputDF <- data.frame(matrix(data=NA, ncol=length(MCMC_Names), nrow=(numSpag*sum(numCtrls))))
  names(MCMC_outputDF) <- MCMC_Names

  MCMC_outputDF$domainID <- metadata$domain
  MCMC_outputDF$siteID <- metadata$site
  MCMC_outputDF$namedLocation <- metadata$namedLocationName

  for(k in 1:numCurves){
    if (k==1) {
      startIdx <- 1+(k-1)*numCtrls[k]*numSpag
      endIdx <- numCtrls[k]*numSpag
    }else{
      startIdx <- min(which(is.na(MCMC_outputDF$curveID)))
      endIdx <- (startIdx-1)+(numCtrls[k]*numSpag)
    }

    MCMC_outputDF$controlNumber[startIdx:endIdx] <- rep(1:numCtrls[k], length(dataFrame$LogPost[dataFrame$curveID==curveIDs[k]]))
    wideStartIdx <- 1+(k-1)*numSpag
    wideEndIdx <- k*numSpag
    MCMC_outputDF$curveID[startIdx:endIdx] <- curveIDs[k]
    MCMC_outputDF$startDate[startIdx:endIdx] <- format(unique(dataFrame$curveStartDate[dataFrame$curveID==curveIDs[k]]), format = "%Y-%m-%dT%H:%M:%S.000Z")
    MCMC_outputDF$endDate[startIdx:endIdx] <- format(unique(dataFrame$curveEndDate[dataFrame$curveID==curveIDs[k]]), format = "%Y-%m-%dT%H:%M:%S.000Z")

    for(i in seq(along = dataFrame$LogPost[dataFrame$curveID == curveIDs[k]])){
      if (k==1) {
        loop_i_startIdx <- i*numCtrls[k]-(numCtrls[k]-1)+(k-1)*numSpag*numCtrls[k]
        loop_i_endIdx <- i*numCtrls[k]+(k-1)*numSpag*numCtrls[k]
      }else{
        loop_i_startIdx <- min(which(is.na(MCMC_outputDF$parameterNumber)))
        loop_i_endIdx <- (loop_i_startIdx-1)+numCtrls[k]
      }

      MCMC_outputDF$spagGamma1[loop_i_startIdx:loop_i_endIdx] <- dataFrame$Y1_gamma1[i+(k-1)*numSpag]
      MCMC_outputDF$spagGamma2[loop_i_startIdx:loop_i_endIdx] <- dataFrame$Y1_gamma2[i+(k-1)*numSpag]
      MCMC_outputDF$spagLogPost[loop_i_startIdx:loop_i_endIdx] <- dataFrame$LogPost[i+(k-1)*numSpag]
      MCMC_outputDF$parameterNumber[loop_i_startIdx:loop_i_endIdx] <- dataFrame$parameterNumber[i+(k-1)*numSpag]

      for(j in 1:numCtrls[k]){
        loop_j_Idx <- (loop_i_startIdx-1)+j
        inputRowIdx <- i+(k-1)*numSpag
        MCMC_outputDF$spagZeroFlowOffset[loop_j_Idx] <- dataFrame[inputRowIdx,which(names(dataFrame)==paste0("b",j))]
        MCMC_outputDF$spagExponent[loop_j_Idx] <- dataFrame[inputRowIdx,which(names(dataFrame)==paste0("c",j))]
        MCMC_outputDF$spagCoefficient[loop_j_Idx] <- dataFrame[inputRowIdx,which(names(dataFrame)==paste0("a",j))]
        MCMC_outputDF$spagActivationStage[loop_j_Idx] <- dataFrame[inputRowIdx,which(names(dataFrame)==paste0("k",j))]
      }
    }
  }
  return(MCMC_outputDF)
}
