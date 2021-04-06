##############################################################################################
#' @title Format MCMC cooked results File

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
#     Code updates to match NEON rating curve development workflow
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

  MCMC_outputDF <- data.frame(matrix(data=NA, ncol=length(MCMC_Names), nrow=(numSpag*numCtrls*numCurves)))
  names(MCMC_outputDF) <- MCMC_Names

  MCMC_outputDF$domainID <- metadata$domain
  MCMC_outputDF$siteID <- metadata$site
  MCMC_outputDF$namedLocation <- metadata$namedLocationName
  #MCMC_outputDF$startDate <- metadata$startDate
  #MCMC_outputDF$endDate <- metadata$endDate

  MCMC_outputDF$controlNumber <- rep(1:numCtrls, length(dataFrame$LogPost))

  for(k in 1:numCurves){
    startIdx <- 1+(k-1)*numCtrls*numSpag
    endIdx <- k*numCtrls*numSpag

    wideStartIdx <- 1+(k-1)*numSpag
    wideEndIdx <- k*numSpag
    currentCurveID <- unique(dataFrame$curveID[wideStartIdx:wideEndIdx])
    currentCurveStartDate <- format(unique(dataFrame$curveStartDate[wideStartIdx:wideEndIdx]), format = "%Y-%m-%dT%H:%M:%S.000Z")
    currentCurveEndDate <- format(unique(dataFrame$curveEndDate[wideStartIdx:wideEndIdx]), format = "%Y-%m-%dT%H:%M:%S.000Z")

    MCMC_outputDF$curveID[startIdx:endIdx] <- currentCurveID
    #MCMC_outputDF$curveStartDate[startIdx:endIdx] <- currentCurveStartDate
    #MCMC_outputDF$curveEndDate[startIdx:endIdx] <- currentCurveEndDate
    MCMC_outputDF$startDate[startIdx:endIdx] <- currentCurveStartDate
    MCMC_outputDF$endDate[startIdx:endIdx] <- currentCurveEndDate

    for(i in seq(along = dataFrame$LogPost[dataFrame$curveID == currentCurveID])){
      loop_i_startIdx <- i*numCtrls-(numCtrls-1)+(k-1)*numSpag*numCtrls
      loop_i_endIdx <- i*numCtrls+(k-1)*numSpag*numCtrls

      MCMC_outputDF$spagGamma1[loop_i_startIdx:loop_i_endIdx] <- dataFrame$Y1_gamma1[i+(k-1)*numSpag]
      MCMC_outputDF$spagGamma2[loop_i_startIdx:loop_i_endIdx] <- dataFrame$Y1_gamma2[i+(k-1)*numSpag]
      MCMC_outputDF$spagLogPost[loop_i_startIdx:loop_i_endIdx] <- dataFrame$LogPost[i+(k-1)*numSpag]
      MCMC_outputDF$parameterNumber[loop_i_startIdx:loop_i_endIdx] <- dataFrame$parameterNumber[i+(k-1)*numSpag]

      for(j in 1:numCtrls){
        loop_j_Idx <- (i-1)*numCtrls+j+(k-1)*numSpag*numCtrls
        inputRowIdx <- i+(k-1)*numSpag

        MCMC_outputDF$spagZeroFlowOffset[loop_j_Idx] <- dataFrame[inputRowIdx,which(names(dataFrame) ==
                                                                                  paste0("b",MCMC_outputDF$controlNumber[(i-1)*numCtrls+j]))]
        MCMC_outputDF$spagExponent[loop_j_Idx] <- dataFrame[inputRowIdx,which(names(dataFrame) ==
                                                                            paste0("c",MCMC_outputDF$controlNumber[(i-1)*numCtrls+j]))]
        MCMC_outputDF$spagCoefficient[loop_j_Idx] <- dataFrame[inputRowIdx,which(names(dataFrame) ==
                                                                               paste0("a",MCMC_outputDF$controlNumber[(i-1)*numCtrls+j]))]
        MCMC_outputDF$spagActivationStage[loop_j_Idx] <- dataFrame[inputRowIdx,which(names(dataFrame) ==
                                                                                   paste0("k",MCMC_outputDF$controlNumber[(i-1)*numCtrls+j]))]
      }
    }
  }
  return(MCMC_outputDF)
}
