##############################################################################################
#' @title Format MCMC cooked results File

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function takes a dataframe and .

#' @param dataFrame A dataframe containing data to be formatted [dataframe]
#' @param numCtrls Number of hydraulic controls [integer]

#' @return This function returns a dataframe formatted for integration into the transition object

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
##############################################################################################
frmt.pre.parm.file <- function(
  dataFrame,
  numCtrls
  ){
  
  output_Names <- c(
    'controlNumber',
    'priorExponent',
    'priorExponentUnc',
    'priorCoefficient',
    'priorCoefficientUnc',
    'priorActivationStage',
    'priorActivationStageUnc'
  )
  
  outputDF <- data.frame(matrix(data=NA, ncol=length(output_Names), nrow=numCtrls))
  names(outputDF) <- output_Names
  
  outputDF$controlNumber <- 1:numCtrls
  
  uncRegex <- "^.*,"
  valRegex <- ",.*$"
  
  for(k in 1:numCtrls){
    inc <- 12*(k-1)
    kIdx <- 8 + inc
    aIdx <- 12 + inc
    cIdx <- 16 + inc
    
    outputDF$priorActivationStage[k] <- as.numeric(gsub(valRegex,"",dataFrame[kIdx,1]))
    outputDF$priorActivationStageUnc[k] <- as.numeric(gsub(uncRegex,"",dataFrame[kIdx,1]))
    
    outputDF$priorCoefficient[k] <- as.numeric(gsub(valRegex,"",dataFrame[aIdx,1]))
    outputDF$priorCoefficientUnc[k] <- as.numeric(gsub(uncRegex,"",dataFrame[aIdx,1]))
    
    outputDF$priorExponent[k] <- as.numeric(gsub(valRegex,"",dataFrame[cIdx,1]))
    outputDF$priorExponentUnc[k] <- as.numeric(gsub(uncRegex,"",dataFrame[cIdx,1]))
  }
  
  return(outputDF)
}
