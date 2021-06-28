##############################################################################################
#' @title Writes out a text file with "cooked spaghettis" for BaM

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function writes out the "cooked spaghettis" for BaM.

#' @param spagDataIn Spaghettis, "sampled parameters", for the rating curve used to predict Q [dataFrame]
#' @param spagOutPath Filepath to write re-formatted file out to including the filename [string]

#' @return This writes out txt file of "cooked spaghettis" for BaM

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-02-20)
#     updates to configure spag data for sites with 2 controls
#   Zachary L. Nickerson (2021-03-26)
#     updates to configure spag data for sites with 4 controls
##############################################################################################
txt.out.spag.data <- function(spagDataIn, spagOutPath){
  numCtrls <- unique(spagDataIn$controlNumber)
  numCtrls <- seq(along = numCtrls)

  ctrlNames <- rep(NA,(length(numCtrls)*3))
  offsetNames <- rep(NA,length(numCtrls))
  for(i in seq(along = numCtrls)){
    ctrlNames[1+(i-1)*3] <- paste0("k",numCtrls[i])
    ctrlNames[2+(i-1)*3] <- paste0("a",numCtrls[i])
    ctrlNames[3+(i-1)*3] <- paste0("c",numCtrls[i])
    offsetNames[i] <- paste0("b",numCtrls[i])
  }

  spag_Names <- c(
    ctrlNames,
    'gamma1',
    'gamma2',
    'LogPost',
    offsetNames
  )
  outputDF <- data.frame(matrix(data=NA, ncol=length(spag_Names), nrow=(length(spagDataIn$controlNumber)/length(numCtrls))))
  names(outputDF) <- spag_Names

  matchOnSpag <- unique(spagDataIn$parameterNumber)
  for(i in seq(along = matchOnSpag)){
    currSpag <- matchOnSpag[i]
    loopSpagData <- spagDataIn[spagDataIn$parameterNumber == currSpag,]

    #Ones that are shared for all controls
    outputDF$gamma1[i] <- unique(loopSpagData$spagGamma1)
    outputDF$gamma2[i] <- unique(loopSpagData$spagGamma2)
    outputDF$LogPost[i] <- unique(loopSpagData$spagLogPost)

    #Converting from long to wide
    for(j in seq(along = numCtrls)){
      currCtrl <- as.character(j)
      outputDF[i,which(names(outputDF) == paste0("k",currCtrl))] <- loopSpagData$spagActivationStage[loopSpagData$controlNumber == currCtrl] #k
      outputDF[i,which(names(outputDF) == paste0("a",currCtrl))] <- loopSpagData$spagCoefficient[loopSpagData$controlNumber == currCtrl] #a
      outputDF[i,which(names(outputDF) == paste0("c",currCtrl))] <- loopSpagData$spagExponent[loopSpagData$controlNumber == currCtrl] #c
      #Count from the end for this one
      outputDF[i,which(names(outputDF) == paste0("b",currCtrl))] <-loopSpagData$spagZeroFlowOffset[loopSpagData$controlNumber == currCtrl] #b
    }
  }
  #Format data
  outputDF <- apply(outputDF,c(1,2),txt.fmt.MCMC.cooked)
  #Format names
  colnames(outputDF) <- format(colnames(outputDF), trim = F, width = 15)
  write.table(outputDF, spagOutPath, row.names = F, quote = F, sep = "") #Specifies the types of runs to perform
}
