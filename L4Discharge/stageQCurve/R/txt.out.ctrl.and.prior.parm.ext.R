##############################################################################################
#' @title Writes out the Config_ControlMatrix and Config_Model text files for BaM

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description This function writes out the ControlMatrix and Model configurations for BaM.

#' @import utils
#' @import neonUtilities

#' @param site 4 letter SITE code [string]
#' @param controlSurveyDate End date of the discharge cross section survey [POSIXct]
#' @param controlMatrixPath File path to write out controlMatrix file [string]
#' @param priorParamsPath File path to write out Model file [string]
#' @param downloadedDataPath File path where downloaded .zip files can be found [string]

#' @return This function read in and writes out a modified version of the RunOptions text
#' file for BaM and returns the number of controls and inputResultUuids for the control
#' matrix and prior parameters tables

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-04-05)
#     Updated data queries and subsetting with current data structure
##############################################################################################
txt.out.ctrl.and.prior.parm.ext <- function(
  site,
  controlSurveyDate,
  controlMatrixPath,
  priorParamsPath,
  downloadedDataPath
  ){

  # Get control data from geomorphology survey
  # Data should have already been downloaded and stacked in stageQCurve::calc.stag.Q.curv()

  # If data has been downloaded using neonUtilities::zipsByProduct() and saved to DATAWS
  if(file.exists(paste0(downloadedDataPath,"filesToStack00133"))){
    ctrlInfo <- try(read.csv(paste(downloadedDataPath,"filesToStack00133","stackedFiles","geo_controlInfo.csv", sep = "/")),silent = T)
    priorParams <- try(read.csv(paste(downloadedDataPath,"filesToStack00133","stackedFiles","geo_priorParameters.csv", sep = "/")),silent = T)
  }else{
    # If data has been directly downloaded from the NEON data portal and saved to DATAWS
    if(file.exists(paste0(downloadedDataPath,"NEON_discharge-rating-curves"))){
      ctrlInfo  <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles","geo_controlInfo.csv", sep = "/")),silent = T)
      priorParams <- try(read.csv(paste(downloadedDataPath,"NEON_discharge-rating-curves","stackedFiles","geo_priorParameters.csv", sep = "/")),silent = T)
    }else{
      # If the individual files are available in DATAWS
      availableFiles <- list.files(downloadedDataPath)
      ctrlInfo  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("geo_controlInfo",availableFiles)], sep = "/")),silent = T))
      priorParams  <- suppressWarnings(try(read.csv(paste(downloadedDataPath,availableFiles[grepl("geo_priorParameters",availableFiles)], sep = "/")),silent = T))
    }
  }

  # Error handling if no control data can be found
  if(attr(ctrlInfo, "class") == "try-error"){
    failureMessage <- paste0("Control activation data could not be retrieved. Ensure the required input data are stored in ",downloadedDataPath)
    stop(failureMessage)
  }
  if(attr(priorParams, "class") == "try-error"){
    failureMessage <- paste0("Prior parameters could not be retrieved. Ensure the required input data are stored in ",downloadedDataPath)
    stop(failureMessage)
  }

  #Subset for the site of interest
  ctrlInfo <- ctrlInfo[ctrlInfo$siteID == site&as.POSIXct(ctrlInfo$endDate) == controlSurveyDate,]
  priorParams <- priorParams[priorParams$siteID == site&as.POSIXct(priorParams$endDate) == controlSurveyDate,]

  # Error handling if zero records remain
  if(nrow(ctrlInfo)<1){
    failureMessage <- paste0("Zero (0) control activation records were retrieved for the ",site," ",controlSurveyDate," survey.")
    stop(failureMessage)
  }
  if(nrow(priorParams)<1){
    failureMessage <- paste0("Zero (0) prior parameters records were retrieved for the ",site," ",controlSurveyDate," survey.")
    stop(failureMessage)
  }

  #Write out control activation state
  numCtrls <- as.numeric(max(ctrlInfo$controlNumber))
  Config_ControlMatrix <- matrix(data=NA, nrow = numCtrls, ncol = numCtrls)
  for(rw in 1:numCtrls){
    for(cl in 1:numCtrls){
      Config_ControlMatrix[rw,cl] <- as.numeric(ctrlInfo$controlActivationState[ctrlInfo$controlNumber == cl & ctrlInfo$segmentNumber == rw])
    }
  }
  write.table(Config_ControlMatrix, controlMatrixPath, row.names = F, col.names = F)

  #write out hydraulic control configurations
  Config_Model <- matrix(data = NA, nrow = (4 + 12*numCtrls))
  Config_Model[1] <- '"BaRatin"'
  Config_Model[2:3] <- 1
  Config_Model[4] <- 3 * numCtrls
  for(j in 1:numCtrls){
    offset <- (j-1)*12
    Config_Model[offset+5] <- paste0('"k', j, '"')
    Config_Model[offset+6] <- priorParams$priorActivationStage[priorParams$controlNumber == j]
    Config_Model[offset+7] <- "'Gaussian'"
    Config_Model[offset+8] <- paste(priorParams$priorActivationStage[priorParams$controlNumber == j],
                                    as.character(priorParams$priorActivationStageUnc[priorParams$controlNumber == j]),
                                    sep = ",")
    Config_Model[offset+9] <- paste0('"a', j, '"')
    Config_Model[offset+10] <- priorParams$priorCoefficient[priorParams$controlNumber == j]
    Config_Model[offset+11] <- "'Gaussian'"
    Config_Model[offset+12] <- paste(priorParams$priorCoefficient[priorParams$controlNumber == j],
                                     as.character(priorParams$priorCoefficientUnc[priorParams$controlNumber == j]),
                                     sep = ",")
    Config_Model[offset+13] <- paste0('"c', j, '"')
    Config_Model[offset+14] <- priorParams$priorExponent[priorParams$controlNumber == j]
    Config_Model[offset+15] <- "'Gaussian'"
    Config_Model[offset+16] <- paste(priorParams$priorExponent[priorParams$controlNumber == j],
                                     as.character(priorParams$priorExponentUnc[priorParams$controlNumber == j]),
                                     sep = ",")
  }
  write.table(Config_Model, priorParamsPath, row.names = F, col.names = F, quote = F)
  return(numCtrls)
}
