##############################################################################################
#' @title Writes out the Config_ControlMatrix and Config_Model text files for BaM

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function writes out the ControlMatrix and Model configurations for BaM.

#' @import utils
#' @import neonUtilities

#' @param site 4 letter SITE code [string]
#' @param searchIntervalStartDate Start date for search interval [POSIXct]
#' @param searchIntervalEndDate Start date for search interval [POSIXct]
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
##############################################################################################
txt.out.ctrl.and.prior.parm.ext <- function(
  site,
  searchIntervalStartDate,
  searchIntervalEndDate,
  controlMatrixPath,
  priorParamsPath,
  downloadedDataPath
  ){
  
  #Get control data from geomorphology survey
  if(file.exists(paste0(downloadedDataPath,"NEON_morphology-stream.zip"))){
    stackByTable(dpID="DP4.00131.001",filepath=paste0(downloadedDataPath,"NEON_morphology-stream.zip"))
    ctrlInfo  <- read.csv(paste(downloadedDataPath,"NEON_morphology-stream","stackedFiles","geo_controlInfo.csv", sep = "/"))
    priorParams  <- read.csv(paste(downloadedDataPath,"NEON_morphology-stream","stackedFiles","geo_priorParameters.csv", sep = "/"))
  }else{
    availableFiles <- list.files(downloadedDataPath)
    ctrlInfo  <- read.csv(paste(downloadedDataPath,availableFiles[grepl("geo_controlInfo",availableFiles)], sep = "/"))
    priorParams  <- read.csv(paste(downloadedDataPath,availableFiles[grepl("geo_priorParameters",availableFiles)], sep = "/"))
  }
  
  
  #Subset for the site of interest
  if(length(ctrlInfo$siteID)>0){
    ctrlInfo <- ctrlInfo[ctrlInfo$siteID == site,]
    priorParams <- priorParams[priorParams$siteID == site,]
  }else if(length(ctrlInfo$locationID)>0){
    ctrlInfo <- ctrlInfo[ctrlInfo$locationID == site,]
    priorParams <- priorParams[priorParams$locationID == site,]
  }else{
    stop("Valid location field could not be found.")
  }
  
  
  if(nrow(ctrlInfo)<1){
    failureMessage <- "Zero (0) control activation records were retrieved"
    stop(failureMessage)
  }
  if(nrow(priorParams)<1){
    failureMessage <- "Zero (0) prior parameters records were retrieved"
    stop(failureMessage)
  }
  if(attr(ctrlInfo, "class") == "try-error"){
    failureMessage <- "Control activation data could not be retrieved"
    stop(failureMessage)
  }
  if(attr(priorParams, "class") == "try-error"){
    failureMessage <- "Prior parameters could not be retrieved"
    stop(failureMessage)
  }
  
  #Pick which geomorph data to use
  ctrlDates <- unique(ctrlInfo$startDate)
  priorParamsDates <- unique(priorParams$startDate)
  
  if(sum(ctrlDates < searchIntervalStartDate, na.rm = T) > 1){
    ctrlDateToUse <- max(ctrlDates[ctrlDates < searchIntervalStartDate])
  }else{
    ctrlDateToUse <- min(ctrlDates)
  }
  
  if(sum(priorParamsDates < searchIntervalStartDate, na.rm = T) > 1){
    priorParamsDateToUse <- max(priorParamsDates[priorParamsDates < searchIntervalStartDate])
  }else{
    priorParamsDateToUse <- min(priorParamsDates)
  }
  
  if(ctrlDateToUse == priorParamsDateToUse){
    finalDateToUse <- ctrlDateToUse
    print(paste0("Prior parameters and controls from ", finalDateToUse, " will be used"))
  }else{
    failureMessage <- "Dates for prior parameters and controls do not match"
    stop(failureMessage)
  }
  
  #Use this control and prior parameters data
  ctrlInfo <- ctrlInfo[ctrlInfo$startDate == finalDateToUse,]
  priorParams <- priorParams[priorParams$startDate == finalDateToUse,]
  
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
                                    priorParams$priorActivationStageUnc[priorParams$controlNumber == j],
                                    sep = ",")
    Config_Model[offset+9] <- paste0('"a', j, '"')
    Config_Model[offset+10] <- priorParams$priorCoefficient[priorParams$controlNumber == j]
    Config_Model[offset+11] <- "'Gaussian'"
    Config_Model[offset+12] <- paste(priorParams$priorCoefficient[priorParams$controlNumber == j],
                                     priorParams$priorCoefficientUnc[priorParams$controlNumber == j],
                                     sep = ",")
    Config_Model[offset+13] <- paste0('"c', j, '"')
    Config_Model[offset+14] <- priorParams$priorExponent[priorParams$controlNumber == j]
    Config_Model[offset+15] <- "'Gaussian'"
    Config_Model[offset+16] <- paste(priorParams$priorExponent[priorParams$controlNumber == j],
                                     priorParams$priorExponentUnc[priorParams$controlNumber == j],
                                     sep = ",")
  }
  write.table(Config_Model, priorParamsPath, row.names = F, col.names = F, quote = F)
  return(numCtrls)
}
