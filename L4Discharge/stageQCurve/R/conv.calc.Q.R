##############################################################################################
#' @title Convert units and re-calculate discharge

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function GETs data from the NEON REST Service after retrieving a 
#' transition object that must be set as an environment variable called "TRANSID" using 
#' two other environment variables that store OS (called osdataserviceurl, required) and 
#' IS (called cdsurl, optional) REST URLs.

#' @param stageData A dataframe containing stage and discharge data for a site/water year [dataframe]
#' @param dischargeData A dataframe containing distance and velocity point measurements for the 
#' site/water year that can be used to re-calculate discharge for the stageData [dataframe]

#' @return This function returns an XML file parsed into a data frame

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
##############################################################################################
conv.calc.Q <- function(
  stageData,
  dischargeData
){
  ##### Constants #####
  cmTom <- 1/100 #Conversion factor for centimeters to meters
  cmsToms <- 1/1000 #Conversion factor for centimetersPerSecond to metersPerSecond
  m3sTolps <- 1000 #Conversion factor for cubicMetersPerSecond to litersPerSecond
  
  #Convert all units to be the same: m, mps, and lps
  changeStageIDs <- stageData$recorduid[stageData$streamStageUnits != 'm']
  changeQIDs <- stageData$recorduid[stageData$totalDischargeUnits != 'lps']
  changeVelocityIDs <- stageData$recorduid[stageData$averageVelocityUnits != 'm/s']
  changeDepthIDs <- stageData$recorduid[stageData$waterDepthUnits != 'm']
  changeTapeDistIDs <- stageData$recorduid[stageData$tapeDistanceUnits != 'm']
  
  #Convert stage and discharge in the stageData table
  stageData$totalDischarge <- as.numeric(stageData$totalDischarge)
  stageData$streamStage <- as.numeric(stageData$streamStage)
  
  stageData$totalDischarge[stageData$recorduid %in% changeQIDs] <- stageData$totalDischarge[stageData$recorduid %in% changeQIDs] * m3sTolps
  stageData$streamStage[stageData$recorduid %in% changeStageIDs] <- stageData$streamStage[stageData$recorduid %in% changeStageIDs] * cmTom
  
  #Convert water depth, velocity, and tape distance
  dischargeData$tapeDistance <- as.numeric(dischargeData$tapeDistance)
  dischargeData$averageVelocity <- as.numeric(dischargeData$averageVelocity)
  dischargeData$waterDepth <- as.numeric(dischargeData$waterDepth)
  dischargeData$sectionFlow <- as.numeric(dischargeData$sectionFlow)
  dischargeData$averageVelocity <- as.numeric(dischargeData$averageVelocity)
  
  dischargeData$tapeDistance[dischargeData$recorduid %in% changeTapeDistIDs] <- dischargeData$tapeDistance[dischargeData$recorduid %in% changeTapeDistIDs] * cmTom
  dischargeData$averageVelocity[dischargeData$recorduid %in% changeVelocityIDs] <- dischargeData$averageVelocity[dischargeData$recorduid %in% changeVelocityIDs] * cmsToms
  dischargeData$waterDepth[dischargeData$recorduid %in% changeDepthIDs] <- dischargeData$waterDepth[dischargeData$recorduid %in% changeDepthIDs] * cmTom
  
  #Recalculate discharge based off of individual measurements
  allRecordID <- unique(stageData$recorduid)
  stageData$calcQ <- NA
  stageData$recalculatedL1QF <- NA
  stageData$L1DataQF <- NA
  
  for(j in seq(along = allRecordID)){
    dataForCalculations <- dischargeData[dischargeData$recorduid == allRecordID[j],]
    dataForCalculations$stationNumber <- as.numeric(dataForCalculations$stationNumber)
    
    #Check for all stations and error if not
    if(max(dataForCalculations$stationNumber) != length(dataForCalculations$stationNumber)){
      print(paste0("Warning, incomplete record for ", allRecordID[j],". Could not re-calculate discharge from measurements"))
      next
    }
    
    totalDischarge <- 0
    
    for(i in seq(along = dataForCalculations$stationNumber)){
      sectionDischarge <- NA
      depth <- dataForCalculations$waterDepth[dataForCalculations$stationNumber == i]
      velocity <- dataForCalculations$averageVelocity[dataForCalculations$stationNumber == i]
      
      previousDist <- dataForCalculations$tapeDistance[dataForCalculations$stationNumber == (i-1)]
      currentDist <- dataForCalculations$tapeDistance[dataForCalculations$stationNumber == i]
      nextDist <- dataForCalculations$tapeDistance[dataForCalculations$stationNumber == (i+1)]
      
      if(length(previousDist) < 1){
        width <- nextDist - currentDist
      }else if(length(nextDist) < 1){
        width <- currentDist - previousDist
      }else{
        width <- (nextDist - currentDist)/2 + (currentDist - previousDist)/2
      }
      #Check to see if the data line up, otherwise, re-calculate
      #Cannot recalculate all of them due to rounding issues for low flows
      roundedVelocity <- round(dataForCalculations$sectionFlow[dataForCalculations$stationNumber == i]/(width * depth * m3sTolps), 2)
      if(!is.na(roundedVelocity) && 
         roundedVelocity == dataForCalculations$averageVelocity[dataForCalculations$stationNumber == i]){
        sectionDischarge <- dataForCalculations$sectionFlow[dataForCalculations$stationNumber == i]
      }else{
        sectionDischarge <- width * depth * velocity * m3sTolps
      }
      totalDischarge <- totalDischarge + sectionDischarge
      
    }
    stageData$calcQ[stageData$recorduid == allRecordID[j]] <- totalDischarge

  }
  
  #Set QF for recalculated data
  stageData$recalculatedL1QF[stageData$calcQ != stageData$totalDischarge] <- 1
  stageData$recalculatedL1QF[stageData$calcQ == stageData$totalDischarge] <- 0
  stageData$L1DataQF <- stageData$dataQF
  
  return(stageData)
}
