##############################################################################################
#' @title Stream discharge formatting

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function formats data from the NEON Salt-based discharge (DP1.20193.001)
#' data product to calculate stream discharge from slug and constant-rate injections. 

#' @importFrom utils read.csv

#' @param backgroundFieldCondData Field data for conductivity timeseries, from 
#' sbd_backgroundFieldCondData table for NEON data [dataframe]
#' @param backgroundDataSalt Field data with background conductivity data, from 
#' sbd_backgroundFieldCondData table for NEON data [dataframe]
#' @param fieldDataSite Field data with experiment type information, from sbd_fieldData 
#' table for NEON data [dataframe]
#' @param plateauDataCond Field data with plateau conductivity data, from 
#' sbd_plateauMeasurementFieldData table for NEON data [dataframe]
#' @param plateauDataSalt Field data for salt sample data, from sbd_plateauSampleFieldData 
#' table for NEON data [dataframe]
#' @param externalLabDataSalt External lab data for injectate, background, and plateau, from 
#' sbd_externalLabDataSalt table for NEON data [dataframe]

#' @return This function returns one data frame formatted for use with def.calc.Q.slug.R 
#' and def.calc.Q.constantRate.R to calculate stream discharge

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords surface water, streams, rivers, discharge, tracer, salt-based, slug, constant rate

#' @examples
#' #Using the example data in this package
#' #dataDirectory <- paste(path.package("neonStreamQ"),"inst\\extdata", sep = "\\")
#' #sbdFormatted <- def.format.Q(dataDir = dataDirectory)

#' @seealso def.calc.Q.slug.R and def.calc.Q.inj.R to calculate stream discharge

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-08-03)
#     original creation
#   Kaelin M. Cawley (2017-05-08)
#     added additional functionality for getting data from the NEON data API
#   Kaelin M. Cawley (2020-12-03)
#     updated to allow users to use data already loaded to R since there are so many options
#     of how to get it there now
##############################################################################################
#This code is for calculating salt-based discharge for a slug
def.format.Q <- function(
  backgroundFieldCondData = NULL,
  backgroundDataSalt = NULL,
  fieldDataSite = NULL,
  plateauDataCond = NULL,
  plateauDataSalt = NULL,
  externalLabDataSalt = NULL
) {
  
  fieldDataSite$namedLocation <- NULL #So that the merge goes smoothly
  
  #A little error handling until there is a de-duping function
  extDuplicates <- externalLabDataSalt$saltSampleID[duplicated(externalLabDataSalt$saltSampleID)]
  if(length(extDuplicates) > 0){
    stop(paste("Reconcile duplicate external lab sample(s):",paste0(extDuplicates, collapse = ", ")))
  }
  
  #Merge the backgroundFieldCondData and fieldDataSite tables
  loggerSiteData <- merge(backgroundFieldCondData, 
                          fieldDataSite, 
                          by = c('siteID', 'startDate'), 
                          all = T)
  
  #Create input file for Q calculations
  outputDFNames <- c(
    'siteID',
    'namedLocation',
    'startDate',
    'injectionType',
    
    'slugTracerMass',
    'slugPourTime',
    
    'injectateSampleID',
    'injConcLab',
    'injConcCalc',
    'constantRateTracerMass',
    'slugTracerMass',
    'carboyVolume',
    'dripStartTime',
    'injRate',
    
    'backgroundConc',
    'plateauConc',
    'hoboSampleID'
  )
  outputDF <- data.frame(matrix(data=NA, ncol=length(outputDFNames), nrow=length(loggerSiteData$siteID)))
  names(outputDF) <- outputDFNames
  
  #Fill in the fields from the loggerSiteData table
  for(i in seq(along = names(outputDF))){
    if(names(outputDF)[i] %in% names(loggerSiteData)){
      outputDF[,i] <- loggerSiteData[,which(names(loggerSiteData) == names(outputDF)[i])]
    }
  }
  
  #Use dripStartTime where slugPoutTime is missing
  outputDF$slugPourTime[is.na(outputDF$slugPourTime) & outputDF$injectionType != 'NaCl' & !is.na(outputDF$dripStartTime)] <-
    outputDF$dripStartTime[is.na(outputDF$slugPourTime) & outputDF$injectionType != 'NaCl' & !is.na(outputDF$dripStartTime)]
  
  #Calculate mean injection rate
  outputDF$injRate <- apply(loggerSiteData[,which(names(loggerSiteData) == "dripRateStart"|names(loggerSiteData) == "dripRateEnd")], 1, mean, na.rm = T)
  
  #Fill in fields that are means of reps
  plateauFields <- c('specificConductanceRep1',
                     'specificConductanceRep2',
                     'specificConductanceRep3',
                     'specificConductanceRep4',
                     'specificConductanceRep5')
  
  for(i in seq(along = outputDF$siteID)){
    
    siteID <- outputDF$siteID[i]
    startDate <- outputDF$startDate[i]
    station <- outputDF$namedLocation[i]
    stationType <- substr(station, 6, nchar(station))
    
    #Use last background startDate where slugPourTime is missing for NaBr injections
    if(is.na(outputDF$slugPourTime[i]) & outputDF$injectionType[i] %in% c("NaBr")){
      outputDF$slugPourTime[i] <- min(backgroundDataSalt$collectDate[backgroundDataSalt$siteID == siteID &
                                                                       backgroundDataSalt$collectDate == startDate])
    }
    
    
    #Pull in external lab data for NaCl and NaBr injections
    if(outputDF$injectionType[i] %in% c("NaCl","NaBr")){
      #Find plateau values to average for NaCl or NaBr injections
      pValues <- c(t(plateauDataCond[plateauDataCond$startDate == startDate &
                                       plateauDataCond$namedLocation == station,
                                     names(plateauDataCond) %in% plateauFields]))
      
      #Populate injectate concentration
      try(outputDF$injConcLab[i] <- unique(externalLabDataSalt$finalConcentration[
        externalLabDataSalt$saltSampleID == outputDF$injectateSampleID[i]]), silent = T)
      
      #Calculate the mean
      outputDF$plateauCond[i] <- mean(pValues, na.rm = T)
      
      #Fill in background concentration data
      try(outputDF$backgroundConc[i] <- 
            unique(externalLabDataSalt$finalConcentration[
              externalLabDataSalt$namedLocation == station &
                externalLabDataSalt$startDate  == startDate & 
                grepl(paste0(".B",substr(station,nchar(station),nchar(station)),"."),
                      externalLabDataSalt$saltSampleID)]), silent = T)
      
      #Fill in plateau concentration data for constant rate injection
      pSaltConc <- externalLabDataSalt$finalConcentration[
        externalLabDataSalt$namedLocation == station &
          externalLabDataSalt$startDate == startDate &
          !grepl(paste0(".B",substr(station,nchar(station),nchar(station)),"."),
                externalLabDataSalt$saltSampleID)]
      
      #Remove outliers TBD
      
      #Calculate the mean plateau concentration
      outputDF$plateauConc[i] <- mean(pSaltConc, na.rm = T)
    }
    
  }
  
  #Calculate concentration of constant-rate injectate if no external lab data
  outputDF$injConcCalc[outputDF$injectionType %in% c("NaBr","NaCl","model - CRI") & 
                         !is.na(outputDF$constantRateTracerMass) & 
                         !is.na(outputDF$carboyVolume)] <- outputDF$constantRateTracerMass[outputDF$injectionType %in% c("NaBr","NaCl", "model - CRI") & 
                                                                                             !is.na(outputDF$constantRateTracerMass) & 
                                                                                             !is.na(outputDF$carboyVolume)] / outputDF$carboyVolume[outputDF$injectionType %in% c("NaBr","NaCl", "model- CRI") & 
                                                                                                                                                      !is.na(outputDF$constantRateTracerMass) & 
                                                                                                                                                      !is.na(outputDF$carboyVolume)]
  
  return(outputDF)
}
