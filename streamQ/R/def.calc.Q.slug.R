##############################################################################################
#' @title Salt-based discharge calculations for a slug injection

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function calculates stream discharge from a slug salt tracer injection. 
#' This function will likely only work well for the NEON conductivity logger data.

#' @importFrom pracma trapz
#' @importFrom neonUtilities stackByTable
#' @importFrom utils read.csv
#' @importFrom grDevices dev.new
#' @importFrom grDevices dev.off
#' @importFrom graphics identify
#' @importFrom graphics plot
#' @importFrom graphics title
#' @importFrom graphics abline
#' @importFrom graphics lines
#' @importFrom graphics points
#' @importFrom stats lsfit

#' @param inputFile Name of the data fram containing the information needed to calculate 
#' discharge from a slug tracer injection. If the headers are named: "slugMass" and 
#' "slugPourTime" the slugMass and slugPourTime paramaters don't need to be defined. 
#' Otherwise, the names of the columns need to be input for the function to work.
#' @param slugMass Mass of the tracer injectate [g]
#' @param slugVol Volume of the tracer injectate [L]
#' @param dataDir User identifies the directory that contains the zipped data
#' @param site User identifies the site(s), defaults to "all" [string]

#' @return This function returns stream discharge [lps] appended as an additional column 
#' to the input data frame along with a quality flag (slugQF) where 1 means that a peak 
#' wasn't detected, 2 means the rising limb was not resolved, and 3 means the falling 
#' limb did not return to basline concentration.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords surface water, streams, rivers, discharge, tracer, salt-based, slug

#' @examples
#' #sbdDataPlusSlugQ <- def.calc.Q.slug(inputFile = sbdFormatted)

#' @seealso def.calc.Q.inj.R for calculating stream discharge from a constant rate injection

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-08-03)
#     original creation
#   Kaelin M. Cawley (2017-05-08)
#     added additional functionality for getting data from the NEON data API
##############################################################################################
#This code is for calculating salt-based discharge for a slug
def.calc.Q.slug <- function(
  inputFile,
  slugMass = inputFile$slugTracerMass,
  slugVol = inputFile$carboyVolume,
  dataDir = paste0(getwd(),"/NEON_discharge-stream-saltbased.zip"),
  site = "all"
) {
  
  ##### Constants #####
  #Conversion factor between specific conductance and concentration 
  #Assumes Na+ and Cl- are both contributing to the conductivity
  NaClCond = 0.12646 #S L mol-1 cm-1 #CRC handbook of chemistry and physics
  NaClmw = 58.44 #molecular weight of sodium chloride g/mol
  cuSmS = 1000000 #Convert Siemens to microsiemens
  cgmg = 1000 #Convert g to mg
  
  #Peak threshold
  #pkRange = 5 #Number of points to test for peak slope
  #pkNum = 4 #Nummber of point that must be increasing/decreasing for peak slope
  #pCond = 1 #Percent of the final baseline
  #condDiff = 25 #Minimum difference between the peak and baseline conductivity
  
  ##### Calculations #####
  dpID <- "DP1.20193.001"
  folder <- FALSE
  if(dataDir == "API"){
    filepath <- paste(getwd(), "/filesToStack", substr(dpID, 5, 9), sep="")
  }else{
    filepath <- dataDir
  }
  
  #Stack field and external lab data if needed
  if(!dir.exists(paste(gsub("\\.zip","",filepath), "stackedFiles", sep = "/"))&&
     file.exists(filepath)){
    #For when data is coming in from the API
    dpID <- "DP1.20193.001"
    #Pull files from the API to stack
    if(dataDir == "API"){
      folder <- FALSE
      dataFromAPI <- zipsByProduct(dpID,site,package="expanded",check.size=TRUE)
      filepath <- paste(getwd(), "/filesToStack", substr(dpID, 5, 9), sep="")
      folder <- TRUE
      API <- TRUE
    }else{
      filepath = dataDir
    }
    stackByTable(dpID=dpID,filepath=filepath,folder=folder)
    filepath <- paste(gsub("\\.zip","",filepath), "stackedFiles", sep = "/")
  }else if(dir.exists(paste(gsub("\\.zip","",filepath), "/stackedFiles", sep = "/"))){
    filepath <- paste(gsub("\\.zip","",filepath), "stackedFiles", sep = "/")
  }
  
  if(dir.exists(filepath)&&
     any(grepl("conductivityFieldData",list.files(filepath)))){
    #Read in stacked logger data
    #Allows for using the reaeration tables in addition to the salt-based discharge tables
    allFiles <- list.files(filepath)
    loggerFile <- allFiles[grepl("conductivityFieldData", allFiles)]
    loggerData <- read.csv(
      paste(filepath,loggerFile,sep = "/"), 
      stringsAsFactors = F)
  }else{
    print("Error, stacked files could not be read in for logger conductivity data")
    return(NULL)
  }

  #A little confusing to have this in different places
  #Made one equation below in the loop
  # #Convert the injectate mass from g to M
  # slugMass <- slugMass/NaClmw
  # 
  # #Convert the corrected slug concentration to conductivity
  # slugCond <- slugMass * NaClCond * cuSmS * 1/cgmg

  inputFile$Q.slug <- NA
  inputFile$slugQF <- NA
  for(i in seq(along = inputFile$siteID)){
    currEventID <- inputFile$eventID[i]
    if(is.na(inputFile$injectionType[i]) ||
       inputFile$injectionType[i] == "NaCl" ||
       nrow(loggerData[loggerData$hoboSampleID == inputFile$hoboSampleID[i],]) == 0){
      next
    }
    stationLoggerData <- loggerData[loggerData$hoboSampleID == inputFile$hoboSampleID[i],]
    stationLoggerData <- stationLoggerData[order(stationLoggerData$measurementNumber),]
    
    #If low range isn't collected use the full range
    ifelse(all(is.na(stationLoggerData$lowRangeSpCondNonlinear)),
           condData <- stationLoggerData$fullRangeSpCondNonlinear,
           condData <- stationLoggerData$lowRangeSpCondNonlinear)
    
    #Plot the logger files to choose the integration range
    #Pretty much can't count on the automated version to always work
    #Have to manually pick the peak range
    #Create a plot where users select the range to pick the peak
    invisible(dev.new(noRStudioGD = TRUE))
    plot(stationLoggerData$measurementNumber, condData, xlab = "Measurement Number", ylab = "Specific Conductance")
    #Have users choose if the plot has a defined peak
    points(x = c(length(condData)*.1,length(condData)*.9),
           y = c(max(condData,na.rm = T)*.8,max(condData,na.rm = T)*.8), 
           col = c("green","red"), 
           lwd = 2, 
           pch = 19,
           cex = 2)
    title(main = paste0("Click green dot (upper lefthand) if the peak/plateau is identifiable. \nClick red dot (upper righthand) if not identifiable.\n",currEventID))
    badPlotBox <- identify(x = c(length(condData)*.1,length(condData)*.9),
                           y = c(max(condData,na.rm = T)*.8,max(condData,na.rm = T)*.8), 
                           n = 1, 
                           tolerance = 0.25, 
                           labels = c("Good", "Bad"))
    Sys.sleep(1)
    invisible(dev.off())
    
    if(badPlotBox==1){
      #If things look good, move on
      invisible(dev.new(noRStudioGD = TRUE))
      plot(stationLoggerData$measurementNumber, condData, xlab = "Measurement Number", ylab = "Specific Conductance")
      title(main = paste0("At least 5 baseline points left of peak should be included in selection. \nThis is about the width of the '+' cursor on most screens.\n",stationLoggerData$hoboSampleID[i]))
      ans <- identify(stationLoggerData$measurementNumber, n = 2, condData, pos = F, tolerance = 0.25)
      Sys.sleep(1)
      invisible(dev.off())
      condData <- condData[min(ans):max(ans)]
    }else{
      return(NULL)
    }
    

    # else{
    #   
    #   #Find slug peak, start with the max and look for 5 on either side that decrease
    #   peakLoc <- min(which(condData == max(condData, na.rm = T)))
    #   testedPeaks <- peakLoc
    #   dataSeq <- seq(along = condData)
    #   
    #   for(j in dataSeq){
    #     peakLocStart <- ifelse((peakLoc-pkRange)<1,1,(peakLoc-pkRange))
    #     peakLocEnd <- ifelse((peakLoc+pkRange)>length(condData),length(condData),(peakLoc+pkRange))
    #     
    #     peakRiseA <- condData[peakLocStart:(peakLoc-1)]
    #     peakRiseB <- condData[(peakLocStart+1):(peakLoc)]
    #     peakFallA <- condData[(peakLoc+1):peakLocEnd]
    #     peakFallB <- condData[peakLoc:(peakLocEnd-1)]
    #     
    #     riseTest <- sum(peakRiseB-peakRiseA > 0) >= (pkNum/pkRange)
    #     fallTest <- sum(peakFallB-peakFallA > 0) >= (pkNum/pkRange)
    #     
    #     if(riseTest & fallTest){
    #       break
    #     }else{
    #       peakLoc <- min(which(condData == max(condData[!(dataSeq %in% testedPeaks)], na.rm = T)), na.rm = T)
    #       testedPeaks[length(testedPeaks) +1] <- peakLoc
    #     }
    #     
    #   }
    #   
    #   if(peakLoc == min(condData, na.rm = T)){
    #     inputFile$slugQF[i] <- 1
    #     warning(paste('No slug peak not detected for:', inputFile$hoboSampleID[i]))
    #     next
    #   }
    #   
    #   if(length(condData)<10){
    #     inputFile$slugQF[i] <- 2
    #     warning(paste('Conductivity dataset less than 10 records long for: ', inputFile$hoboSampleID[i]))
    #     next
    #   }
    #   
    #   #Find pre-slug baseline, work left from the peak
    #   intBL <- NA
    #   for(k in seq(from = peakLoc, to = 11)){
    #     dataRange <- (k-10):k
    #     
    #     #Check that it has a flat slope and is a flat line
    #     if(abs(lsfit(dataRange, condData[dataRange])[[1]][2]) < 0.1){
    #       intBL <- (k-10)
    #       break
    #     }
    #   }
    #   
    #   if(is.na(intBL) || condData[intBL] > (condData[peakLoc] - condDiff)){
    #     inputFile$slugQF[i] <- 2
    #     warning(paste('Defined rising limb not detected for:', inputFile$hoboSampleID[i]))
    #     next
    #   }
    #   
    #   #Find post-slug baseline from the baseline conductivity
    #   finBL <- NA
    #   finBL <- min(which(condData[peakLoc:length(condData)] <= (condData[intBL]*(1 + pCond/100))), na.rm = T)+peakLoc
    #   
    #   if(condData[finBL] < (condData[intBL] * 0.75) | is.na(finBL) | length(finBL) < 1){
    #     inputFile$slugQF[i] <- 3
    #     warning(paste('Incomplete falling limb:', inputFile$hoboSampleID[i]))
    #     next
    #   }
    #   
    #   if(plot == T){
    #     invisible(dev.new(noRStudioGD = TRUE))
    #     plot((intBL-20):(finBL+20), condData[(intBL-20):(finBL+20)], xlab = "Measurement Number", ylab = "Specific Conductance")
    #     title(main = stationLoggerData$hoboSampleID[i])
    #     baseline <- lines((intBL-20):(finBL+20), rep(mean(condData[intBL:(intBL+5)]), times = (finBL-intBL+41)), col = 'blue', lwd = 2)
    #     intStart <- abline(v = intBL, col = "green", lwd = 2)
    #     intEnd <- abline(v = finBL, col = "red", lwd = 2)
    #   }
    #   
    #   condData <- condData[intBL:finBL]
    #   
    # }
    
    #Background correct the logger data
    condData <- condData - mean(condData[1:5])
    
    #Calculate the area under the conductivity time series
    areaResponse <- trapz(1:length(condData), condData)
    
    #Convert from integration over measurement number to time
    areaResponse <- areaResponse*10 #Each measurement is 10 seconds apart
    
    #Calculate discharge according to equation 4 of Sappa et al., Validation of 
    #salt dilution method for discharge measurements in the upper valley of aniene river (central italy)
    #inputFile$Q.slug[i] <- slugCond[i] * slugVol[i]/ areaResponse
    inputFile$Q.slug[i] <- slugMass[i]*cgmg/(NaClmw/NaClCond*cgmg*1/cuSmS*areaResponse)
    
  }
  
  return(inputFile)
  
}
