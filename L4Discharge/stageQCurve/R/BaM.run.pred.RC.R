# ############## For Testing ###########################
# DIRPATH = Sys.getenv("DIRPATH")
# BAMFOLD = Sys.getenv("BAMFOLD")
# BAMFILE = Sys.getenv("BAMFILE")
# BAMWS = Sys.getenv("BAMWS")
# minH
# maxH
# ######################################################

##############################################################################################
#' @title Run predictions for rating curve

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function writes out configuration files and runs BaM in prediction 
#' mode for ratings curves

#' @param DIRPATH An environment variable that contains the location of the files in 
#' the Docker container [string]
#' @param BAMFOLD An environment variable that contains the folder name of the BaM exe in 
#' the Docker container [string]
#' @param BAMFILE An environment variable that contains the name of the BaM exe in 
#' the Docker container [string]
#' @param BAMWS An environment variable that contains the location of the BaM config 
#' files in the Docker container [string]
#' @param minH Minimum stage value to evaluate rating curve over [numeric]
#' @param maxH Maximum stage value to evaluate rating curve over [numeric]

#' @return This function writes out configurations and runs BaM in prediction mode

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
##############################################################################################
BaM.run.pred.RC <- function(
  DIRPATH = Sys.getenv("DIRPATH"),
  BAMFOLD = Sys.getenv("BAMFOLD"),
  BAMFILE = Sys.getenv("BAMFILE"),
  BAMWS = Sys.getenv("BAMWS"),
  minH,
  maxH
  ){
  
  #Plot rating curve with gaugings using a prediction
  #And local gaugings data
  RunOptionsName <- "Config_RunOptions.txt"
  txt.out.run.opts(runType = "pred", RunOptionsPath = paste0(DIRPATH, BAMWS, RunOptionsName))
  
  #Write out predictions master file
  predMasterName <- "Config_Pred_Master.txt"
  Config_Pred_Master <- readLines(paste0(DIRPATH, BAMWS, predMasterName))
  Config_Pred_Master[1] <- gsub("[0-9] ","4 ",Config_Pred_Master[1])
  Config_Pred_Master[2] <- gsub("'.*'","'Config_Pred_Prior.txt'",Config_Pred_Master[2])
  Config_Pred_Master[3] <- gsub("'.*'","'Config_Pred_RCMaxpost.txt'",Config_Pred_Master[3])
  Config_Pred_Master[4] <- gsub("'.*'","'Config_Pred_RCParamU.txt'",Config_Pred_Master[4])
  Config_Pred_Master[5] <- gsub("'.*'","'Config_Pred_RCTotalU.txt'",Config_Pred_Master[4])
  writeLines(Config_Pred_Master,paste0(DIRPATH, BAMWS, predMasterName))
  
  #Write out Hgrid.txt
  Hgrid <- seq(from = minH, to = maxH, length.out = 181)
  #Hgrid <- round(Hgrid, digits = 3)
  write.table(Hgrid,paste0(DIRPATH,BAMWS,"data/Hgrid.txt"),row.names = F,col.names = F)
  
  #The prediction configurations should be good with the name and Nobs, change if needed
  
  #Write out the gaugings file if needed
  if(!file.exists(paste0(DIRPATH, BAMWS, "data/Gaugings.txt"))){
    gaugingsStageData <- read.table("~/GitHub/NEON-stream-discharge/L4Discharge/data/L1_Results_sdrc_gaugeDischargeMeas_pub.txt", header = T)
    #Write out the formatted data file to data folder for BaM
    gagNam <- c('H','uH','bH','bHindx','Q','uQ','bQ','bQindx')
    gaugings <- data.frame(matrix(data=NA, ncol=length(gagNam), nrow=length(gaugingsStageData$gaugeHeight)))
    names(gaugings) <- gagNam
    
    #Zeroes below assume no uncertainty, may want to change that
    gaugings$H <- gaugingsStageData$gaugeHeight #Stream stage values (m)
    gaugings$uH <- 0.00 #May include in the future
    gaugings$bH <- 0.00
    gaugings$bHindx <- 0.00
    gaugings$Q <- gaugingsStageData$streamDischarge #Stream discharge values (lps), re-calculated
    gaugings$uQ <- gaugingsStageData$streamDischargeUnc
    gaugings$bQ <- 0.00
    gaugings$bQindx <- 0.00
    
    write.table(gaugings,
                paste0(DIRPATH, BAMWS, "data/Gaugings.txt"),
                sep = "\t",
                row.names = F,
                quote = F)
    
    #Write configuration and data files to the BaM folder for the water year
    Config_Data <- readLines(paste0(DIRPATH, BAMWS, "Config_Data.txt"))
    Config_Data[3] <- gsub("[0-9]{1,6}",nrow(gaugings),Config_Data[3]) #Replace the existing value with the current value
    writeLines(Config_Data, paste0(DIRPATH, BAMWS, "Config_Data.txt")) #Specifies the calibration data
  }
  
  #R doesn't like the trailing slash
  BaM_path <- paste0(DIRPATH,gsub("/$","",BAMFOLD))
  if(!file.exists(BaM_path)){
    failureMessage <- "Path to BaM executable not found"
    stop(failureMessage)
  }
  #Run the exe
  setwd(BaM_path)
  system2(BAMFILE)
  
  return(Hgrid)
  
}
