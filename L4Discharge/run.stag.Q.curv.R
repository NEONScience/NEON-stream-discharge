##############################################################################################
#' @title Stage-Discharge Rating Curve Docker Script

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description This script uses the BaM executable to calculate the parameters that define
#' the stage-discharge relationship for a site and water year using NEON data.

#' @details FILL IN LATER

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-03-17)
#     code updates to fit updated workflow for generating stage-discharge rating curves
##############################################################################################

## -- TO RUN THE CODE OUTSIDE OF A DOCKER CONTAINER, UNCOMMENT AND RUN LINES __ - __ -- ##
# The paths can get pretty long (>260 characters), which can cause trouble with the data stacker in windows. Choose wisely.

# User inputs for site and date
# If you enter a startDate that is not YYYY-10-01, the script will determine the water year (10-01 - 09-30) that started immediately before the date entered here and use it as the startDate

# Set global environment variables (see stageQCurve package readme for a description of each variable)
Sys.setenv(DIRPATH = "C:/Users/nickerson/Documents/GitHub/NEON-stream-discharge/L4Discharge/",
           BAMFOLD="BaM_beta/",
           BAMFILE="BaM_MiniDMSL.exe",#Windows version
           #BAMFILE="BaM_exe",#Linux version
           DATAWS="C:/Users/nickerson/Documents/stageQCurve_data/",
           BAMWS="BaM_beta/BaM_BaRatin/",
           STARTDATE = "2018-10-01",
           SITE = "WLOU")

# Call global environment variables into local environment
DIRPATH = Sys.getenv("DIRPATH")
BAMFOLD = Sys.getenv("BAMFOLD")
BAMFILE = Sys.getenv("BAMFILE")
DATAWS = Sys.getenv("DATAWS")
BAMWS = Sys.getenv("BAMWS")
startDate = Sys.getenv("STARTDATE")
site = Sys.getenv("SITE")

# # Need to run this periodically if you're running the code outside of the Docker container as NEON packages get updated
# library(devtools)
# install_github("NEONScience/NEON-utilities/neonUtilities", force = TRUE, dependencies = TRUE)
# install_github("NEONScience/NEON-stream-discharge/L4Discharge/stageQCurve", force = TRUE, dependencies = TRUE)
# Load needed library for Docker testing prior to GitHub package release
# setwd("/app/L4_discharge/")
# devtools::install("stageQCurve")
# library(stageQCurve)

# Environment options and configurations
options(stringsAsFactors = F)
Sys.setenv(TZ='UTC')
library(neonUtilities)
library(stageQCurve)

### --- RUN MAIN FUNCTION TO GENERATE A RATING CURVE --- ###

# We will split rating curve segments before proceeding with the remaining script

# If data has been downloaded using neonUtilities::zipsByProduct() and saved to DATAWS
if(file.exists(paste0(Sys.getenv("DATAWS"),"filesToStack00133"))){
  # Stack the tables if they have not been already
  if (!file.exists(paste0(Sys.getenv("DATAWS"),"filesToStack00133/stackedFiles"))) {
    neonUtilities::stackByTable(paste0(Sys.getenv("DATAWS"),"filesToStack00133"))
  }
  curveIdentification <- try(read.csv(paste(Sys.getenv("DATAWS"),"filesToStack00133","stackedFiles","geo_curveIdentification.csv", sep = "/")),silent = T)
}else{
  # If data has been directly downloaded from the NEON data portal and saved to DATAWS
  if(file.exists(paste0(Sys.getenv("DATAWS"),"NEON_discharge-rating-curves.zip"))|
     file.exists(paste0(Sys.getenv("DATAWS"),"NEON_discharge-rating-curves"))){
    # Stack the tables if they have not been already
    if(file.exists(paste0(Sys.getenv("DATAWS"),"NEON_discharge-rating-curves.zip"))){
      neonUtilities::stackByTable(paste0(Sys.getenv("DATAWS"),"NEON_discharge-rating-curves.zip"))
    }
    curveIdentification <- try(read.csv(paste(Sys.getenv("DATAWS"),"NEON_discharge-rating-curves","stackedFiles","geo_curveIdentification.csv", sep = "/")),silent = T)
  }else{
    # If the individual files are available in DATAWS
    availableFiles <- list.files(Sys.getenv("DATAWS"))
    curveIdentification  <- suppressWarnings(try(read.csv(paste(Sys.getenv("DATAWS"),availableFiles[grepl("geo_curveIdentification",availableFiles)], sep = "/")),silent = T))
  }
}

# Error handling if no discharge or curve identification data can be found
if(attr(curveIdentification, "class") == "try-error"){
  failureMessage <- paste0("Data could not be retrieved from geo/bat_curveIdentification_pub. Ensure the required input data are stored in ",Sys.getenv("DATAWS"))
  stop(failureMessage)
}

# Remove any curve IDs that shouldn't be included in this site or water year
curveIdentification <- curveIdentification[grepl(paste(Sys.getenv("SITE"),format(as.POSIXct(Sys.getenv("STARTDATE"))+31536000,"%Y"),sep = "."),curveIdentification$curveID),]
curveIdentification <- curveIdentification[order(curveIdentification$curveID),]
curveIDSegment <- unique(curveIdentification$curveID)[order(unique(curveIdentification$curveID))]
numCurves <- length(curveIDSegment)

for (i in 1:numCurves) {
  curveID <- curveIDSegment[i]

  # The inputs are set as environment variables rather than R variables to allow for running the Docker container for diffrent sites and dates without rebuilding it
  # To run the function, a user must have data downloaded from the expanded download package of the Stage-discharge rating curves (DP4.00133.001) data product. Data must be saved in the DATAWS file path
  stageQCurve::calc.stag.Q.curv(curveID=curveID)
  
  ### --- PLOT RATING CURVE PRIOR AND POSTERIOR PARAMETER DISTRIBUTIONS --- ###
  
  # From calc.stag.Q.curv outputs directly
  numCtrls <- nrow(read.table(paste0(DIRPATH,BAMWS,"Config_ControlMatrix.txt")))
  priorParams <- read.table(paste0(DIRPATH,BAMWS,"Config_Model.txt"),header = F)
  Results_MCMC_Cooked <- read.table(paste0(DIRPATH,BAMWS,"Results_MCMC_Cooked.txt"),header = T)
  
  # # From NEON OS transition system download -- FOR NEON INTERNAL USE ONLY
  # posteriorParameter <- read.table(paste0(DATAWS,"L1_Results_sdrc_posteriorParameters_pub.txt", header = T))
  # resultsResiduals <- read.table(paste0(DATAWS,"L1_Results_sdrc_resultsResiduals_pub.txt", header = T))
  # sampledParameters <- read.table(paste0(DATAWS,"L1_Results_sdrc_sampledParameters_pub.txt", header = T))
  # stageQCurve::txt.out.spag.data(spagDataIn=sampledParameters, spagOutPath=paste0(DIRPATH, BAMWS, "Results_MCMC_Cooked.txt"))
  # numCtrls <- nrow(posteriorParameter)
  # priorParams <- read.table(paste0(DIRPATH,BAMWS,"Config_Model.txt"),header = F)
  # Results_MCMC_Cooked <- read.table(paste0(DIRPATH,BAMWS,"Results_MCMC_Cooked.txt"),header = T)
  
  # Run the functions to plot rating curve prior and posterior parameter distributions
  stageQCurve::pre.post.parm.plot(curveID=curveID,
                                  numCtrls=numCtrls,
                                  priorParams=priorParams,
                                  Results_MCMC_Cooked=Results_MCMC_Cooked,
                                  NEONformat=F)
  stageQCurve::MCMC.sim.plot(numCtrls=numCtrls,
                             Results_MCMC_Cooked=Results_MCMC_Cooked,
                             NEONformat=F)
  
  ### --- RUN THE BaM RATING CURVE PREDICTION MODEL AND PLOT THE POSTERIOR RATING CURVE --- ###
  
  # From calc.stag.Q.curv outputs directly
  stageDischargeCurveInfo <- read.csv(paste0(DIRPATH,BAMWS,"stageDischargeCurveInfo_",curveID,".csv"),header = T)
  gaugeDischargeMeas <- read.csv(paste0(DIRPATH,BAMWS,"gaugeDischargeMeas_",curveID,".csv"),header = T)
  
  # # From NEON OS transition system download -- FOR NEON INTERNAL USE ONLY
  # stageDischargeCurveInfo <- read.table(paste0(DATAWS,"L1_Results_sdrc_stageDischargeCurveInfo_pub.txt", header = T))
  # gaugeDischargeMeas <- read.table(paste0(DATAWS,"L1_Results_sdrc_gaugeDischargeMeas_pub.txt", header = T))
  
  #Run BaM in prediction mode to get the information for the rating curve
  # minH <- 0
  # maxH <- 0.6
  minH <- stageDischargeCurveInfo$minStage - abs(stageDischargeCurveInfo$minStage)
  maxH <- stageDischargeCurveInfo$maxStage + stageDischargeCurveInfo$maxStage*0.3
  Hgrid <- stageQCurve::BaM.run.pred.RC(gaugingsData = gaugeDischargeMeas,
                                        minH = minH,
                                        maxH = maxH)
  
  #Plot and save figures of BaM rating curve prediction run
  stageQCurve::BaM.RC.out.plot(Hgrid=Hgrid)
  
  if (file.exists(paste0(Sys.getenv("DIRPATH"),Sys.getenv("BAMWS"),"data/Hgrid.txt"))) {
    file.remove(paste0(Sys.getenv("DIRPATH"),Sys.getenv("BAMWS"),"data/Hgrid.txt"))
  }
  if (file.exists(paste0(Sys.getenv("DIRPATH"),Sys.getenv("BAMWS"),"data/Gaugings.txt"))) {
    file.remove(paste0(Sys.getenv("DIRPATH"),Sys.getenv("BAMWS"),"data/Gaugings.txt"))
  }
  
}