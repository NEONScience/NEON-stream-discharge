##############################################################################################
#' @title Stage-Discharge Rating Curve Docker Script

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This script uses the BaM executable to calculate the parameters that define 
#' the stage-discharge relationship for a site and water year using NEON data.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
##############################################################################################

# #For running the code outside of a Docker container
# #The paths can get pretty long (>260 characters),
# #which can cause trouble with the data stacker in windows, choose wisely.
# 
# #User inputs for site and date
# #If you enter a startData that is not YYY-10-01, the script will determine the
# #water year that started immediately before the date entered here and use it as the
# #startDate
# 
# Sys.setenv(DIRPATH = "C:/Users/kcawley/Documents/GitHub/NEON-stream-discharge/L4Discharge/",
#            BAMFOLD="BaM_beta/",
#            BAMFILE="BaM_MiniDMSL.exe",#Windows version
#            #BAMFILE="BaM_exe",#Linux version
#            DATAWS="C:/Users/kcawley/Desktop/data/",
#            BAMWS="BaM_beta/BaM_BaRatin/",
#            STARTDATE = "2016-10-01",
#            SITE = "HOPB")
# 
# #Need to run this periodically if you're running the code outside of the Docker container
# #as NEON packages get updated
# library(devtools)
# install_github("NEONScience/NEON-utilities/neonDataStackR", force = TRUE, dependencies = TRUE)
# install_github("NEONScience/NEON-stream-discharge/L4Discharge/stageQCurve", force = TRUE, dependencies = TRUE)

# #Load needed library for Docker testing prior to GitHub package release
# setwd("/app/L4_discharge/")
# devtools::install("stageQCurve")
# library(stageQCurve)

#Environment options and configurations
options(stringsAsFactors = F)
Sys.setenv(TZ='UTC')
library(neonDataStackR)
library(stageQCurve)

#Run main function
#The inputs are set as environment variables rather than R variables to make running the Docker container easier
calc.stag.Q.curv()


