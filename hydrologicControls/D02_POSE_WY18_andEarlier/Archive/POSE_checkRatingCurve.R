
#This script runs the bayesian model and plots results for QAQC prior to loading control data to PROD

Sys.setenv(DIRPATH = "C:/Users/kcawley/Documents/GitHub/NEON-stream-discharge/hydrologicControls/D02_POSE_WY18_andEarlier/",
           BAMFOLD="BaM_beta/",
           BAMFILE="BaM_MiniDMSL.exe",#Windows version
           #BAMFILE="BaM_exe",#Linux version
           DATAWS="H:/controlTesting/",
           BAMWS="BaM_beta/BaM_BaRatin/",
           STARTDATE = "2016-10-01",
           SITE = "POSE")

DIRPATH = Sys.getenv("DIRPATH")
BAMFOLD = Sys.getenv("BAMFOLD")
BAMFILE = Sys.getenv("BAMFILE")
DATAWS = Sys.getenv("DATAWS")
BAMWS = Sys.getenv("BAMWS")
startDate = Sys.getenv("STARTDATE")
site = Sys.getenv("SITE")

# #Need to run this periodically if you're running the code outside of the Docker container
# #as NEON packages get updated
# library(devtools)
# install_github("NEONScience/NEON-utilities/neonUtilities", force = TRUE, dependencies = TRUE)
# install_github("NEONScience/NEON-stream-discharge/L4Discharge/stageQCurve", force = TRUE, dependencies = TRUE)

options(stringsAsFactors = FALSE,scipen = 999)
Sys.setenv(TZ='UTC')
library(neonUtilities)
library(stageQCurve)

#This step will likely download data from the portal and you will see text in the r console and a pop-up of download progrssion
calc.stag.Q.curv()

#Plot rating curve prior and posterior parameter distributions from MCMC outputs directly
#Check model outputs and fit
numCtrls <- nrow(read.table(paste(DIRPATH,BAMWS,"Config_ControlMatrix.txt",sep = "/")))
priorParams <- read.table(paste(DIRPATH,BAMWS,"Config_Model.txt",sep = "/"))
Results_MCMC_Cooked <- read.table(paste(DIRPATH,BAMWS,"Results_MCMC_Cooked.txt",sep = "/"),header = T)
pre.post.parm.plot(numCtrls=numCtrls,
                   priorParams=priorParams,
                   Results_MCMC_Cooked=Results_MCMC_Cooked,
                   NEONformat=FALSE)
MCMC.sim.plot(numCtrls=numCtrls,Results_MCMC_Cooked=Results_MCMC_Cooked,NEONformat=FALSE)

#Read in results to plot the actual rating curve
#posteriorParameter <- read.table("~/GitHub/NEON-stream-discharge/L4Discharge/data/L1_Results_sdrc_posteriorParameters_pub.txt", header = T)
#resultsResiduals <- read.table("~/GitHub/NEON-stream-discharge/L4Discharge/data/L1_Results_sdrc_resultsResiduals_pub.txt", header = T)
#sampledParameters <- read.table(paste(DIRPATH,BAMWS,"sampledParameters.csv",sep = "/"), sep = ",",header = T)
#txt.out.spag.data(spagDataIn=sampledParameters, spagOutPath=paste0(DIRPATH, BAMWS, "Results_MCMC_Cooked.txt"))

#Run the results through a range of stage values
stageDischargeCurveInfo <- read.table(paste(DIRPATH,BAMWS,"stageDischargeCurveInfo.csv",sep = "/"), sep = ",",header = T)
minH <- stageDischargeCurveInfo$minStage - abs(stageDischargeCurveInfo$minStage)
#For the log-scale plotting later make sure that nothing is less than or equal to 0
if(minH <= 0){
  minH <- 0.00001
}
maxH <- stageDischargeCurveInfo$maxStage + stageDischargeCurveInfo$maxStage*0.3

#Run BaM in prediction mode to get the information for the rating curve
#Hgrid <- BaM.run.pred.RC(minH = -0.05,maxH = 0.4)
Hgrid <- BaM.run.pred.RC(minH = minH,maxH = maxH)

#Plot and save figures of BaM rating curve prediction run
BaM.RC.out.plot(Hgrid=Hgrid)













