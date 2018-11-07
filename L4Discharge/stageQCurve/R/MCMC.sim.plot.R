##############################################################################################
#' @title Plot prior and posterior parameter distributions

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function plot prior and posterior parameters from BaM outputs or 
#' NEON data downloads

#' @importFrom graphics hist lines par
#' @importFrom stats dnorm
#' @importFrom grDevices dev.off dev.copy2pdf

#' @param DIRPATH An environment variable that contains the location of the files in 
#' the Docker container [string]
#' @param BAMWS An environment variable that contains the location of the BaM config 
#' files in the Docker container [string]
#' @param numCtrls Number of hydraulic controls [integer]
#' @param Results_MCMC_Cooked A dataframe containing the posterior MCMC samples, 
#' AKA cooked spaghettis [dataframe]
#' @param NEONformat An indicator of the format of the priorParams and Results_MCMC_Cooked 
#' dataframes, for NEON downloads = TRUE, for data firectly form a BaM run = FALSE [boolean]

#' @return This function read in and writes out a modified version of the RunOptions for BaM

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
##############################################################################################
MCMC.sim.plot <- function(
  DIRPATH = Sys.getenv("DIRPATH"),
  BAMWS = Sys.getenv("BAMWS"),
  numCtrls,
  Results_MCMC_Cooked,
  NEONformat
  ){
  
  # if(NEONformat==FALSE){
  #   priorParams <- frmt.pre.parm.file(dataFrame=priorParams,numCtrls=numCtrls)
  # }else if(NEONformat==TRUE){
  #   #TBD
  # }
  
  test.par <- par(mfrow=c(numCtrls,3))
  par(mar=c(2,2,1.5,1.5))
  ctrls <- c("k","a","c")
  for(i in 1:numCtrls){
    kMCMC <- Results_MCMC_Cooked[,names(Results_MCMC_Cooked) == paste0("k",i)]
    plot(kMCMC,
         type = "l",
         main = paste("k - Control", i), 
         xlab = "Iteration", 
         ylab = "K",
         ylim = c(min(kMCMC),max(kMCMC)))
    
    aMCMC <- Results_MCMC_Cooked[,names(Results_MCMC_Cooked) == paste0("a",i)]
    plot(aMCMC,
         type = "l",
         main = paste("a - Control", i), 
         xlab = "Iteration", 
         ylab = "A",
         ylim = c(min(aMCMC),max(aMCMC)))
    
    cMCMC <- Results_MCMC_Cooked[,names(Results_MCMC_Cooked) == paste0("c",i)]
    plot(cMCMC,
         type = "l",
         main = paste("c - Control", i), 
         xlab = "Iteration", 
         ylab = "C",
         ylim = c(min(cMCMC),max(cMCMC)))
  }
  dev.copy2pdf(file = paste(DIRPATH,BAMWS,"MCMC_simulations.pdf",sep = "/"), width = 16, height = 9)
  dev.off()
}
