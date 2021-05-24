##############################################################################################
#' @title Plot Marcov Chain Monte Carlo Simulations

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function plots MCMC simulations from BaM outputs or
#' NEON data downloads

#' @importFrom graphics hist lines par
#' @importFrom stats dnorm
#' @importFrom grDevices dev.off dev.copy2pdf

#' @param DIRPATH An environment variable that contains the location of the files in
#' the Docker container [string]
#' @param BAMWS An environment variable that contains the location of the BaM config
#' files in the Docker container [string]
#' @param curveID Unique identifier of active stage-discharge rating curve [string]
#' @param numCtrls Number of hydraulic controls [integer]
#' @param Results_MCMC_Cooked A dataframe containing the posterior MCMC samples,
#' AKA cooked spaghettis [dataframe]
#' @param NEONformat An indicator of the format of the priorParams and Results_MCMC_Cooked
#' dataframes, for NEON downloads = TRUE, for data firectly form a BaM run = FALSE [boolean]

#' @return This function writes out .PDF files visualizing MCMC simulations

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-05-24)
#     include the curveID in each filename to be saved out
##############################################################################################
MCMC.sim.plot <- function(DIRPATH = Sys.getenv("DIRPATH"),
                          BAMWS = Sys.getenv("BAMWS"),
                          curveID,
                          numCtrls,
                          Results_MCMC_Cooked,
                          NEONformat=F){

  if(NEONformat==FALSE){
    priorParams <- frmt.pre.parm.file(dataFrame=priorParams,numCtrls=numCtrls)
  }else if(NEONformat==TRUE){
    #TBD
  }

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
  dev.copy2pdf(file = paste0(DIRPATH,BAMWS,"MCMC_simulations_",curveID,".pdf"), width = 16, height = 9)
  dev.off()
}
