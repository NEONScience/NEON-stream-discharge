##############################################################################################
#' @title Plot prior and posterior parameter distributions

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function plot prior and posterior parameters from BaM outputs or 
#' NEON data downloads

#' @importFrom graphics hist lines par
#' @importFrom stats dnorm
#' @importFrom grDevices dev.off

#' @param numCtrls Number of hydraulic controls [integer]
#' @param priorParams A dataframe containing the prior parameters [dataframe]
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
pre.post.parm.plot <- function(
  numCtrls,
  priorParams, 
  Results_MCMC_Cooked,
  NEONformat
  ){
  
  if(NEONformat==FALSE){
    priorParams <- frmt.pre.parm.file(dataFrame=priorParams,numCtrls=numCtrls)
  }else if(NEONformat==TRUE){
    #TBD
  }
  
  test.par <- par(mfrow=c(numCtrls,3))
  par(mar=c(2,2,1.5,1.5))
  ctrls <- c("k","a","c")
  for(i in 1:numCtrls){
    kMean <- as.numeric(priorParams$priorActivationStage[priorParams$controlNumber == i])
    kStd <- as.numeric(priorParams$priorActivationStageUnc[priorParams$controlNumber == i])
    kStartRange <- kMean - 3*abs(kStd)
    kEndRange <- kMean + 3*abs(kStd)
    kSeq <- seq(from = kStartRange, to = kEndRange, length.out = 30)
    kMCMC <- Results_MCMC_Cooked[,names(Results_MCMC_Cooked) == paste0("k",i)]
    kDnorm <- dnorm(kSeq,mean = kMean,sd = kStd)
    hist(kMCMC,breaks = 25,freq = FALSE, main = paste("k - Control", i), 
         xlab = "K", ylab = "Density", col = "red", 
         xlim = c(min(kStartRange,kMCMC),max(kEndRange,kMCMC)),
         ylim = c(0,max(kDnorm,hist(kMCMC, plot = F)$density)))
    lines(kSeq, kDnorm, col = "blue", lw = 1)
    
    aMean <- as.numeric(priorParams$priorCoefficient[priorParams$controlNumber == i])
    aStd <- as.numeric(priorParams$priorCoefficientUnc[priorParams$controlNumber == i])
    aStartRange <- aMean - 3*abs(aStd)
    aEndRange <- aMean + 3*abs(aStd)
    aSeq <- seq(from = aStartRange, to = aEndRange, length.out = 30)
    aMCMC <- Results_MCMC_Cooked[,names(Results_MCMC_Cooked) == paste0("a",i)]
    aDnorm <- dnorm(aSeq,mean = aMean,sd = aStd)
    hist(aMCMC,breaks = 25,freq = FALSE, main = paste("a - Control", i), 
         xlab = "a", ylab = "Density", col = "red", 
         xlim = c(min(aStartRange,aMCMC),max(aEndRange,aMCMC)),
         ylim = c(0,max(aDnorm,hist(aMCMC, plot = F)$density)))
    lines(aSeq, aDnorm, col = "blue", lw = 1)
    
    cMean <- as.numeric(priorParams$priorExponent[priorParams$controlNumber == i])
    cStd <- as.numeric(priorParams$priorExponentUnc[priorParams$controlNumber == i])
    cStartRange <- cMean - 3*abs(cStd)
    cEndRange <- cMean + 3*abs(cStd)
    cSeq <- seq(from = cStartRange, to = cEndRange, length.out = 30)
    cMCMC <- Results_MCMC_Cooked[,names(Results_MCMC_Cooked) == paste0("c",i)]
    cDnorm <- dnorm(cSeq,mean = cMean,sd = cStd)
    hist(cMCMC,breaks = 25,freq = FALSE, main = paste("c - Control", i), 
         xlab = "c", ylab = "Density", col = "red", 
         xlim = c(min(cStartRange,cMCMC),max(cEndRange,cMCMC)),
         ylim = c(0,max(cDnorm,hist(cMCMC, plot = F)$density)))
    lines(cSeq, cDnorm, col = "blue", lw = 1)
  }
  dev.print(pdf,'~/GitHub/NEON-stream-discharge/L4Discharge/BaM_beta/BaM_BaRatin/priorAndPostParams.pdf')
  dev.off()
}
