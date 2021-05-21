##############################################################################################
#' @title Plot rating curve outputs

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function plots and saves figures from the BaM rating curve prediction
#' outputs

#' @importFrom graphics plot lines polygon points
#' @importFrom grDevices dev.copy2pdf dev.off adjustcolor

#' @param DIRPATH An environment variable that contains the location of the files in
#' the Docker container [string]
#' @param BAMWS An environment variable that contains the location of the BaM config
#' files in the Docker container [string]
#' @param curveID Unique identifier of active stage-discharge rating curve [string]
#' @param Hgrid A numeric array of the stage values overwhich the rating curve was evaluated
#' and will be plotted [numeric]

#' @return This function writes out configurations and runs BaM in prediction mode

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-04-06)
#     code updates to no plot the prior rating curve due to impossible spaghettis
##############################################################################################
BaM.RC.out.plot <- function(DIRPATH = Sys.getenv("DIRPATH"),
                            BAMWS = Sys.getenv("BAMWS"),
                            curveID,
                            Hgrid){

  #Read in output data of the rating curve MCMC predictions
  # Qrc_Prior_spag <- read.table(paste0(DIRPATH, BAMWS, "Qrc_Prior.spag"), header = F)
  # Qrc_Prior_env <- read.table(paste0(DIRPATH, BAMWS, "Qrc_Prior.env"), header = T)

  Qrc_Maxpost_spag <- read.table(paste0(DIRPATH, BAMWS, "Qrc_Maxpost.spag"), header = F)

  Qrc_ParamU_spag <- read.table(paste0(DIRPATH, BAMWS, "Qrc_ParamU.spag"), header = F)
  Qrc_ParamU_env <- read.table(paste0(DIRPATH, BAMWS, "Qrc_ParamU.env"), header = T)

  Qrc_TotalU_spag <- read.table(paste0(DIRPATH, BAMWS, "Qrc_TotalU.spag"), header = F)
  Qrc_TotalU_env <- read.table(paste0(DIRPATH, BAMWS, "Qrc_TotalU.env"), header = T)

  gaugings <- read.table(paste0(DIRPATH, BAMWS, "data/Gaugings.txt"), header = T)
  gaugings$Q <- as.numeric(gaugings$Q)/1000 #Convert to cms from lps
  gaugings$uQ <- as.numeric(gaugings$uQ)/1000 #Convert to cms from lps

  #Plot rating curve
  pramUForPlottingTop <- cbind.data.frame(Hgrid,Qrc_ParamU_env$Q_q2.5)
  pramUForPlottingBottom <- cbind.data.frame(Hgrid,Qrc_ParamU_env$Q_q97.5)
  names(pramUForPlottingTop) <- c("Hgrid","Q")
  names(pramUForPlottingBottom) <- c("Hgrid","Q")
  pramUForPlotting <- rbind(pramUForPlottingTop,pramUForPlottingBottom[dim(pramUForPlottingBottom)[1]:1,])

  totalUTop <- cbind.data.frame(Hgrid,Qrc_TotalU_env$Q_q2.5)
  totalUBottom <- cbind.data.frame(Hgrid,Qrc_TotalU_env$Q_q97.5)
  names(totalUTop) <- c("Hgrid","Q")
  names(totalUBottom) <- c("Hgrid","Q")
  totalUForPlotting <- rbind(totalUTop,totalUBottom[dim(totalUBottom)[1]:1,])

  # priorTop <- cbind.data.frame(Hgrid, Qrc_Prior_env$Q_q2.5)
  # priorBottom <- cbind.data.frame(Hgrid, Qrc_Prior_env$Q_q97.5)
  # names(priorTop) <- c("Hgrid","Q")
  # names(priorBottom) <- c("Hgrid","Q")
  # priorForPlotting <- rbind(priorTop,priorBottom[dim(priorBottom)[1]:1,])

  #Set back to default plotting margins
  test.par <- par(mfrow=c(1,1))
  par(mar=c(5.1,4.1,4.1,2.1))
  plot(Hgrid,
       Qrc_Maxpost_spag$V1,
       type = "l",
       xlim = c(min(Hgrid),max(Hgrid)),
       ylim = c(min(totalUForPlotting$Q),max(totalUForPlotting$Q)),
       xlab = "Stage (m)",
       ylab = "Discharge (cms)")

  #Work from background to foreground
  polygon(totalUForPlotting$Hgrid,totalUForPlotting$Q, col = "red", border = NA)
  # polygon(priorForPlotting$Hgrid, priorForPlotting$Q, col = "royalblue1", border = NA)
  polygon(pramUForPlotting$Hgrid,pramUForPlotting$Q, col = adjustcolor("lightpink",alpha.f = 0.7), border = NA)

  # lines(Hgrid,Qrc_Prior_env$Q_Median, col = "blue", lwd = 2)
  lines(Hgrid,Qrc_Maxpost_spag$V1, col = "black", lwd = 2)

  dev.copy2pdf(file = paste0(DIRPATH,BAMWS,"postRatingCurveWUnc_linearScale_",curveID,".pdf"), width = 16, height = 9)
  dev.off()

  #Plot rating curve with gaugings
  plot(Hgrid,
       Qrc_Maxpost_spag$V1,
       type = "l",
       xlim = c(min(Hgrid),max(Hgrid)),
       ylim = c(min(pramUForPlotting$Q,gaugings$H),max(pramUForPlotting$Q,gaugings$H)),
       xlab = "Stage (m)",
       ylab = "Discharge (cms)")
  polygon(pramUForPlotting$Hgrid,pramUForPlotting$Q, col = "lightpink", border = NA)
  lines(Hgrid,Qrc_Maxpost_spag$V1, col = "black", lwd = 2)
  points(gaugings$H,gaugings$Q, pch = 19)

  dev.copy2pdf(file = paste0(DIRPATH,BAMWS,"ratingCurveWithGaugings_linearScale_",curveID,".pdf"), width = 16, height = 9)
  dev.off()

  #Plot log-scale
  test.par <- par(mfrow=c(1,1))
  par(mar=c(5.1,4.1,4.1,2.1))
  Qrc_Maxpost_spag$V1[Qrc_Maxpost_spag$V1 <= 0] <- 0.000000001
  plot(Hgrid,
       Qrc_Maxpost_spag$V1,
       type = "l",
       log = "xy",
       xlim = c(0.008,max(Hgrid)),
       ylim = c(0.0005,max(totalUForPlotting$Q)),
       xlab = "Stage (m)",
       ylab = "Discharge (cms)")

  #Work from background to foreground
  totalUForPlotting$Qlog <- totalUForPlotting$Q
  totalUForPlotting$Qlog[totalUForPlotting$Q<=0] <- 0.000001
  polygon(totalUForPlotting$Hgrid,totalUForPlotting$Qlog, col = "red", border = NA)
  # polygon(priorForPlotting$Hgrid, priorForPlotting$Q, col = "royalblue1", border = NA)
  polygon(pramUForPlotting$Hgrid,pramUForPlotting$Q, col = adjustcolor("lightpink",alpha.f = 0.7), border = NA)

  # lines(Hgrid,Qrc_Prior_env$Q_Median, col = "blue", lwd = 2)
  lines(Hgrid,Qrc_Maxpost_spag$V1, col = "black", lwd = 2)

  dev.copy2pdf(file = paste0(DIRPATH,BAMWS,"postRatingCurveWUnc_logScale_",curveID,".pdf"), width = 16, height = 9)
  dev.off()

  #Plot rating curve with gaugings in log scale
  plot(Hgrid,
       Qrc_Maxpost_spag$V1,
       type = "l",
       log = "xy",
       xlim = c(0.008,max(Hgrid)),
       ylim = c(0.0005,max(pramUForPlotting$Q)),
       xlab = "Stage (m)",
       ylab = "Discharge (cms)")
  polygon(pramUForPlotting$Hgrid,pramUForPlotting$Q, col = "lightpink", border = NA)
  lines(Hgrid,Qrc_Maxpost_spag$V1, col = "black", lwd = 2)
  points(gaugings$H,gaugings$Q, pch = 19)

  dev.copy2pdf(file = paste0(DIRPATH,BAMWS,"ratingCurveWithGaugings_logScale_",curveID,".pdf"), width = 16, height = 9)
  dev.off()

  #Plot spaghettis
  #Total Uncertainty quite large
  plot(Hgrid,
       Qrc_TotalU_spag[,1],
       type = "l",
       col = "red",
       ylim = c(min(Qrc_TotalU_spag),max(Qrc_TotalU_spag)),
       ylab = "Spaghettis with parametric uncertainty")
  for(i in 2:ncol(Qrc_TotalU_spag)){
    lines(Hgrid,Qrc_TotalU_spag[,i], col = "red")
  }
  dev.copy2pdf(file = paste0(DIRPATH,BAMWS,"spaghettisWithTotalU_",curveID,".pdf"), width = 16, height = 9)
  dev.off()

  plot(Hgrid,
       Qrc_ParamU_spag[,1],
       type = "l",
       col = "lightpink",
       ylim = c(min(Qrc_ParamU_spag),max(Qrc_ParamU_spag)),
       ylab = "Spaghettis with parametric uncertainty")
  for(i in 2:ncol(Qrc_ParamU_spag)){
    lines(Hgrid,Qrc_ParamU_spag[,i], col = "lightpink")
  }
  dev.copy2pdf(file = paste0(DIRPATH,BAMWS,"spaghettisWithParamU_",curveID,".pdf"), width = 16, height = 9)
  dev.off()

  # plot(Hgrid,
  #      Qrc_Prior_spag[,1],
  #      type = "l",
  #      col = "royalblue1",
  #      ylim = c(min(Qrc_Prior_spag),max(Qrc_Prior_spag)),
  #      ylab = "Spaghettis with parametric uncertainty")
  # for(i in 2:ncol(Qrc_Prior_spag)){
  #   lines(Hgrid,Qrc_Prior_spag[,i], col = "royalblue1")
  # }
  # dev.copy2pdf(file = paste(DIRPATH,BAMWS,"priorSpaghettis.pdf",sep = "/"), width = 16, height = 9)
  # dev.off()

}
