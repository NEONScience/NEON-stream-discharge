##############################################################################################
#' @title Writes out a text file with RunOptions for BaM

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function writes out the RunOptions for BaM.

#' @param runType Either "param" or "pred" to determine which RunOptions to enable [string]
#' @param RunOptionsPath A filepath for the RunOptions text file [string]

#' @return This function read in and writes out a modified version of the RunOptions for BaM

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
##############################################################################################
txt.out.run.opts <- function(runType, RunOptionsPath){
  RunOptions <- readLines(RunOptionsPath)
  switch(runType,
         param = {
           #Write RunOptions files for parameters
           RunOptions[1] <- gsub("\\.false\\.",".true.",RunOptions[1]) #Make sure it's true MCMC
           RunOptions[2] <- gsub("\\.false\\.",".true.",RunOptions[2]) #Make sure it's true MCMC Summary
           RunOptions[3] <- gsub("\\.false\\.",".true.",RunOptions[3]) #Make sure it's true Residual Diagnostics
           RunOptions[4] <- gsub("\\.true\\.",".false.",RunOptions[4]) #Make sure it's false Predictions
         },
         pred ={
           #Write RunOptions files for predictions
           RunOptions[1] <- gsub("\\.true\\.",".false.",RunOptions[1]) #Make sure it's false MCMC
           RunOptions[2] <- gsub("\\.true\\.",".false.",RunOptions[2]) #Make sure it's false MCMC Summary
           RunOptions[3] <- gsub("\\.true\\.",".false.",RunOptions[3]) #Make sure it's false Residual Diagnostics
           RunOptions[4] <- gsub("\\.false\\.",".true.",RunOptions[4]) #Make sure it's true Predictions
         })
  writeLines(RunOptions, RunOptionsPath) #Specifies the types of runs to perform
}
