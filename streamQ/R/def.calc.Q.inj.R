##############################################################################################
#' @title Salt-based discharge calculations for a constant rate injection

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function calculates discharge from a constant rate salt tracer injection.

#' @param inputFile Name of the data fram containing the information needed to calculate stream discharge from a constant-rate injection. If the headers are named: "injRate", "injConc", "backgroundConc", and "plateauConc", no other inputs are required. Otherwise, the names of the columns need to be input for the function to work.
#' @param injRate Flow rate of tracer injection [mL/min]
#' @param injConc Concentration of the tracer injectate [mg/L]
#' @param backgroundConc Background concentration of the tracer in the stream [mg/L]
#' @param plateauConc Plateau concentration of the tracer in the stream[mg/L]

#' @return This function returns stream discharge "Q.inj" [lps] appended as an additional column to the inputFile with flags set to 1 when the background corrected injectate concentration and/or plateau concentration are less than 0.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords surface water, streams, rivers, discharge, tracer, salt-based, constant-rate injection

#' @examples
#' sbdDataPlusInjQ <- def.calc.Q.inj(inputFile = sbdFormatted)

#' @seealso def.calc.Q.slug.R for calculating stream discharge from a slug injection

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-08-03)
#     original creation
##############################################################################################
#This code is for calculating salt-based discharge from a constant-rate injection
def.calc.Q.inj <- function(
  inputFile,
  injRate = inputFile$injRate,
  injConc = inputFile$injConc,
  backgroundConc = inputFile$backgroundConc,
  plateauConc = inputFile$plateauConc
) {
  
  ##### Constants #####
  #Conversion factor between mL/min and lps
  rateConv <- 1/60 * 1/1000
  
  #### Calculations #####
  #Convert injRate to lps
  injRateLPS <- injRate * rateConv
  
  #Calculate discharge with a little error handling for negative background subtractions
  injCorr <- injConc - backgroundConc
  plaCorr <- plateauConc - backgroundConc
  
  inputFile$Q.inj <- (injCorr)/(plaCorr) * injRateLPS
  
  #Clean and flag negative values
  inputFile$Q.inj[inputFile$Q.inj < 0] <- NA
  inputFile$lowInjectateQF <- 0
  inputFile$lowPlateauQF <- 0
  inputFile$lowInjectateQF[injCorr < 0] <- 1
  inputFile$lowPlateauQF[plaCorr < 0] <- 1
  
  #Round to significant figures
  inputFile$Q.inj <- signif(inputFile$Q.inj, digits = 2)
  
  return(inputFile)
  
}