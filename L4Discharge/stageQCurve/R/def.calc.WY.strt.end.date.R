##############################################################################################
#' @title Find water year start and end date for an input date

#' @author 
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This function returns the start and end dates of the water year for the 
#' input searchIntervalStartDate

#' @param searchIntervalStartDate A date in POSIXct format [POSIXct]

#' @return List of start and end dates of the water year for the input searchIntervalStartDate

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2017-12-07)
#     original creation
##############################################################################################
def.calc.WY.strt.end.date <- function(searchIntervalStartDate){
  Oct <- "10"
  Sept <- "09"
  first <- "01"
  thirtieth <- "30"
  startYear <- substr(searchIntervalStartDate, 1, 4)
  diffOctFirst <- difftime(searchIntervalStartDate, 
                           as.POSIXct(paste(startYear, Oct, first, sep = "-"), tz = "UTC"), 
                           units = "days")
  if(diffOctFirst > 0){
    octWY <- as.POSIXct(paste(startYear, Oct, first, sep = "-"), tz = "UTC")
  }else if(diffOctFirst < 0){
    octWY <- as.POSIXct(paste((as.numeric(startYear)-1), Oct, first, sep = "-"), tz = "UTC")
  }else{
    octWY <- searchIntervalStartDate
  }
  
  octWY <- as.numeric(substr(octWY, 1, 4))
  startDate <- as.POSIXct(paste(octWY, Oct, first, sep = "-"), tz = "UTC")
  endDate <- as.POSIXct(paste((octWY+1), Sept, thirtieth, sep = "-"), tz = "UTC")
  return(list("startDate"=startDate,"endDate"=endDate))
}
