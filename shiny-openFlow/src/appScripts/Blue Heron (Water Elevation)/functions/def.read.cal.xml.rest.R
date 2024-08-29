##############################################################################################
#' @title Parses calibration files from REST calls that return XML

#' @author
#' Claire Lunch \email{clunch@battelleecology.org} \cr

#' @description This function reads in calibration files in XML format and returns the
#' same data in a dataframe format

#' @import XML

#' @param xmlCalFile List of XML data returned by GET call [list]

#' @return This function returns a data frame

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2017-12-07)
#     original creation
#   Zachary L. Nickerson (2021-01-18)
#     Added code to pull certificate numbers and add to output dataframe
##############################################################################################
def.read.cal.xml.rest <- function(
  xmlCalFile, session, input, output
){
  cal.pars <- XML::xmlParse(httr::content(xmlCalFile, as="text", encoding = "UTF-8"))
  certificateNumber.list <- XML::xmlApply(cal.pars["//calibration"],XML::xmlAttrs)
  cal.list <- XML::xmlApply(cal.pars["//calibration"],XML::xmlToDataFrame)

  #Loop through all of the calibration files that are returned
  df.names <- c("assetUid",
                "sensorStreamNum",
                "validStartTime",
                "validEndTime",
                "certificateFileName",
                "certificateNumber",
                "certificateMD5Sum",
                "objectUri",
                "objectDate",
                #names(cal.list[[1]])[2:5],
                "text",
                "name",
                "value",
                "type",
                "unitName")
  df.out <- data.frame(matrix(nrow = 0,ncol = length(df.names), data = NA))
  names(df.out) <- df.names
  
  
  for(i in seq(along=cal.list)){
    currList <- cal.list[[i]]
    currList$assetUid <- cal.list[[i]]$text[1]
    currList$sensorStreamNum <- cal.list[[i]]$text[2]
    currList$validStartTime <- cal.list[[i]]$text[3]
    currList$validEndTime <- cal.list[[i]]$text[4]
    currList$certificateFileName <- cal.list[[i]]$text[5]
    currList$certificateNumber <- as.numeric(certificateNumber.list[[i]][1])
    currList$certificateMD5Sum <- cal.list[[i]]$text[6]
    currList$objectUri <- cal.list[[i]]$text[7]
    currList$objectDate <- cal.list[[i]]$text[8]
    
    df.out <- merge(df.out,currList[!is.na(currList$name),], by = intersect(names(df.out),names(currList)), all = TRUE)
  }

  return(df.out)
}
