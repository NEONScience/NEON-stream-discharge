##############################################################################################
#' @title SUpport Function for parsing data from REST calls that return XML

#' @author 
#' Claire Lunch \email{clunch@battelleecology.org} \cr

#' @description This function flattens and converts data to a dataframe

#' @param x List of data converted from XML [list]
#' @param tb Dataframe containing rank with row names equal to the WB terms [dataframe]

#' @return This function returns a list parsed into a dataframe according to workbook fields

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2017-12-07)
#     original creation
#   Kaelin Cawley (2020-08-05)
#     added dateValue option for dates in DP table
#   Kaelin Cawley (2020-08-11)
#     added null test to all value options
##############################################################################################
make.flat <- function(x, tb) {
  x$fieldName[1:6] <- c("named_location","DPID","table","startDate","endDate","TID")
  
  if(!is.null(x$numberValue)){
    x$stringValue[which(is.na(x$stringValue))] <- x$numberValue[which(is.na(x$stringValue))]
  }
  
  if(!is.null(x$text)){
    x$stringValue[which(is.na(x$stringValue))] <- x$text[which(is.na(x$stringValue))]
  }
  
  if(!is.null(x$dateValue)){
    x$stringValue[which(is.na(x$stringValue))] <- x$dateValue[which(is.na(x$stringValue))]
  }
  
  if(length(which(is.na(x$fieldName)))>0) {
    x <- x[-which(is.na(x$fieldName)),]
  }
  
  y <- data.frame(x$stringValue, row.names=x$fieldName)
  y <- merge(tb, y, all=T, by="row.names")
  return(y[order(y[,grep("rank", colnames(y))]),c("Row.names","x.stringValue")])
}

