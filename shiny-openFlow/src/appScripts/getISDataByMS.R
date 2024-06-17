##############################################################################################
#' @title SCRIPT FOR PULLING IS DATA FROM CDS WEB APP

#' @author
#' Zachary Nickerson \email{nickerson@battelleecology.org} \cr

#' @description compiled from restR2 functions get.is.ms.multi and def.read.is.rest.multi.
#' See the helper files for those functions for more details.

#' @return XML converted to a data frame

# changelog and author contributions / copyrights
#   Zachary Nickerson (2024-06-17)
#     original compilation of restR2 functions (credit to Claire and Kaelin for original creation)
##############################################################################################


stack="prod"
startDate="2024-06-15T00:00:00.000Z"
endDate="2024-06-16T00:00:00.000Z"
meas=c("NEON.D01.HOPB.DP0.20016.001.01379.131.100.000")

# construct the call to get the data
data.call <- paste0("http://den-",stack,"cdsllb-1.ci.neoninternal.org/cdsWebApp/measurement-readouts",
                    "?start-date-begin=",startDate,
                    "&start-date-cutoff=", endDate)

#now loop through all of the full DPIDs
for(i in meas){
  #check that the format of the full DPID is valid
  if(!grepl("NEON\\.D[01][0-9]\\.[A-Z]{4}\\.DP[01234]\\.[0-9]{5}\\.[0-9]{3}\\.[0-9]{5}\\.[0-9]{3}\\.[0-9]{3}\\.[0-9]{3}",i)){
    stop(paste("Format of full DPID is invalid for:",i))
  }else{
    data.call <- paste0(data.call,"&meas-strm-name=",i)
  }
}

# make the GET call
req <- httr::GET(data.call,
                 httr::accept("application/vnd.neoninc.cds.measurement-group-v1.0+xml"),
                 httr::progress())

# check status code
if(req$status_code!=200) {
  stop(paste("Data GET failed with status code ", req$status_code, 
             ". Check the formatting of your inputs.", sep=""))
}

dataOut <- list()
idx <- 1
for(i in meas){
  
  #cat(i,"\n")
  xXML <- try(XML::xmlParse(httr::content(req, as="text", encoding = "UTF-8"))[[paste0('//readoutList[measurementStream/@name=\"',i,'\"]')]])
  if(all(attr(xXML, "class") == "try-error")){
    cat("No data returned for:",i,"\n")
    next
  }else{
    dateFrm <- XML::xmlToDataFrame(xXML, stringsAsFactors=F,homogeneous = FALSE)
    dateFrmTrim <- dateFrm[!is.na(dateFrm$startDate),colSums(!is.na(dateFrm))>2]
    dataOut[[idx]] <- dateFrmTrim
    names(dataOut)[idx] <- i
    idx <- idx+1
  }
  remove(xXML,dateFrm,dateFrmTrim)
}
