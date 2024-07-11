processWaterData <- function(input, output, session, DPID, startDate = NULL, endDate = NULL)
{
  stack = "prod"
  if(is.null(startDate) || is.null(endDate))
  {
    startDate = paste0("20",format(Sys.time()-86400, "%y-%m-%dT%H:%M:%S.000Z"))
    endDate = paste0("20",format(Sys.time(), "%y-%m-%dT%H:%M:%S.000Z"))
  } else {
    startDate = paste0("20",format(startDate, "%y-%m-%dT00:00:00.000Z"))
    endDate = paste0("20",format(endDate, "%y-%m-%dT00:00:00.000Z"))
  }
  meas = DPID
  
  # construct the call to get the data 
  data.call <- paste0("http://den-",stack,"cdsllb-1.ci.neoninternal.org/cdsWebApp/measurement-readouts",
                      "?start-date-begin=", startDate,
                      "&start-date-cutoff=", endDate)
  
  # loop through all of th efull DPIDs
  for( i in meas)
  {
    if(!grepl("NEON\\.D[01][0-9]\\.[A-Z]{4}\\.DP[01234]\\.[0-9]{5}\\.[0-9]{3}\\.[0-9]{5}\\.[0-9]{3}\\.[0-9]{3}\\.[0-9]{3}",i))
    {
      stop(paste("Format of full DPID is invalid for:", i))
    } else {
      data.call <- paste0(data.call, "&meas-strm-name=",i)
    }
  }
  
  # make the GET call
  req <- httr::GET(data.call, 
                   httr::accept("application/vnd.neoninc.cds.measurement-group-v1.0+xml"),
                   httr::progress())
  
  # check status code
  if(req$status_code != 200)
  {
    stop(paste("Data GET failed with status code ", req$status_code, ". Check formatting of your inputs.", sep = ""))
  }
  
  #dataOut <- list()
  #idx <- 1
  for(i in meas)
  {
    xXML <- try(XML::xmlParse(httr::content(req, as="text", encoding = "UTF-8"))[[paste0('//readoutList[measurementStream/@name=\"', i, '\"]')]])
    if(all(attr(xXML, "class") == "try-error"))
    {
      cat("No data returned for:", i, "\n")
      next
    } else {
      dateFrom <- XML::xmlToDataFrame(xXML, stringsAsFactors = F, homogeneous = F)
      dataOut <- dateFrom[!is.na(dateFrom$startDate), colSums(!is.na(dateFrom))>2]
      #dataOut[[idx]] <- dateFromTrim
      #names(dataOut)[idx] <- i
      #idx <- idx+1
    }
  }
  #remove(xXML, dateFrom, dateFromTrim)
  return(dataOut)
}
