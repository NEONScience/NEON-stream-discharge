L0DataQuery <- function(input, output, session, DPID, startDate = NULL, endDate = NULL)
{
  stack = "prod"
  if(is.null(startDate) || is.null(endDate))
  {
    startDate = paste0("20",format(Sys.time()-86400, "%y-%m-%dT%H:%M:%S.000Z"))
    endDate = paste0("20",format(Sys.time(), "%y-%m-%dT%H:%M:%S.000Z"))
  } else {
    startDate = paste0("20",format(startDate, "%y-%m-%dT%H:%M:%S.000Z"))
    endDate = paste0("20",format(endDate, "%y-%m-%dT%H:%M:%S.000Z"))
  }
  meas = DPID
  # startDate = "2024-07-29"
  # endDate = "2024-07-30"
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
  
  if(input$dataSource == "L0 Data Query")
  {
    for(i in meas)
    {
      xXML <- try(XML::xmlParse(httr::content(req, as="text", encoding = "UTF-8"))[[paste0('//readoutList[measurementStream/@name=\"', i, '\"]')]])
      locID <- XML::xpathSApply(xXML, "//namedLocation/@id")
      locName <- XML::xpathSApply(xXML, "//namedLocation/@name")
      locationInfo <- list(id = locID, name = locName)
    }
    dataOut <- list(namedLocation = locationInfo, L0Data = xXML)
  } else {
    for(i in meas)
    {
      locID <- XML::xpathSApply(try(XML::xmlParse(httr::content(req, as="text", encoding = "UTF-8"))[[paste0('//readoutList[measurementStream/@name=\"', i, '\"]')]]), "//namedLocation/@name")
    }
    dataOut <- locID
  }
  
  return(dataOut)
}
