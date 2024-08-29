queryDischargeData <- function(OSURL = "http://prod-os-ds-1.ci.neoninternal.org:8080/osDataService/", startDate = NULL, endDate = NULL, DPID = "NEON.DOM.SITE.DP4.00133.001", osTable = "sdrc_gaugePressureRelationship_pub", namedLocation = "LECO")
{
  # get the matching pub workbook
  sdfc.pub <- httr::GET(paste0(OSURL,
                               "pub-tables/",
                               DPID,
                               ":",
                               osTable),
                        httr::accept("application/vnd.neoninc.os.pub-table-v1.0+xml"))
  if(sdfc.pub$status_code != 200){
    stop(paste0("Pub WB could not be retrieved for ", DPID, ":", osTable))
  }else{
    print(paste0("Pub WB retrieved for ", DPID, ":", osTable))
  }
  sdfc.pub <- XML::xmlToDataFrame(httr::content(sdfc.pub, as="text", encoding = "UTF-8"))
  sdfc.pub <- sdfc.pub[which(!is.na(sdfc.pub$rank)),]
  
  # get data matching the dates on the transition object
  if(is.null(startDate) || is.null(endDate)){
    print("startDate and/or endDate is null, searching for data from 2010-01-01 to now")
    startDate <- "2010-01-01"
    endDate <- Sys.Date()
  }

  sdfc.req <- httr::GET(paste0(OSURL,
                               "results?pub-table-key=",
                               DPID,
                               ":",
                               osTable,
                               "&named-location-name=",namedLocation,
                               "&end-date-begin=", startDate,
                               "Z&end-date-cutoff=", endDate,
                               "Z&include-result-data=true&include-samples=true"),
                        httr::accept("application/vnd.neoninc.os.result-list-v1.0+xml"))
  
  if(sdfc.req$status_code != 200){
    stop(paste0("OS data could not be retrieved for ", DPID, ":", osTable))
  }else{
    print(paste0("OS data retrieved for ", DPID, ":", osTable))
  }
  
  # parse the discharge field data
  dsc.pars <- parseOSData(x = sdfc.req, wb = sdfc.pub)
  if(length(dsc.pars)>0){
    print(paste0('Successfully parsed ', length(dsc.pars[,1]), " records from ", osTable))
  }else{
    print(paste0("No records parsed from ", osTable))
  }
  return(dsc.pars)
  
}
