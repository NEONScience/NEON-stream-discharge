grabCalibrations <- function(
  CDSURL,
  startDate,
  endDate,
  DPID,
  session,
  input,
  output
){
  
  #Need to get asset-stream-key from DPID
  call.asset.string <- paste0(CDSURL, "asset-installs?meas-strm-name=", DPID,
                              "&install-range-begin=",
                              startDate,
                              "&install-range-cutoff=",
                              endDate)
  
  spatialDataLB_title <- "Getting asset ID and stream number for calibration files."
  updateProgressBar(session = session, id = "spatialDataLB", value = 10, title = spatialDataLB_title)
  
  cal.asset.req <- httr::GET(call.asset.string)
  if(cal.asset.req$status_code != 200){
    stop(paste0("Asset ID and stream number could not be retrieved for ", DPID))
  }else{
    spatialDataLB_title <- paste0("Asset ID and stream number retrieved for ", DPID)
  }
  updateProgressBar(session = session, id = "spatialDataLB", value = 13, title = spatialDataLB_title)
  
  #Need to parse out the assetUid and streamId for the correct term
  cal.asset.text <- XML::xmlParse(httr::content(cal.asset.req, "text", encoding = "UTF-8"))
  
  # Form the url 
  #call.data.string <- paste0(CDSURL,'calibrations?msName=',DPID,'&startDate=',startDate,'&enddate=',endDate,collapse='')
  
  
  
  assetUid <- XML::xmlAttrs(cal.asset.text[["//asset"]])["assetUid"]
  streamNum <- XML::xmlValue(cal.asset.text[[paste0('//ingestTerm[term/termNumber="',substr(DPID,29,33),'"]/streamId')]])
  
  #Put together call
  call.data.string <- paste0(CDSURL, "calibrations?asset-stream-key=", assetUid, ":", streamNum,
                             "&startdate=",
                             startDate,
                             "&enddate=",
                             endDate)
  spatialDataLB_title <- "Getting calibration files"
  updateProgressBar(session = session, id = "spatialDataLB", value = 15, title = spatialDataLB_title)
  
  cal.data.req <- httr::GET(call.data.string,
                            httr::accept("application/vnd.neoninc.cds.calibration-list-v1.0+xml"))

  if(cal.data.req$status_code != 200){
    stop(spatialDataLB_title <- paste0("Calibration files could not be retrieved for ", DPID),
         updateProgressBar(session = session, id = "spatialDataLB", value = 18, title = spatialDataLB_title)
    )
  }else{
    spatialDataLB_title <- paste0("Calibration files retrieved for ", DPID)
  }
  updateProgressBar(session = session, id = "spatialDataLB", value = 18, title = spatialDataLB_title)
  
  
  cal.data.pars <- def.read.cal.xml.rest(xmlCalFile = cal.data.req)
  
  if(length(cal.data.pars)<1){
    spatialDataLB_title <- paste0("Calibration file(s) parsed with length < 1 for MS ",DPID," for ",startDate," to ",endDate,".")
  }else{
    spatialDataLB_title <- paste0('Successfully parsed ', length(cal.data.pars[,1]), " calibrations from ", DPID)
  }
  updateProgressBar(session = session, id = "spatialDataLB", value = 25, title = spatialDataLB_title)
  
  #Convert the dates to actual date objects
  cal.data.pars$validStartTime <- as.POSIXct(cal.data.pars$validStartTime, format = "%Y-%m-%dT%H:%M:%OSZ")
  cal.data.pars$validEndTime <- as.POSIXct(cal.data.pars$validEndTime, format = "%Y-%m-%dT%H:%M:%OSZ")
  
  return(cal.data.pars)
}
