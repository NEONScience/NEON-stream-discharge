parseOSData <- function(x, wb)
{
  xXML <- XML::xmlParse(content(x, as="text", encoding = "UTF-8"))
  xSub <- XML::xpathApply(xXML, "//result")
  #Kind of long, but not minutes
  datList <- lapply(xSub, FUN=xmlToDataFrame, stringsAsFactors=F)
  
  if(length(datList)<1){
    print("Retrieved data contains no records (def.read.os.rest.R)")
    return(datList)
  }
  
  uuids <- character(length(datList))
  #A couple of minutes
  for(i in 1:length(datList)) {
    uuids[i] <- xmlAttrs(xXML[[paste("//result[", i, "]", sep="")]])[1]
  }
  
  wbFields <- data.frame(wb$rank, row.names=wb$fieldName)
  
  #Also a couple of minutes
  datListFlat <- lapply(datList, FUN=make.flat, wbFields)
  
  datFrm <- datListFlat[[1]]
  
  #Also takes quite a bit of time
  if(length(datListFlat)>1){
    for(i in 2:length(datListFlat)) {
      datFrm <- merge(datFrm, datListFlat[[i]], all=T, by="Row.names", suffixes = c((i-1),i))
    }
  }
  
  datFrm <- t(datFrm)
  colnames(datFrm) <- datFrm[1,]
  datFrm <- as.data.frame(datFrm)
  datFrm <- datFrm[-1,]
  datFrm$uid <- uuids
  row.names(datFrm) <- 1:length(datFrm$startDate)
  return(datFrm)
}
