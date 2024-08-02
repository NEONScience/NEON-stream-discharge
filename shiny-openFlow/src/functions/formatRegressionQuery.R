formatReg <- function(input, output, session)
{
  updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 70, title = "Retrieving regression data from L1")
  
  url="http://prod-os-ds-1.ci.neoninternal.org:8080/osDataService/results?pub-table-key=NEON.DOM.SITE.DP1.00133.001:csd_gaugeWaterColumnRegression_pub"
  req <- httr::GET(url=url,
                   httr::accept("application/vnd.neoninc.os.result-list-v1.0+xml"))
  
  
  xXML <- XML::xmlParse(httr::content(req, as="text", encoding = "UTF-8"))
  xSub <- XML::xpathApply(xXML, "//result")
  
  datList <- lapply(xSub, FUN=xmlToDataFrame, stringsAsFactors=F)
  
  for(j in 1:length(datList)){
    temp <- datList[[j]]
    temp <- temp[!is.na(temp$fieldName),]
    valueCol <- names(temp)[grepl("Value",names(temp))]
    for(i in 1:length(valueCol)){
      tempdf <- temp[,c("fieldName",valueCol[i])]
      tempdf <- tempdf[!is.na(tempdf[,2]),]
      names(tempdf)[2] <- "value"
      if(i==1)
      {
        cleandf <- tempdf
      }else{
        cleandf <- rbind(cleandf,tempdf)
        }
    }
    cleandfmerge <- data.frame(matrix(data=NA,nrow=1,ncol=nrow(cleandf)))
    names(cleandfmerge) <- cleandf$fieldName
    cleandfmerge[1,] <- cleandf$value
    if(j==1){
      cleandfall <- cleandfmerge
    }else{
        cleandfall <- merge(cleandfall,cleandfmerge,all = T)
        }
  }
  updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 35, title = "Retrieval of regression data finished")
  
  return(cleandfall)
}


#



#