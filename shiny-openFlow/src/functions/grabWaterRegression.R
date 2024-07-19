# make function: get.Q.Stag.Data.API.R

grabWaterRegression <- function()
{
  queriedRegressionData <- httr::GET("http://int-os-ds-1.ci.neoninternal.org:8080/osDataService/pub-tables/DP4.00130.001:csd_gaugeWaterColumnRegression_pub", httr::accept("application/vnd.neoninc.os.pub-table-v1.0+xml"))
  
  if(queriedRegressionData$status_code != 200){
    stop(paste0("Pub WB could not be retrieved for ", "DP4.00130.001", ":", osTable))
  }else{
    print(paste0("Pub WB retrieved for ", "DP4.00130.001", ":", osTable))
  }
  regressionData <- XML::xmlToDataFrame(httr::content(queriedRegressionData, as="text", encoding = "UTF-8"))
  regressionData <- regressionData[which(!is.na(regressionData$rank)),]
  
  regressionData <- try(read.csv(paste(downloadedDataPath,"filesToStack00130","stackedFiles",paste0(mod,"_gaugeWaterColumnRegression.csv"), sep = "/")),silent = T)
  regressionData$regressionStartDate <- as.POSIXct(regressionData$regressionStartDate, format = osPubDateFormat)
  regressionData$regressionEndDate <- as.POSIXct(regressionData$regressionEndDate, format = osPubDateFormat)
  regressionData$regressionSlope <- as.numeric(regressionData$regressionSlope)
  regressionData$regressionIntercept <- as.numeric(regressionData$regressionIntercept)
}
