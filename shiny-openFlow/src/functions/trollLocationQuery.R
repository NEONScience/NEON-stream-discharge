TLOC <- function(input, output, session, site, startDate, endDate, waterElevationDF)
{
  #STEP 2: Grab sensor elevation data
  TLOC_OutputNodes <- c("locationHistory","location","coordinates")
  # updateProgressBar(session = session, id = "spatialDataLB", value = 35, title = "Gathering spatial data")
  updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 30, title = "Requesting url")
  
  locationsREQ <- GET(paste0("http://den-prodcdsllb-1.ci.neoninternal.org/cdsWebApp/",
                           "namedlocations?name=",
                           waterElevationDF$namedLocation[1],
                           "&getAllLocationHistory=true"),
                    accept("application/vnd.neoninc.cds.namedlocation-v1.0+xml"))
  
  if(locationsREQ$status_code != 200){
    updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 0, title = paste0("Location data could not be retreived for ", waterElevationDF$namedLocation[1]))
    stop()
  }else{
    updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 35, title = paste0("Location retrieved for ", waterElevationDF$namedLocation[1]))
    
  }
  
  loc.xml <- xmlParse(content(locationsREQ, as="text", encoding = "UTF-8"))
  
  locationDomain <- xmlAttrs(loc.xml[["//domain"]])["name"]
  numHist <- length(xpathApply(loc.xml, "//locationHistory"))
  
  #Initialize output DF
  trollLocations <- data.frame(matrix(data=NA, ncol=1, nrow=numHist))
  #Loop through nodes and history to create full history DF
  for(i in 1:length(TLOC_OutputNodes)){
    tempXML <- xpathApply(loc.xml, paste0("//",TLOC_OutputNodes[i]))
    
    #Loop through removing the "propertyValue" nodes as you build the dataframe
    newCols <- xmlToDataFrame(tempXML[1])
    newCols <- newCols[,!is.na(names(newCols))]
    if(numHist > 1){
      for(j in 2:numHist){
        tempCols <- xmlToDataFrame(tempXML[j])
        tempCols <- tempCols[,!is.na(names(tempCols))]
        
        #If there are columns in one but not the other, add the column with NA data
        if(all(names(newCols)%in%names(tempCols)) && all(names(tempCols)%in%names(newCols))){
          newCols <- rbind(newCols,tempCols)
        }else{
          allNeededNames <- union(names(newCols),names(tempCols))
          addToNew <- allNeededNames[which(!allNeededNames%in%names(newCols))]
          addToTemp <- allNeededNames[which(!allNeededNames%in%names(tempCols))]
          
          for(k in addToNew){
            newCols[,(ncol(newCols)+1)] <- NA
            names(newCols)[ncol(newCols)] <- k
          }
          
          for(l in addToTemp){
            tempCols[,(ncol(tempCols)+1)] <- NA
            names(tempCols)[ncol(tempCols)] <- l
          }
          newCols <- rbind(newCols,tempCols)
        }
      }
    }
    
    trollLocations <- cbind(trollLocations,newCols)
  }
  #Add the domain of the location
  trollLocations$domain <- locationDomain
  
  if(length(trollLocations$startDate) < 1){
    updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 0, title = paste0("Number of named location locations <1"))
    
  }else{
    updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 50, title = paste0("Successfully retrieved ",
                                                                                               length(trollLocations$startDate) ,
                                                                                               " named location locations for ",
                                                                                               trollLocations$namedLocation))
  }
  
  trollLocations$zOffset <- as.numeric(trollLocations$zOffset)
  trollLocations$elevation <- as.numeric(trollLocations$elevation)
  # locations_trolls<-locations[grepl("Water Level",locations$locationDescription),]
  # locations_wells<-locations[grepl("Groundwater Well ",locations$locationDescription),]
  # trollLocations <- rbind(locations_trolls,locations_wells)
  
  #No information is used from this to calculate water depth. Elevated water depth and other elevation information could be added if needed or wanted.
  trollLocations <- trollLocations %>% 
    select(startDate, endDate, namedLocationOffset, zOffset, elevation) %>% 
    arrange(startDate)  
   
  for(i in 1:nrow(trollLocations))
  { 
    if(is.na(trollLocations$endDate[i]) & i != nrow(trollLocations))
    {
      trollLocations$endDate[i] <- trollLocations$startDate[i+1]
    } else if(i == nrow(trollLocations)) {
      trollLocations$endDate[is.na(trollLocations$endDate)] <- as.character(now())
    }
  }
  
  # if(nrow(trollLocations[startDate <= trollLocations$startDate & trollLocations$endDate >= endDate,]) == 0)
  # {
  #   trollLocationsRef <- trollLocations[trollLocations$startDate==max(trollLocations$startDate)]
  # } else {
  #   trollLocationsRef <- trollLocations[startDate <= trollLocations$startDate & trollLocations$endDate >= endDate,]
  # }
  #adjust for elevation offset
  # trollLocationsBeforeOffset <<- trollLocations
  # trollLocations <- trollLocationsBeforeOffset
  
  if(input$waterType == "GW")
  {
    well_depth_file_CFGLOC <- well_depth_file %>% filter(cfgloc == waterElevationDF$namedLocation[1])
    trollLocations <- trollLocations %>%
      mutate(refElevPlusZ = elevation + zOffset) %>%
      mutate(zRefOffset = append(0, refElevPlusZ[2:length(refElevPlusZ)] - refElevPlusZ[1])) %>%
      mutate(distanceToAdd = well_depth_file_CFGLOC$well_depth-well_depth_file_CFGLOC$cable_length)
    # distanceToAdd <<- trollLocations$distanceToAdd
    
  }
  else {
      trollLocations <- trollLocations %>%
        mutate(refElevPlusZ = elevation + zOffset) %>%
        mutate(zRefOffset = append(0, refElevPlusZ[2:length(refElevPlusZ)] - refElevPlusZ[1]))      
    }
    #I will need to come back to this later and make it better
    
    # startDate <- "2024-07-30"
  # endDate <- "2024-08-01"
  if(nrow(trollLocations[startDate <= trollLocations$startDate & trollLocations$endDate >= endDate,]) == 0)
  {
    trollLocations <- trollLocations[trollLocations$startDate==max(trollLocations$startDate),]
  } else {
    trollLocations <- trollLocations[startDate <= trollLocations$startDate & trollLocations$endDate >= endDate,]
  }
  # trollLocationsCopy <<- trollLocations
  # waterElevationDFCopy <<- waterElevationDF
  # waterElevationDF <- waterElevationDFCopy
  for(i in 1:nrow(trollLocations))
  {
    locStart <- trollLocations$startDate[i]
    locEnd <- trollLocations$endDate[i]
    dataToApplyOffset <- waterElevationDF$waterColumnHeight[waterElevationDF$Date>=locStart&
                                                              waterElevationDF$Date<locEnd]
    
    if(input$waterType == "GW")
    {
      waterElevationDF$waterColumnHeight[waterElevationDF$Date>=locStart&
                                           waterElevationDF$Date<locEnd] <- dataToApplyOffset+as.numeric(trollLocations$distanceToAdd)
    } else {
      waterElevationDF$waterColumnHeight[waterElevationDF$Date>=locStart&
                                           waterElevationDF$Date<locEnd] <- dataToApplyOffset+as.numeric(trollLocations$zRefOffset)
    }

  }

  updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 60, title = "Finished querying location data and applying zOffset")
  
  return(waterElevationDF)
}