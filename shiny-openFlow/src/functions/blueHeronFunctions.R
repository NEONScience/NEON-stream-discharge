#Blue Heron water height calculation functions

#Water pressure function, calculates water pressure from calibration and troll pressure values
waterPressure_calc <- function(calValCP0,calValCP1, calValCP2, trollPressure)
{
  waterPressure <- calValCP2*(trollPressure^2)+calValCP1*trollPressure+calValCP0
  return(waterPressure)
}

#water depth function, calculates water depth from water pressure, density, and gravity
waterDepth_calc <- function(waterPressure)
{
  waterDepth <- round(1000*(waterPressure/(999*9.81)),4) # 9.81 is gravity and 999 is the water density. This also converts kPA to PA.
  return(waterDepth)
}

waterColumnHeight_calc <- function(waterDepth, wellDepth, cableLength)
{
  if(!is.null(wellDepth) & !is.null(cableLength))
  {
    waterColumnHeight <- round(waterDepth + wellDepth - cableLength,4)
  }
}


BlueHeron <- function(input, output, session, realTimeSingleInput = NULL, realTimeSeriesInput = NULL)
{
  shinyjs::show("LB")
  tryCatch({
    
    #set DPname to either 'SW_elev' or 'GW_elev' allows for AquaTROLL and LevelTROLL for SW locations dependent on user selection
    if(input$waterType == "SW")
    {
      if(input$trollType == "AquaTroll")
      {
        DPname <- paste0(input$waterType,'_elev_aquatroll')
        
      } else{
        DPname <- paste0(input$waterType,'_elev_leveltroll')
        
      }
    } else{
      DPname <- paste0(input$waterType,'_elev')
    }
    
    #Site and domain inputs 
    site <- input$BH_site
    domain <- input$BH_domain
    
    #update to a date range that you know the troll in question was installed (small date range is better for not returning multiple cal files)
    startDate <- input$BH_dateRange[1]
    endDate <- input$BH_dateRange[2]
    
    #Sets the HOR for the location of interest (surface water locations are 101,102,131,132,110 and groundwater locations are 301-308 for wells 1-8)
    HOR <<- as.character(input$HOR)
    
    #Compiles DP Ids
    if(DPname=='GW_cond'){
      DPnum<-"DP0.20015.001.01371"
      VER<-"000"
    }else if(DPname=='GW_elev'){
      DPnum<-"DP0.20015.001.01376"
      VER<-"000"
    }else if(DPname=='GW_temp'){
      DPnum<-"DP0.20015.001.01374"
      VER<-"000"
    }else if(DPname=='SW_elev_leveltroll'){
      DPnum<-"DP0.20016.001.01379"
      VER<-"100"
    }else if(DPname=='SW_elev_aquatroll'){
      DPnum<-"DP0.20054.001.01376"
      VER<-"100"
    }else if(DPname=='SW_cond'){
      DPnum<-"DP0.20054.001.01371"
      VER<-"100"
    }else if(DPname=='SW_temp'){
      DPnum<-"DP0.20054.001.01374"
      VER<-"100"
    } #End of ifelse statements
    
    # Troubleshooting information, commented out unless needed
    # domain = "D07"
    # site = "WALK"
    # HOR = "101"
    # startDate = "2023-01-01"
    # endDate = "2023-01-28"
    # DPnum<-"DP0.20016.001.01379"
    # VER<-"100"
    # End troubleshooting information
    DPID = paste("NEON",domain,site,DPnum,HOR,VER,"000",sep=".")
    
    #Pulls calibration information
    calibrations<-grabCalibrations(CDSURL = "http://den-prodcdsllb-1.ci.neoninternal.org/cdsWebApp/",
                                   startDate = startDate,
                                   endDate = endDate,
                                   DPID = DPID,
                                   session,
                                   input,
                                   output)
    #STEP 2: Grab sensor elevation data
    
    #Get site history for all named locations. I have not included any elevation information at this time. It could be added.
    updateProgressBar(session = session, id = "spatialDataLB", value = 35, title = "Gathering spatial data")
    locations <- getLocBySite(site, type = "all", history = T)
    updateProgressBar(session = session, id = "spatialDataLB", value = 75, title = "Finished gathering spatial data")
    locations_troll<-locations[grepl("Water Level",locations$locationDescription),]
    locations_wells<-locations[grepl("Groundwater Well ",locations$locationDescription),]
    locations<-rbind(locations_troll,locations_wells)
    #locations<-locations[!grepl("Not Used",locations$locationDescription),]
    
    #No information is used from this to calculate water depth. Elevated water depth and other elevation information could be added if needed or wanted.
    TrollLocations<<-locations %>%
      select('namedLocation','locationDescription', 'locationStartDate', 'locationEndDate','domainID','siteID','elevation','zOffset')
    
    
    if(input$L0Choice == "Yes")
    {
      shinyjs::hide("singleWaterColumnHeight")
      shinyjs::hide("singlePressureOutput")
      #Grab L0 data from local .csv download of L0 data
      if(is.null(realTimeSeriesInput))
      {
        #Query L0 data
        trollL0Data <<- processWaterData(input, output, session, DPID = DPID, startDate = startDate, endDate = endDate)
      } else {
        trollL0Data <- read.csv("tempBHData/Data.csv")
      }

      #grabs the relevant calibration information from the downloaded calibrations data
      calibration_info <- as.data.frame(rbind(calibrations$value[1:3]))
      colnames(calibration_info) <- c("calValCP0","calValCP1","calValCP2")
      #combine the trollPressure, Dates, and calibrations information into one DF
      waterElevationDF <- as.data.frame(cbind(as.character(trollL0Data$startDate), as.numeric(trollL0Data$numberValue), as.numeric(rep(calibration_info$calValCP0)), as.numeric(rep(calibration_info$calValCP1)), as.numeric(rep(calibration_info$calValCP2)), rep(999), rep(9.81)))
      colnames(waterElevationDF) <- c("Date","trollPressure", "calValCP0","calValCP1","calValCP2", "waterDensity", "Gravity")
      
      #For some reason when combining even as.numeric the variables below come out as character columns. Re-applying the command seems to work.
      waterElevationDF$trollPressure <- as.numeric(trollL0Data$numberValue)
      waterElevationDF$calValCP0 <- as.numeric(waterElevationDF$calValCP0)
      waterElevationDF$calValCP1 <- as.numeric(waterElevationDF$calValCP1)
      waterElevationDF$calValCP2 <- as.numeric(waterElevationDF$calValCP2)
      
      #Calculate water pressure
      waterPressure <- waterPressure_calc(calValCP0 = waterElevationDF$calValCP0, calValCP1 = waterElevationDF$calValCP1,calValCP2 = waterElevationDF$calValCP2, trollPressure = waterElevationDF$trollPressure)
      #Calculate water depth 
      waterDepth <- waterDepth_calc(waterPressure)
      #Add water pressure and depth to DF
      waterElevationDF <<- cbind(waterElevationDF,waterPressure,waterDepth)
      
      #adjust for elevation offset
      TrollLocations <- TrollLocations[order(TrollLocations$locationEndDate)]
      TrollLocations$refElevPlusZ <- TrollLocations$elevation + TrollLocations$zOffset
      for(i in 1:length(TrollLocations$locationEndDate))
      {
        locStart <- TrollLocations$locationStartDate[i]
        locEnd <- TrollLocations$locationEndDate[i]
        dataToApplyOffset <- waterElevationDF$waterDepth[waterElevationDF$Date>=locStart&
                                                           waterElevationDF$Date<locEnd]
        if(i==1)
        {
          locOffset <- 0
          waterElevationDF$waterDepth[waterElevationDF$Date>=locStart&
                                        waterElevationDF$Date<locEnd] <- dataToApplyOffset+as.numeric(locOffset)
        } else {
          locOffset <- TrollLocations$refElevPlusZ[i] - TrollLocations$refElevPlusZ[1]
          waterElevationDF$waterDepth[waterElevationDF$Date>=locStart&
                                                             waterElevationDF$Date<locEnd] <- dataToApplyOffset+as.numeric(locOffset)
        }
      }

      
      output$waterElevation <- renderPlotly({
        waterElevationPlot <- plot_ly(waterElevationDF, x = ~Date, y = ~waterDepth, type = 'scatter', mode = 'lines') %>% layout(xaxis= list(title = "Date",autotick = T,nticks = 25, tickmode = "auto"), yaxis = list(title = 'water depth above sensor'))
      }) #End of waterElevation output
      shinyjs::show("waterElevation")
      shinyjs::show("Title_CWE")
      
      #Add water column height graph if information is provided
      if(!is.na(input$wellDepth) & !is.na(input$cableLength) & input$waterType == "GW")
      {
        waterColumnHeight <- waterColumnHeight_calc(waterDepth, input$wellDepth,input$cableLength)
        waterElevationDF <- cbind(waterElevationDF,waterColumnHeight)
        output$waterColumnHeightPlot <- renderPlotly({
          waterElevationPlot <- plot_ly(waterElevationDF, x = ~Date, y = ~waterColumnHeight, type = 'scatter', mode = 'lines') %>% layout(xaxis= list(title = "Date",autotick = T,nticks = 25, tickmode = "auto"), yaxis = list(title = 'water column height'))
        }) 
        shinyjs::show("waterColumnHeightPlot")
        shinyjs::show("Title_WCH")
      }
      
    } else{ #This else is for single inputs of pressure so it will calculate differently
      shinyjs::hide("singleWaterColumnHeight")
      
      #grabs the relevant calibration information from the downloaded calibrations data
      calibration_info <- as.data.frame(rbind(calibrations$value[1:3]))
      colnames(calibration_info) <- c("calValCP0","calValCP1","calValCP2")
      waterElevationDF <- as.data.frame(cbind(as.numeric(input$singlePressure), as.numeric(rep(calibration_info$calValCP0)), as.numeric(rep(calibration_info$calValCP1)), as.numeric(rep(calibration_info$calValCP2)), rep(999), rep(9.81)))
      colnames(waterElevationDF) <- c("trollPressure", "calValCP0","calValCP1","calValCP2", "waterDensity", "Gravity")
      waterPressure <- waterPressure_calc(calValCP0 = waterElevationDF$calValCP0, calValCP1 = waterElevationDF$calValCP1,calValCP2 = waterElevationDF$calValCP2, trollPressure = waterElevationDF$trollPressure)
      waterDepth <- waterDepth_calc(waterPressure)
      waterElevationDF <- cbind(waterElevationDF,waterPressure,waterDepth)
      
      
      if(!is.na(input$wellDepth) & !is.na(input$cableLength) & input$waterType == "GW")
      {
        waterColumnHeight <- waterColumnHeight_calc(waterDepth, input$wellDepth, input$cableLength)
        output$singleWaterColumnHeight <- renderText({paste0("Current water column height: ",waterColumnHeight,"m")})
        shinyjs::show("singleWaterColumnHeight")
        shinyjs::show("Title_WCH")
      }
      
      shinyjs::show("singlePressureOutput")
      shinyjs::show("Title_CWE")
      shinyjs::hide("waterElevation")
      shinyjs::hide("waterColumnHeightPlot")
      waterElevationDF$waterDepth[1] <- waterElevationDF$waterDepth[1]
      output$singlePressureOutput <- renderText({paste0("Current water depth: ",waterElevationDF$waterDepth[1],"m")})
    } #End of ifelse statement
    
    updateProgressBar(session = session, id = "spatialDataLB", value = 100, title = paste0("All information gathered for ",input$site))
    shinyjs::hide("LB")
    # warning("It appears there is an issue with the settings selected. Please try different settings.")
  })
}