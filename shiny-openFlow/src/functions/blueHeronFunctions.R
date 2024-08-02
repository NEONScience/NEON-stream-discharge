#Blue Heron water height calculation functions

#Water pressure function, calculates water pressure from calibration and troll pressure values
waterPressure_calibration <- function(calValCP0,calValCP1, calValCP2, trollPressure)
{
  waterPressureCalibrated <- calValCP2*trollPressure**2+calValCP1*trollPressure+calValCP0
  return(waterPressureCalibrated)
}

#water depth function, calculates water depth from water pressure, density, and gravity
waterColumn_calc <- function(waterPressureCalibrated)
{
  waterColumnHeight <- (waterPressureCalibrated/(waterDensity*gravity)) * convKPatoPa # 9.81 is gravity and 999 is the water density. This also converts kPA to PA.
  
  if(exists("wellDepth") & exists("cableLength"))
  {
    if(!is.null(wellDepth) & !is.null(cableLength))
    {
      waterColumnHeight <- round(waterColumnHeight + wellDepth - cableLength,4)
    }
  }
  
  return(waterColumnHeight)
}

# wellWaterColumn_calc <- function(waterDepth, wellDepth, cableLength)
# {
#   if(!is.null(wellDepth) & !is.null(cableLength))
#   {
#     waterColumnHeight <- round(waterDepth + wellDepth - cableLength,4)
#   }
# }


BlueHeron <- function(input, output, session, realTimeSingleInput = NULL, realTimeSeriesInput = NULL)
{
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
    HOR <- as.character(input$HOR)
    
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
    # site = "LECO"
    # HOR = "102"
    # startDate = "2023-01-01"
    # endDate = "2023-01-28"
    # DPnum<-"DP0.20016.001.01379"
    # VER<-"100"
    # End troubleshooting information
    DPID <- paste("NEON",domain,site,DPnum,HOR,VER,"000",sep=".")
    
    #Pulls calibration information
    calibrations <- grabCalibrations(CDSURL = "http://den-prodcdsllb-1.ci.neoninternal.org/cdsWebApp/",
                                   startDate = startDate,
                                   endDate = endDate,
                                   DPID = DPID,
                                   session,
                                   input,
                                   output)
    # warning("It appears there is an issue with the settings selected. Please try different settings.")
    
    #grabs the relevant calibration information from the downloaded calibrations data
    calibration_info <- as.data.frame(rbind(calibrations$value[1:3]))
    colnames(calibration_info) <- c("calValCP0","calValCP1","calValCP2")
    # Downloads location data and formats/filters the data

    if(input$L0Choice == "Yes")
    {
      shinyjs::hide("singleWaterColumnHeight")
      #Grab L0 data from local .csv download of L0 data
      if(is.null(input$L0File))
      {
        #Query L0 data
        L0TrollInformation <- L0DataQuery(input, output, session, DPID = DPID, startDate = startDate, endDate = endDate)
        trollL0Data <- xmlToDataFrame(L0TrollInformation$L0Data)
        trollL0Data <- trollL0Data[-1,]
        trollNamedLocation <- as.data.frame(L0TrollInformation$namedLocation)
      } else {
        trollL0Data <- read.csv(input$L0File$datapath) %>% 
          select(paste0("NEON.",domain,".",site,".DP0.20016.001.01379.132.100.000.time"), paste0("NEON.",domain,".",site,".DP0.20016.001.01379.132.100.000.data"))
        colnames(trollL0Data) <- c("startDate","numberValue")
        startDate <- min(trollL0Data$startDate)
        endDate <- min(trollL0Data$endDate)
      }
      
      #combine the trollPressure, Dates, and calibrations information into one DF
      waterElevationDF <- as.data.frame(cbind(as.character(trollL0Data$startDate), as.numeric(trollL0Data$numberValue), as.character(trollNamedLocation$name), as.numeric(rep(calibration_info$calValCP0)), as.numeric(rep(calibration_info$calValCP1)), as.numeric(rep(calibration_info$calValCP2)), rep(waterDensity), rep(gravity)))
      colnames(waterElevationDF) <- c("Date","trollPressure", "namedLocation", "calValCP0","calValCP1","calValCP2", "waterDensity", "Gravity")
      
      #For some reason when combining even as.numeric the variables below come out as character columns. Re-applying the command seems to work.
      waterElevationDF$trollPressure <- as.numeric(trollL0Data$numberValue)
      waterElevationDF$calValCP0 <- as.numeric(waterElevationDF$calValCP0)
      waterElevationDF$calValCP1 <- as.numeric(waterElevationDF$calValCP1)
      waterElevationDF$calValCP2 <- as.numeric(waterElevationDF$calValCP2)
      
      #Calculate water pressure
      waterPressureCalibrated <- waterPressure_calibration(calValCP0 = waterElevationDF$calValCP0, calValCP1 = waterElevationDF$calValCP1,calValCP2 = waterElevationDF$calValCP2, trollPressure = waterElevationDF$trollPressure)
      #Calculate water depth 
      waterColumnHeight <- waterColumn_calc(waterPressureCalibrated)
      #Add water pressure and depth to DF
      waterElevationDF <- cbind(waterElevationDF,waterPressureCalibrated,waterColumnHeight)
      
      waterElevationDF <- TLOC(input, output, session, site, startDate, endDate, waterElevationDF)
      waterElevationDFSW <<- waterElevationDF
      if(input$waterType == "SW")
      {
        regressionData <- formatReg(input, output, session)
        waterElevationDFBEFORE <<- waterElevationDF
        waterElevationDF <- applyRegtoL0(regressionData = regressionData, L0PressureData = waterElevationDF, site = site, startDate = startDate, endDate = endDate, session) 
        waterElevationDFAFTER <<- waterElevationDF
        output$calculatedStagePlot <- renderPlotly({
          waterElevationPlot <- plot_ly(waterElevationDF, x = ~Date, y = ~calculatedStage, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>% 
            layout(xaxis= list(title = "Date",autotick = T,nticks = 25, tickmode = "auto"), yaxis = list(title = 'Calculated stage height (m)'))
          }) #End of waterElevation output
        }
        else {
          output$calculatedStagePlot <- renderPlotly({
            waterElevationPlot <- plot_ly(waterElevationDF, x = ~format(Date, "%Y-%m-%d %H:%M"), y = ~waterColumnHeight, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>% 
              layout(xaxis= list(title = "Date",autotick = T,nticks = 25, tickmode = "auto"), yaxis = list(title = 'Water column height (m)'))
            }) #End of waterElevation output
          }


      shinyjs::show("calculatedStagePlot")
      shinyjs::show("Title_CWE")
      
    } else{ # Single input pressure from user on else
      shinyjs::hide("calculatedStagePlot")
      trollNamedLocation <- L0DataQuery(input, output, session, DPID = DPID, startDate = startDate, endDate = endDate)
      
      #combine the trollPressure, Dates, and calibrations information into one DF
      waterElevationDF <- as.data.frame(cbind(as.character(startDate), as.numeric(input$singlePressure), as.character(trollNamedLocation), as.numeric(rep(calibration_info$calValCP0)), as.numeric(rep(calibration_info$calValCP1)), as.numeric(rep(calibration_info$calValCP2)), rep(waterDensity), rep(gravity)))
      colnames(waterElevationDF) <- c("Date","trollPressure", "namedLocation","calValCP0","calValCP1","calValCP2", "waterDensity", "Gravity")
      
      #For some reason when combining even as.numeric the variables below come out as character columns. Re-applying the command seems to work.
      waterElevationDF$trollPressure <- as.numeric(input$singlePressure)
      waterElevationDF$calValCP0 <- as.numeric(waterElevationDF$calValCP0)
      waterElevationDF$calValCP1 <- as.numeric(waterElevationDF$calValCP1)
      waterElevationDF$calValCP2 <- as.numeric(waterElevationDF$calValCP2)
      
      #Calculate water pressure
      waterPressureCalibrated <- waterPressure_calibration(calValCP0 = waterElevationDF$calValCP0, calValCP1 = waterElevationDF$calValCP1,calValCP2 = waterElevationDF$calValCP2, trollPressure = waterElevationDF$trollPressure)
      #Calculate water depth 
      waterColumnHeight <- waterColumn_calc(waterPressureCalibrated)
      #Add water pressure and depth to DF
      waterElevationDF <- cbind(waterElevationDF,waterPressureCalibrated,waterColumnHeight)
      waterElevationDF <- TLOC(input, output, session, site, startDate, endDate, waterElevationDF)
      if(input$waterType == "SW")
      {
        regressionData <- formatReg(input, output, session)
        waterElevationDF <- applyRegtoL0(regressionData = regressionData, L0PressureData = waterElevationDF, site = site, startDate = startDate, endDate = endDate, session)
        output$singleWaterColumnHeight <- renderText({paste0("Calculated stage height: ",round(waterElevationDF$calculatedStage[1],3),"m")})
      } else {
        output$singleWaterColumnHeight <- renderText({paste0("Calculated water column height: ",round(waterElevationDF$waterColumnHeight[1],3),"m")})
      }

      
      shinyjs::show("singleWaterColumnHeight")
      shinyjs::show("Title_CWE")
      
    } #End of ifelse statement
    
    updateProgressBar(session = session, id = "gaugeHeightLoadBar", value = 100, title = paste0("Finished applying relevant data to L0 pressure data for: ",input$site))
    shinyjs::hide("gaugeHeightLoadBar")
  }, error = function(err) {
    updateProgressBar(session = session, id = "gaugeHeightLoadBar", value = 0, title = paste0("There was an error for: ", DPID, ", please double check that the inputs are correct"))
    return()
  })
}