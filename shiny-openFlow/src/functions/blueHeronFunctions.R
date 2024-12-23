#This file contains functions related to what was the original Blue Heron application which is now the real-time-data-viewer tab within the OpenFlow application

#rtdv is shorthand for real-time-data-viewer 

####################################################################################
# Function Name: waterPressure_calibration 
# Arguments: calValCP0, calvalCP1, calvalCP2, trollPressure
# Description: applies calibration information to troll pressure (kPa) and returns the calibrated troll pressure 
####################################################################################

waterPressure_calibration <- function(calValCP0,calValCP1, calValCP2, trollPressure)
{
  waterPressureCalibrated <- calValCP2*trollPressure**2+calValCP1*trollPressure+calValCP0
  return(waterPressureCalibrated)
}

####################################################################################
# Function Name: waterColumn_calc 
# Arguments: waterPressureCalibrated
# Description: converts the calibrated water pressure to water column height using water density, gravity
####################################################################################
waterColumn_calc <- function(waterPressureCalibrated)
{
  waterColumnHeight <- (waterPressureCalibrated/(waterDensity*gravity)) * convKPatoPa 
  
  return(waterColumnHeight)
}

####################################################################################
# Function Name: realTimeDataViewer
# Arguments: input, output, session, realTimeSingleInput = NULL, realTimeSeriesInput = NULL
# Description: Converts water pressure as kPa from level and aqua trolls to calculated stage height and water column height respectively
####################################################################################
realTimeDataViewer <- function(input, output, session, realTimeSingleInput = NULL, realTimeSeriesInput = NULL)
{
    #read in the grafana csv file the user uploads
    if(input$dataSource == "Grafana CSV File"){ # Use Grafana CSV for kPa pressure readings
        if(!is.null(input$grafanaFile$datapath))
        {
          trollL0Data <- read.csv(input$grafanaFile$datapath)
          trollL0Data <- trollL0Data[-1,]
          colnames(trollL0Data) <- c("startDate","numberValue")
          startDate <- as.Date(min(trollL0Data$startDate))
          endDate <- as.Date(max(trollL0Data$startDate))
          #Fix the enddate to be the day before. Same day calibration information pulls causes an error so this is the work around
          if(endDate == Sys.Date())
          {
            endDate <- endDate-1
            if(startDate == endDate)
            {
              startDate <- startDate-1
            }
          }
        }
    } else {
      #update to a date range that you know the troll in question was installed (small date range is better for not returning multiple cal files)
      startDate <- input$rtdvDaterange[1]
      endDate <- input$rtdvDaterange[2]
    }
  
    # Set site and domain inputs 
    site <- input$rtdvSite
    domain <- input$rtdvDomain
    
    #Sets the HOR for the location of interest (surface water locations are 101,102,131,132,110 and groundwater locations are 301-308 for wells 1-8)
    HOR <- as.character(input$HOR)

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
    
    #Compiles DP Ids
    if(DPname=='GW_elev'){
      DPnum<-"DP0.20015.001.01376"
      VER<-"000"
    }else if(DPname=='SW_elev_leveltroll'){
      DPnum<-"DP0.20016.001.01379"
      VER<-"100"
    }else if(DPname=='SW_elev_aquatroll'){
      DPnum<-"DP0.20054.001.01376"
      VER<-"100"
    }
    
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
    tryCatch({
      calibrations <- grabCalibrations(CDSURL = "http://den-prodcdsllb-1.ci.neoninternal.org/cdsWebApp/",
                                       startDate = startDate,
                                       endDate = endDate,
                                       DPID = DPID,
                                       session,
                                       input,
                                       output)
    }, error = function(e){
      updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 0, title = paste0("There was a problem retrieving the calibration information for ",input$rtdvSite,". Please verify that the HOR location is correct. 
                                                                                                Contact the author if problem persists."))
      Sys.sleep(5)
      removeModal()
    })
    
    if(exists("calibrations"))
    {
      #get the relevant calibration information from the calibrations data frame
      calibration_info <- as.data.frame(rbind(calibrations$value[1:3]))
      colnames(calibration_info) <- c("calValCP0","calValCP1","calValCP2")
  
      # Checking to see what the user selected in the dataSource input which determines how the data will be handled/sourced
      if(input$dataSource != "Instant Pressure Reading")
      {
        shinyjs::hide("singleWaterColumnHeight")
        # Queries L0 data if it is chosen in the dataSource input
        dbquery <- paste0("SELECT * FROM rcdata WHERE \"curveID\"  = (
                            SELECT MAX(\"curveID\")
                            FROM rcdata WHERE \"curveID\" LIKE ", "'%",input$rtdvSite,"%')")
        rcdata <- DBI::dbSendQuery(con,dbquery)
        rcData <- DBI::dbFetch(rcdata)
        
        if(input$dataSource == "L0 Data Query")
        {
          # Query L0 data
          L0TrollInformation <- L0DataQuery(input, output, session, DPID = DPID, startDate = startDate, endDate = endDate)
          trollL0Data <- xmlToDataFrame(L0TrollInformation$L0Data)
          trollL0Data <- trollL0Data[-1,]
          trollNamedLocation <- as.data.frame(L0TrollInformation$namedLocation)
          trollNamedLocation <- trollNamedLocation
          waterElevationDF <- as.data.frame(cbind(as.character(trollL0Data$startDate), as.numeric(trollL0Data$numberValue), as.character(trollNamedLocation$name), as.numeric(rep(calibration_info$calValCP0)), as.numeric(rep(calibration_info$calValCP1)), as.numeric(rep(calibration_info$calValCP2)), rep(waterDensity), rep(gravity)))
          
        } else if(input$dataSource == "Grafana CSV File"){ # Use Grafana CSV for kPa pressure readings
          if(!is.null(input$grafanaFile$datapath))
          {
            shinyjs::show("rtdvStageDischargePlotly")
            
            numberValue_split <- as.data.frame(str_split(trollL0Data$numberValue, pattern = " "))
            trollL0Data$numberValue <- as.numeric(t(numberValue_split[1,]))
            #colnames(trollL0Data) <- c("startDate","numberValue")
            trollNamedLocation <- L0DataQuery(input, output, session, DPID = DPID, startDate = startDate, endDate = endDate)
            waterElevationDF <- as.data.frame(cbind(as.character(trollL0Data$startDate), as.numeric(trollL0Data$numberValue), as.character(trollNamedLocation), as.numeric(rep(calibration_info$calValCP0)), as.numeric(rep(calibration_info$calValCP1)), as.numeric(rep(calibration_info$calValCP2)), rep(waterDensity), rep(gravity)))
          }
        }
        
        #combine the trollPressure, Dates, and calibrations information into one DF
        colnames(waterElevationDF) <- c("Date","trollPressure", "namedLocation", "calValCP0","calValCP1","calValCP2", "waterDensity", "Gravity")
        if(input$dataSource == "L0 Data Query") {          
          waterElevationDF$Date <- as.POSIXct(gsub("\\.[0-9]{3}Z$","",waterElevationDF$Date),tz="UTC",format="%Y-%m-%dT%H:%M:%S",origin="1970-01-01")
          }
        waterElevationDF <- waterElevationDF[order(waterElevationDF$Date),]
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
        
        shinyjs::show("rtdvStageDischargePlotly")
        if(input$waterType == "SW")
        {
          shinyjs::show("Title_CSH")
          shinyjs::hide("Title_CWE")
          regressionData <- formatReg(input, output, session)
          waterElevationDF <- applyRegtoL0(regressionData = regressionData, L0PressureData = waterElevationDF, site = site, startDate = startDate, endDate = endDate, session) 
            

            for(i in 1:nrow(waterElevationDF))
            {
              estimatedDischarge = rcData$maxPostQ[which(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i])==min(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i])))]
              pramUTop = rcData$pramUTop[which(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i])==min(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i])))]
              pramUBottom = rcData$pramUBottom[which(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i])==min(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i])))]
              waterElevationDF$estimatedDischarge[i] <- estimatedDischarge
              waterElevationDF$uncertaintyUp[i] <- pramUTop
              waterElevationDF$uncertaintyBottom[i] <- pramUBottom
            }
            waterElevationDF <- waterElevationDF %>% 
              mutate(Date_15min = floor_date(Date, "15 minutes")) %>% 
              group_by(Date_15min) %>% 
              summarise(uncertaintyUp_15min = mean(uncertaintyUp, na.rm = TRUE),
                        uncertaintyBottom_15min = mean(uncertaintyBottom, na.rm = TRUE),
                        estimatedDischarge_15min = mean(estimatedDischarge, na.rm = TRUE))
            # waterElevationDF <<- waterElevationDF
            
            sd_discharge <- SharedData$new(waterElevationDF, group = "waterElevation")

            DischargePlotly <- plot_ly(sd_discharge, source = "dischargeHover_source")
            DischargePlotly <- DischargePlotly %>%
              plotly::add_trace(x=~Date_15min,y=~as.numeric(uncertaintyUp_15min),name="Discharge Bottom Uncertainty",type='scatter',mode='line',line=base::list(color='#E69F00'))%>%
              plotly::add_trace(x=~Date_15min,y=~as.numeric(uncertaintyBottom_15min),name="Discharge Upper Uncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#E69F00') %>%
              plotly::add_trace(x=~Date_15min,y=~as.numeric(estimatedDischarge_15min),name="Estimated Discharge",type='scatter',mode='lines',line=list(color="#009E73")) %>%
              #plotly::add_trace(x=~Date,y=~as.numeric(calculatedStage),name="Estimated Gauge",type='scatter',mode='lines',line=list(color="#F0E442"), yaxis = "y2") %>%
              layout(yaxis = list(title = "Estimated Discharge (lps)"), hovermode = "x unified", title = paste("Discharge for",input$rtdvSite," (15min avg)"))

            output$rtdvStageDischargePlotly <- renderPlotly(
              DischargePlotly
            )
            
            # import rating curve graph and plot to UI
            rcPlot <- RC.plot(site, startDate, endDate)
            rcPlotly <- rcPlot %>% layout(hovermode = "x unified")
            output$ratingCurvePlotly <- renderPlotly({rcPlotly})
            
            # interactive gauge height by scrolling on on any of the two paired graphs 
            # Currently using GUIL survey data (hard coded) needs to be updated with dynamic query of site surveys
             dscSurvey <- read_csv("data/D04_GUIL_surveyPts_20200312.csv") %>% filter(mapCode == "Transect_DSC") %>% arrange(E)
             output$waterHeightInteractive <- renderPlotly({
               rowTest <- nrow(dscSurvey)
               hover <- event_data("plotly_hover", source = "rcHover_source")
               if(is.null(hover))
               {
                 if(exists("hover_point_pre"))
                 {
                   plot_ly(dscSurvey) %>%
                     add_lines(x = c(0, rowTest), y = as.numeric(hovered_point_pre+min(dscSurvey$H)),name="Estimated Gauge",type='scatter',mode='lines+maarkers',fill = 'tonexty',fillcolor='lightblue') %>%
                     add_trace(y = ~H, type = 'scatter', mode = 'lines+markers', fill = 'tozeroy', fillcolor = 'brown', name = "DSC Transect", text = ~name, hoverinfo = "text+y") %>%
                     layout(xaxis = list(range=c(0,as.numeric(rowTest))), yaxis = list(range = c(min(dscSurvey$H), max(dscSurvey$H)+2)), legend = list(x = 0.1, y = 0.9))
            
                 } 
                 # else {
                 #   plot_ly(dscSurvey) %>%
                 #     add_lines(x = c(0, rowTest), y = as.numeric(0.25+min(dscSurvey$H)),name="Estimated Gauge",type='scatter',mode='lines',fill = 'tonexty',fillcolor='lightblue') %>%
                 #     add_trace(y = ~H, type = 'scatter', mode = 'lines+markers', fill = 'tozeroy', fillcolor = 'brown', name = "DSC Transect", text = ~name, hoverinfo = "text+y") %>%
                 #     layout(xaxis = list(range=c(0,as.numeric(rowTest))), yaxis = list(range = c(min(dscSurvey$H), max(dscSurvey$H)+2)))
                 # }
            
               } else {
                 hovered_point <- hover$x[1]
                 hovered_point_pre <<- hovered_point
                 plot_ly(dscSurvey) %>%
                   add_lines(x = c(0, rowTest), y = as.numeric(hovered_point+min(dscSurvey$H)),name="Estimated Gauge",type='scatter',mode='lines+maarkers',fill = 'tonexty',fillcolor='lightblue') %>%
                   add_trace(y = ~H, type = 'scatter', mode = 'lines+markers', fill = 'tozeroy', fillcolor = 'brown', name = "DSC Transect", text = ~name, hoverinfo = "text+y") %>%
                   layout(xaxis = list(range=c(0,as.numeric(rowTest))), yaxis = list(range = c(min(dscSurvey$H), max(dscSurvey$H)+2)), legend = list(x = 0.1, y = 0.9))
            
               }


             })
            
          }
          else {
            shinyjs::hide("Title_CSH")
            shinyjs::show("Title_CWE")
            sd_discharge <- SharedData$new(waterElevationDF, group = "waterElevation")
            
              output$rtdvStageDischargePlotly <- renderPlotly(
                waterElevationPlot <- plot_ly(sd_discharge, x = ~Date, y = ~waterColumnHeight, type = 'scatter', mode = 'lines',fill = 'tozeroy', name = 'Water column height (m)') %>% 
                  layout(xaxis= list(title = "Date",autotick = T,nticks = 25, tickmode = "auto"), yaxis = list(title = 'Water column height (m)'))
              )
              }
        shinyjs::hide("singleOutputBox")
        shinyjs::show("calculatedStagePlot")
        return(dscSurvey)
      } else{ # Single input pressure from user on else
        shinyjs::show("singleOutputBox")
        shinyjs::hide("rtdvStageDischargePlotly")
        
        #shinyjs::show("EstimatedDischarge")        
        # Queries L0 data if it is chosen in the dataSource input
        dbquery <- paste0("SELECT * FROM rcdata WHERE \"curveID\"  = (
                            SELECT MAX(\"curveID\")
                            FROM rcdata WHERE \"curveID\" LIKE ", "'%",input$rtdvSite,"%')")
        rcdata <- DBI::dbSendQuery(con,dbquery)
        rcData <- DBI::dbFetch(rcdata)
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
          shinyjs::show("EstimatedDischarge")
          shinyjs::show("Title_CSH")
          shinyjs::hide("Title_CWE")
          regressionData <- formatReg(input, output, session)
          waterElevationDF <- applyRegtoL0(regressionData = regressionData, L0PressureData = waterElevationDF, site = site, startDate = startDate, endDate = endDate, session)
          for(i in 1:nrow(waterElevationDF))
          {
            estimatedDischarge = rcData$maxPostQ[which.min(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i]))]
            pramUTop = rcData$pramUTop[which.min(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i]))]
            pramUBottom = rcData$pramUBottom[which.min(abs(rcData$Hgrid-waterElevationDF$calculatedStage[i]))]
            waterElevationDF$estimatedDischarge[i] <- estimatedDischarge
            waterElevationDF$uncertaintyUp[i] <- pramUTop
            waterElevationDF$uncertaintyBottom[i] <- pramUBottom
          }
          output$singleWaterColumnHeight <- renderText({paste0("Calculated stage height: ",round(waterElevationDF$calculatedStage[1],3),"m")})
          output$EstimatedDischarge <- renderText({paste0("Estimated Discharge: ",round(waterElevationDF$estimatedDischarge[1],3),"lps")})
          
        } else {
          shinyjs::hide("EstimatedDischarge")
          shinyjs::show("Title_CWE")
          shinyjs::hide("Title_CSH")
          
          #waterElevationDF <- waterElevationDF %>% 
            #mutate(waterColumnHeight = waterColumnHeight+distanceToAdd)
          output$singleWaterColumnHeight <- renderText({paste0("Calculated water column height: ",round(waterElevationDF$waterColumnHeight[1],3),"m")})
        }
  
        
        shinyjs::show("singleWaterColumnHeight")

      } #End of ifelse statement
      #Sourced code from RC.plot.R
      
      updateProgressBar(session = session, id = "GaugeHeightLoadBar", value = 100, title = paste0("Finished applying relevant data to L0 pressure data for: ",input$site))
      shinyjs::hide("GaugeHeightLoadBar")
    }
}