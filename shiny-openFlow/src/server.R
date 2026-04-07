##############################################################################################
#' @title 

#' @author
#' Zachary Nickerson \email{nickerson@battelleecology.org} \cr

#' @description 

#' @return 

# changelog and author contributions / copyrights
#   Zachary Nickerson (YYYY - MM - DD)
#     original creation
##############################################################################################
server <- function(input, output, session) {
  
  # Hide title ####
  shinyjs::hide("Title_CWE")

  # TIMESERIES VIEWER -- BEGIN #####

  # 1 Set reactives ####
  plots <- shiny::reactiveValues()
  whichTab <- shiny::reactiveValues()

  # 2 Select site ID based on the domain ID chosen ####
  shiny::observe({x <- productList$siteID[productList$domain == input$domainId]
  shiny::updateSelectInput(session,"siteId",choices = unique(x))})
  
  # 3 Create site description output based on selected site ####
  shiny::observeEvent(input$siteId,{
    siteURL <- base::gsub("\\_inflow|\\_outflow","",base::paste0("https://www.neonscience.org/field-sites/",base::tolower(input$siteId)))
    domainURL <- base::paste0("https://www.neonscience.org/field-sites/about-field-sites")
    siteLink <- a("Click here", href=siteURL,target="_blank")
    domainLink <- a("Click here", href=domainURL,target="_blank")
    output$siteInfo <- shiny::renderUI({tagList("Site: ",base::gsub("\\_inflow|\\_outflow","",input$siteId), siteLink, "for site description",sep="\n")})
    output$domainInfo <- shiny::renderUI({tagList("Domain: ", domainLink, "for domain map and info",sep="\n")})
  })

  # 4 Process inputs & generate outputs ####
  shiny::observeEvent(input$submit,{
    
    #. . 4.1 Isolate inputs ####
    TS_siteID <- shiny::isolate(input$siteId)
    TS_startDate <- shiny::isolate(input$dateRange[[1]])
    TS_endDate <- shiny::isolate(input$dateRange[[2]])
    
    #. . 4.2 Generate metadata table ####
    metaD <-  productList%>%
      dplyr::filter(siteID == TS_siteID)%>%
      dplyr::select(upstreamWatershedAreaKM2,reachSlopePercent,averageBankfullWidthM,d50ParticleSizeMM)%>%
      dplyr::rename("Upstream watershed area (km^2)"= upstreamWatershedAreaKM2,
                    "Reach slope (%)" = reachSlopePercent,
                    "Mean bankfull width (m)"= averageBankfullWidthM,
                    "D50 particle size (mm)"=d50ParticleSizeMM) %>%
      dplyr::mutate_all(as.character)%>%
      tidyr::pivot_longer(c("Upstream watershed area (km^2)","Reach slope (%)","Mean bankfull width (m)","D50 particle size (mm)"),
                          names_to = "MetaData",
                          values_to = "Values")
    output$table <- DT::renderDataTable({dat <- DT::datatable(metaD,  options = list(dom = 't'))},selection = 'single')
    
    #. . 4.3 Render continous discharge plot ####
    output$plot1 <- plotly::renderPlotly({
      
      #. . . 4.3.1 Change plot and app formatting based on user selection ####
      if(input$qctrFlagScRv == TRUE){
        sciRvwQfInput <- T
      }else{
        sciRvwQfInput <- F
      }
      if(input$dark_mode == TRUE){
        darkModeInput <- T
      }else{
        darkModeInput <- F
      }
      if(input$impUnitFlag == TRUE){
        impUnitInput <- T
      }else{
        impUnitInput <- F
      }
      
      #. . . 4.3.2 Run function to generate plot ####
      plots$plot.cont.Q <- cont.Q.plot(site.id = TS_siteID,
                                       start.date = TS_startDate,
                                       end.date = TS_endDate,
                                       plot.imp.unit = impUnitInput,
                                       mode.dark = darkModeInput,
                                       plot.sci.rvw.QF = sciRvwQfInput,
                                       plot.q.stats = include.q.stats)
    })# End renderPlotly
    
    #. . 4.4 Render rating curve plot ####
    output$plot2 <- plotly::renderPlotly({
      
      #. . . 4.4.1 Change plot and app formatting based on user selection ####
      if(input$dark_mode == TRUE){
        darkModeInput <- T
      }else{
        darkModeInput <- F
      }
      if(input$impUnitFlag == TRUE){
        impUnitInput <- T
      }else{
        impUnitInput <- F
      }
      
      #. . . 4.4.2 Run function to generate plot ####
      plots$plot.RC <- RC.plot(site.id = TS_siteID,
                               start.date = TS_startDate,
                               end.date = TS_endDate,
                               plot.imp.unit = impUnitInput,
                               mode.dark = darkModeInput)
    })# End renderPlotly
    
  },ignoreNULL = T)# End observeEvent

  # 5 Download handler for downloading HTML plotly plots ####

  #. . 5.1 Identify which plot a user wishes to download ####
  shiny::observeEvent(input$selectedTab, {
    whichTab$currentTab = input$selectedTab
  })
  
  #. . 5.2 Identify plot and data package name to download handler ####
  whichPlot <- function(){
    if(whichTab$currentTab == "Continuous Discharge"){
      downloadParam <- base::list("plotToWidget" = plots$plot.cont.Q, "dpName" = "DP4.00130")
    }
    else{
      downloadParam <- base::list("plotToWidget" = plots$plot.RC, "dpName" = "DP4.00133")
    }
    return(downloadParam)
  }
  
  #. . 5.3 Render download handler ####
  output$downloadPlotly <- shiny::downloadHandler(
    filename = function() {
      downloadParam <- whichPlot()
      #file name format NEON.DOMAIN.SITE.DP4.0013[0,3]_STARTDATE_ENDDATE.html
      base::paste0("NEON.",input$domainId,".",TS_siteID,".",downloadParam$dpName,"_",TS_startDate,"_",TS_endDate,".html")
    },
    content = function(file) {
      downloadParam <- whichPlot()
      shiny::withProgress(
        message = paste0("Downloading Plot to HTML file"),
        value = 0,
        {
          shiny::incProgress(1/10)
          base::Sys.sleep(1)
          shiny::incProgress(5/10)
          htmlwidgets::saveWidget(as_widget(plotly::partial_bundle(downloadParam$plotToWidget)), file, selfcontained = TRUE)
        }
      )
    }
  )# End downloadHandler
  
  # 6 Click event to render PhenoCam image ####
  shiny::observeEvent(plotly::event_data(event = "plotly_click",source="timeseries_viewer"),{
    new_clickEvent <- plotly::event_data(event = "plotly_click",source="timeseries_viewer")
    if (!base::is.null(new_clickEvent)) {
      #. . 6.1 Formats date & time for phenocamGet ####
      dateTime <- stringr::str_replace(new_clickEvent$x, " ","T")
      dateTime <- base::paste0(dateTime,":00Z")
      
      #. . 6.2 Returns url for phenocam image ####
      phenoURL <- pheno.GET(dp.id="DP1.20002",
                            site.id=input$siteId,
                            domain.id=input$domainId,
                            date.time=dateTime)
      
      #. . 6.3 Formats date & time for bad request modal ####
      usrDateTime <- dateTime
      usrDateTime <- stringr::str_replace(usrDateTime, "T"," ")
      usrDateTime <- base::substr(usrDateTime,1,base::nchar(usrDateTime)-4)
      tookInfo <- ""
      
      #. . 6.4 TOOK location handling ####
      if(input$siteId == "TOOK_inflow" || input$siteId == "TOOK_outflow"){
        tookInfo <- "Note: The phenocam image is NOT located at the inlet or outlet.
           The phenocam shows the main lake."
      }
      
      #. . 6.5 Render PhenoCam image in dialog box ####
      if(!base::is.null(phenoURL)){
        phenoInfo <<- createPhenoInfo(phenoURL,usrDateTime)
        shiny::showModal(shiny::modalDialog(
          title = "Phenocam Image",
          size = "l",
          tookInfo,
          tags$img(
            src = phenoURL),
          footer = shiny::downloadButton("downloadPheno",label = "Download Phenocam Image"),
          easyClose = TRUE))
      }
      else{
        shiny::showModal(shiny::modalDialog(
          title = "Phenocam Image",
          "No phenocam image available at ",input$siteId," for Date/Time",usrDateTime,
          size = "s",
          easyClose = TRUE))
      }
    }
  })# End observe
  
  # 7 Get PhenoCam info for download handler ####
  phenoInfo <- NULL
  createPhenoInfo <- function(phenoURL,usrDateTime){
    usrDateTime <- stringr::str_replace(usrDateTime, " ","_")
    usrDateTime <- stringr::str_replace(usrDateTime, ":","-")
    usrDateTime <- base::paste0(usrDateTime,"-UTC")
    phenoInfo <- base::list("URL" = phenoURL, "dateTime" = usrDateTime)
    return(phenoInfo)
  }
  
  # 8 Download handler for downloading PhenoCam image ####
  output$downloadPheno <- shiny::downloadHandler(
    filename = function() {
      base::paste0("NEON.",input$domainId,".",input$siteId,".","DP1.20002","_",phenoInfo$dateTime,".jpg")
    },
    content = function(file) {
      utils::download.file(phenoInfo$URL,file,mode='wb')
    }
  )# End downloadHandler
  
  # TIMESERIES VIEWER -- END #####
  
  # TARGET GAUGE HEIGHT -- BEGIN ####
  
  # 1 Select site ID based on the domain ID chosen ####
  shiny::observe({
    x<-Target_df$siteID[Target_df$domainID==input$XS_Domain]
    shiny::updateSelectInput(session, "XS_site", choices= unique(x))
  })# End observe
  
  # 2 Show results and render plots ####
  shiny::observeEvent(input$showResultsTargetGAG,{
    
    #. . 2.1 Isolate Inputs ####
    XS_site <- isolate(input$XS_site)

    #. . 2.2 Pull in target gauge height information to render ####
    output$targetGaugeHeight <- shiny::renderText({paste0("Target Gauge Height (m): <b>",Target_df$targetGaugeHeight[Target_df$siteID==XS_site],"</b>")})
    output$highFlowPeriod <- shiny::renderText({paste0("Typical High Flow Period: <b>",Target_df$highFlowPeriod[Target_df$siteID==XS_site],"</b>")})
    output$targetGaugeHeightPlus10 <- shiny::renderText({paste0("Target + 10% (m): <b>",Target_df$targetGaugeHeightPlus10[Target_df$siteID==XS_site],"</b>")})
    output$targetGaugeHeightMinus30 <- shiny::renderText({paste0("Target - 30% (m): <b>",Target_df$targetGaugeHeightMinus30[Target_df$siteID==XS_site],"</b>")})
    
    #. . 2.3 Render rating curve with target range highlighted ####
    output$rc_targetGAG <- plotly::renderPlotly({
      p <- RC.plot(site.id = XS_site,
                   start.date = Sys.Date(),
                   end.date = Sys.Date(),
                   plot.imp.unit = F,
                   mode.dark = F,
                   target.gag.range = list(min=as.numeric(Target_df$targetGaugeHeightMinus30[Target_df$siteID==XS_site]),
                                           tar=as.numeric(Target_df$targetGaugeHeight[Target_df$siteID==XS_site]),
                                           max=as.numeric(Target_df$targetGaugeHeightPlus10[Target_df$siteID==XS_site])),
                   show.legend = F,
                   uncertainty.visibility = T)
      p
    })
    
    #. . 2.4 Render discharge cross-section with target range highlighted ####
    output$xs_targetGAG <- plotly::renderPlotly({
      p <- XS.plot(site.id = XS_site,
                   target.gag.range = list(min=as.numeric(Target_df$targetGaugeHeightMinus30[Target_df$siteID==XS_site]),
                                           tar=as.numeric(Target_df$targetGaugeHeight[Target_df$siteID==XS_site]),
                                           max=as.numeric(Target_df$targetGaugeHeightPlus10[Target_df$siteID==XS_site])))
      p
    })
    
  },ignoreNULL = T)# End observeEvent
  
  # TARGET GAUGE HEIGHT -- END ####
  
  # REAL-TIME DATA VIEWER -- BEGIN ####
  
  # 1 Set reactives ####
  realTimeReactives <- shiny::reactiveValues(estimatedStage=NA,
                                             estimatedDischarge=NA,
                                             totalUTop=NA,
                                             totalUBottom=NA,
                                             timeToProcess=NA,
                                             HgridMinMax=NA,
                                             distanceAdjMinMax=NA,
                                             stageMin=NA)
  
  # 2 Select site ID based on the domain ID chosen ####
  shiny::observe({x <- productList$siteID[productList$domain == input$rtdvDomain]
  shiny::updateSelectInput(session,"rtdvSite",choices = unique(x))})
  
  # 3 Show and hide data inputs based on the data type selected ####
  shiny::observe({
    if(input$dataSource=="Single Staff Gauge Reading"){
      shinyjs::show("singleStaffGauge")
      shinyjs::hide("singlePressure")
      shinyjs::hide("grafanaFile")
    }
    if(input$dataSource=="Single Pressure Reading"){
      shinyjs::hide("singleStaffGauge")
      shinyjs::show("singlePressure")
      shinyjs::hide("grafanaFile")
    }
    if(input$dataSource=="Grafana CSV File"){
      shinyjs::hide("singleStaffGauge")
      shinyjs::hide("singlePressure")
      shinyjs::show("grafanaFile")
    }
  })# End observe
  
  # 4 Process inputs & generate outputs ####
  shiny::observeEvent(input$rtdvRun,{
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Converting Data & Rendering Outputs",
      type = "info",
      btn_labels = NA, #prevents any buttons from appearing
      closeOnEsc = FALSE, #prevents closing the box with the Esc key
      closeOnClickOutside = FALSE # prevents the user from clicking outside the box
    )
    
    #. . 4.1 Format inputs based on data type ####
    if(input$dataSource=="Single Pressure Reading"){
      pressureToProcess <- as.numeric(input$singlePressure)
      timeToProcess <- NA
    }else{
      if(input$dataSource=="Grafana CSV File"){
        grafanaFile <- read.csv(input$grafanaFile$datapath)
        pressureToProcess <- as.numeric(gsub(" kPa","",grafanaFile[,2]))
        timeToProcess <- as.POSIXct(grafanaFile[,1],tz="UTC",format="%Y-%m-%d %H:%M:%S")
      }
    }
    
    #. . 4.2 Process pressure data through to stage ####
    if(input$dataSource%in%c("Single Pressure Reading","Grafana CSV File")){
      
      #. . . 4.2.1 Calibrate pressure data and convert to water column height ####
      calibrations <- DBI::dbReadTable(con,"calibration")
      calibrations <- calibrations[calibrations$siteid==input$rtdvSite,]
      
      #. . . 4.2.3 If query was successful, get calibration coefficients ####
      if(nrow(calibrations)!=1){
        shinyWidgets::closeSweetAlert(session = session)
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "No Calibration Files Available",
          text = "Unable to pull current calibration files for these inputs. Please check inputs and try again. If the problem persists, contact the app author.",
          type = "error",
          btn_labels = "OK"
        )
        shiny::validate(shiny::need(!isTRUE(attr(calibrations, "class") == "try-error"), label = "Could not pull current calibration files"))
      }
      
      #. . . 4.2.4 Apply calibration coefficients ####
      calibratedPressure <- as.numeric(calibrations$calvalcp2)*pressureToProcess**2+as.numeric(calibrations$calvalcp1)*pressureToProcess+as.numeric(calibrations$calvalcp0)
      
      #. . . 4.2.5 Convert to water column height ####
      convKPatoPa <- 1000 #Pa per 1 kPa
      roe = 999 #kg/m^3 density of water
      grav = 9.80665 #m/s^2 gravity constant
      waterColumnHeight <- (calibratedPressure/(roe*grav)) * convKPatoPa
      
      #. . . 4.2.6 Convert water column height data to calculated stage ####
      regressionData <- DBI::dbReadTable(con,"regression")
      regressionData <- regressionData[regressionData$regressionid==max(regressionData$regressionid[grepl(input$rtdvSite,regressionData$regressionid)]),]
      stageHeight <- (as.numeric(regressionData$regressionslope)*waterColumnHeight)+as.numeric(regressionData$regressionintercept)
      
    }else{
      #. . 4.3 Process gauge heights through to stage ####
      timeToProcess <- NA
      stageHeight <- as.numeric(input$singleStaffGauge)
      
      #. . . 4.3.1 Get gauge named location offsets ####
      gaugeLocData <- DBI::dbReadTable(con,"hormap")
      gaugeLocData <- gaugeLocData[gaugeLocData$siteID==input$rtdvSite,]
      
      #. . . 4.3.2 Error handling if no data or if GET failed ####
      if(nrow(gaugeLocData)<1){
        failureMessage <- "Zero (0) gauge location history records were retrieved"
        stop(failureMessage)
      }
      
      #. . . 4.3.5 Apply gauge offset ####
      gaugeLocData_nl <- gaugeLocData[gaugeLocData$namedlocation==gaugeLocData$namedlocation[gaugeLocData$endDate==max(gaugeLocData$endDate)],]
      if(nrow(gaugeLocData_nl)>1){
        offset <- gaugeLocData_nl$refelevplusz[nrow(gaugeLocData_nl)] - gaugeLocData_nl$refelevplusz[1]
        stageHeight <- stageHeight+offset
      }
    }
    
    # 5 Convert stage to discharge using the most recently-published rating curve ####
    siteID_rc <- input$rtdvSite
    
    #. . 5.1 TOOK location handling ####
    if(siteID_rc=="TOOK_inflow"){
      siteID_rc <- "TKIN"
    }
    if(siteID_rc=="TOOK_outflow"){
      siteID_rc <- "TKOT"
    }
    
    #. . 5.2 Query posterior rating curve data from the openflow DB ####
    dbquery <- paste0("SELECT * FROM rcdata WHERE \"curveID\"  = (SELECT MAX(\"curveID\") FROM rcdata WHERE \"curveID\" LIKE ", "'%",siteID_rc,"%')")
    rcdata <- DBI::dbSendQuery(con,dbquery)
    rcData <- DBI::dbFetch(rcdata)
    
    #. . 5.3 Estimate discharge directly from rating curve ####
    estimatedDischarge <- NULL
    totalUTop <- NULL
    totalUBottom <- NULL
    for(i in 1:length(stageHeight)){
      estimatedDischarge <- c(estimatedDischarge,rcData$maxPostQ[which(abs(rcData$Hgrid-stageHeight[i])==min(abs(rcData$Hgrid-stageHeight[i])))])
      totalUTop = c(totalUTop,rcData$totalUTop[which(abs(rcData$Hgrid-stageHeight[i])==min(abs(rcData$Hgrid-stageHeight[i])))])
      totalUBottom = c(totalUBottom,rcData$totalUBottom[which(abs(rcData$Hgrid-stageHeight[i])==min(abs(rcData$Hgrid-stageHeight[i])))])
    }

    # 6 Set the data to reactive values ####
    realTimeReactives$estimatedStage <- stageHeight
    realTimeReactives$estimatedDischarge <- estimatedDischarge
    realTimeReactives$totalUTop <- totalUTop
    realTimeReactives$totalUBottom <- totalUBottom
    realTimeReactives$timeToProcess <- timeToProcess
    shinyWidgets::closeSweetAlert(session = session)
    
  },ignoreNULL = T)# End observeEvent
  
  # 7 Render UI for single pressure or staff gauge reading (real-time value) ####
  shiny::observe({
    if(all(!is.na(realTimeReactives$estimatedDischarge))&&length(realTimeReactives$estimatedDischarge)==1){
      
      #. . 7.1 Render rating curve with target range highlighted ####
      output$rc_rtdv <- plotly::renderPlotly({
        
        #. . . 7.1.1 Build in target gauge heights for the rating curve plots ####
        if(!input$rtdvSite%in%c("FLNT","BLWA","TOMB")){
          targets <- list(min=as.numeric(Target_df$targetGaugeHeightMinus30[Target_df$siteID==input$rtdvSite]),
                          tar=as.numeric(Target_df$targetGaugeHeight[Target_df$siteID==input$rtdvSite]),
                          max=as.numeric(Target_df$targetGaugeHeightPlus10[Target_df$siteID==input$rtdvSite]))
        }else{
          targets <- NULL
        }
        
        #. . . 7.1.2 Render rating curve plot ####
        p <- RC.plot(site.id = input$rtdvSite,
                     start.date = Sys.Date(),
                     end.date = Sys.Date(),
                     plot.imp.unit = F,
                     mode.dark = F,
                     target.gag.range = targets,
                     med.3x = productList$threeXMedQPlusUnc[productList$siteID==input$rtdvSite],
                     rtdv.values = realTimeReactives$estimatedDischarge,
                     show.legend = F,
                     uncertainty.visibility = T)
        p
      })# End renderPlotly
      
      #. . 7.2 Render discharge cross-section with target range highlighted ####
      output$xs_rtdv <- plotly::renderPlotly({
        p <- XS.plot(site.id = input$rtdvSite,
                     rtdv.values=realTimeReactives$estimatedStage)
        p
      })# End renderPlotly
      
      #. . 7.3 Render UI as a shinydashboard box ####
      output$realTimeUIOutput <- shiny::renderUI({
        shinydashboard::box(width=12,
                            
                            #. . . 7.3.1 Render title and high-level info ####
                            title = paste0("Results for ",input$dataSource," = ",ifelse(input$dataSource=="Single Staff Gauge Reading",input$singleStaffGauge,input$singlePressure)," at ",input$rtdvSite),
                            status = "primary", solidHeader = T,
                            tags$h3(shiny::HTML("Estimated Discharge = <b>",round(as.numeric(realTimeReactives$estimatedDischarge),digits = 0),"</b> L/s")),
                            tags$h3(shiny::HTML("Discharge Range w/ Total Uncertainty: <b>",round(as.numeric(realTimeReactives$totalUTop),digits = 0),"-",round(as.numeric(realTimeReactives$totalUBottom),digits = 0),"</b> L/s")),
                            
                            #. . . 7.3.2 Render box with plots ####
                            shinydashboard::box(width=12,
                                                title = "Rating Curve & Cross Section Plots",
                                                status = "primary",solidHeader = T,
                                                plotly::plotlyOutput("rc_rtdv"),
                                                plotly::plotlyOutput("xs_rtdv")
                                                ),# End shinydashboard box
                            
                            #. . . 7.3.3 Render box with interpretation guide ####
                            shinydashboard::box(
                              width = 12,
                              title= "Click To View Guide for Interpreting the 'Real-Time Data Viewer' Outputs for A Single Reading Input",
                              solidHeader= F,
                              status= "primary",
                              collapsible = T,
                              collapsed = T,
                              tags$h4(shiny::HTML('<b>Reported Values: </b>')),
                              tags$p(shiny::HTML("If the user provides a <b>Single Staff Gauge Reading</b>, the raw gauge height is offset according to the current named location data and coverted directly to estimated discharge")),
                              tags$p(shiny::HTML("If the user provides a <b>Single Pressure Reading</b>, the raw pressure is first calibrated using the most recent calibration files, converted to water column height using a constant equation, converted to estimated stage using the most recent gauge~water column height regression, and converted to estimated discharge.")),
                              tags$h4(shiny::HTML('<b>The Visuals: </b>')),
                              tags$p(shiny::HTML("<b>Rating Curve Plot</b> - The upper plot shows the most recently-published stage discharge rating curve with the following additions:
                                                 <li>The estimated output discharge plotted as a horizontal <b>dashed black line</b></li>
                                                 <li>The current 3x median discharge value plotted as a horizontal <b><span style=\"color: red;\">dashed red line</span></b></li>
                                                 <li>The target gauge range (See the 'Target Gauge Height' tab) higlighted in <b><span style=\"color: darkgreen;\">green</span></b> showing (from lower to higher stage) the -30% threshold, the target gauge height, and the +10% threshold</li>")),
                              tags$p(shiny::HTML("<b>Discharge Cross Section Plot</b> - The lower plot shows the most recently-surveyed discharge cross-section. The cross section is 'filled' to the level of the estimated discharge output."))
                            )# End shinydashboard  box
                            )# End shinydashboard box
        })# End renderUI
    }
  })# End observe
  
  # 8 Render UI for spreadsheet upload from Grafana (real-time timeseries) ####
  shiny::observe({
    if(all(!is.na(realTimeReactives$estimatedDischarge))&&length(realTimeReactives$estimatedDischarge)>1){
      
      #. . 8.1 Render timeseries plot ####
      output$timeseries_rtdv <- plotly::renderPlotly({
        p <- plotly::plot_ly(source="timeseries_rtdv")%>%
          plotly::add_trace(x=~realTimeReactives$timeToProcess,y=~as.numeric(realTimeReactives$totalUTop),name="Total Discharge\nUncertainty",showlegend=F,legendgroup="unc",type='scatter',mode='line',line=base::list(color='#D55E00'))%>%
          plotly::add_trace(x=~realTimeReactives$timeToProcess,y=~as.numeric(realTimeReactives$totalUBottom),name="Total Discharge\nUncertainty",showlegend=T,legendgroup="unc",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#D55E00') %>%
          plotly::add_trace(x=~realTimeReactives$timeToProcess,y=~as.numeric(realTimeReactives$estimatedDischarge),name="Estimated Discharge",type='scatter',mode='lines',line=list(color="black")) %>%
          #plotly::add_trace(x=~Date,y=~as.numeric(calculatedStage),name="Estimated Gauge",type='scatter',mode='lines',line=list(color="#F0E442"), yaxis = "y2") %>%
          layout(yaxis = list(title = "Estimated Discharge (L/s)"), 
                 xaxis = list(title = "Date"),
                 hovermode = "x unified", 
                 title = paste("Estimated Discharge for",input$rtdvSite))
        p
      })# End render plotly
      
      #. . 8.2 Render rating curve with target range highlighted ####
      output$rc_timeseries_rtdv <- plotly::renderPlotly({
        
        #. . . 8.2.1 Build in target gauge heights for the rating curve plots ####
        if(!input$rtdvSite%in%c("FLNT","BLWA","TOMB")){
          targets <- list(min=as.numeric(Target_df$targetGaugeHeightMinus30[Target_df$siteID==input$rtdvSite]),
                          tar=as.numeric(Target_df$targetGaugeHeight[Target_df$siteID==input$rtdvSite]),
                          max=as.numeric(Target_df$targetGaugeHeightPlus10[Target_df$siteID==input$rtdvSite]))
        }else{
          targets <- NULL
        }
        
        #. . . 8.2.2 Render rating curve plot ####
        p <- RC.plot(site.id = input$rtdvSite,
                     start.date = Sys.Date(),
                     end.date = Sys.Date(),
                     plot.imp.unit = F,
                     mode.dark = F,
                     target.gag.range = targets,
                     med.3x = productList$threeXMedQPlusUnc[productList$siteID==input$rtdvSite],
                     rtdv.values = realTimeReactives$estimatedDischarge[1],
                     show.legend = F,
                     uncertainty.visibility = T,
                     p.source="rc_timeseries_rtdv")
        p_extract <- plotly::plotly_build(p)
        realTimeReactives$HgridMinMax <- c(min(p_extract$x$data[[1]]$x),
                                           max(p_extract$x$data[[1]]$x))
        p
      })# End renderPlotly
      
      #. . 8.3 Render discharge cross-section with target range highlighted ####
      output$xs_timeseries_rtdv <- plotly::renderPlotly({
        p <- XS.plot(site.id = input$rtdvSite,
                     rtdv.values=realTimeReactives$estimatedStage[1],
                     p.source="xs_timeseries_rtdv")
        p_extract <- plotly::plotly_build(p)
        realTimeReactives$distanceAdjMinMax <- p_extract$x$data[[1]]$x
        realTimeReactives$stageMin <- min(p_extract$x$data[[2]]$y)
        p
      })# End renderPlotly
      
      #. . 8.4 Observe hover events on the timeseries plot ####
      observeEvent(event_data("plotly_hover", source = "timeseries_rtdv"), {
        
        #. . . 8.4.1 Definte hover info ####
        hover_data <- event_data("plotly_hover", source = "timeseries_rtdv")
        if (!is.null(hover_data)) {
          hover_value <- hover_data$y[1]
          
          #. . . 8.4.2 Update the rating curve plot to move the dashed horizontal line ####
          plotlyProxy("rc_timeseries_rtdv", session) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(11))) %>%
            plotlyProxyInvoke("addTraces", list(x = realTimeReactives$HgridMinMax,
                                                y = c(hover_value, hover_value),
                                                type = 'scatter',
                                                mode = 'lines',
                                                line = list(dash = 'dash', color = 'black')))
          
          #. . . 8.4.3 Convert hover discharge to stage ####
          hover_convertH <- realTimeReactives$estimatedStage[which(abs(realTimeReactives$estimatedDischarge-hover_value)==min(abs(realTimeReactives$estimatedDischarge-hover_value)))[1]]
          
          #. . . 8.4.4 Update the cross-section plot to move the water level line ####
          plotlyProxy("xs_timeseries_rtdv", session) %>%
            plotlyProxyInvoke("deleteTraces",
                              list(as.integer(0)))%>%
            plotlyProxyInvoke("addTraces",
                              list(x = realTimeReactives$distanceAdjMinMax,
                                   y = rep(hover_convertH,length(realTimeReactives$distanceAdjMinMax)),
                                   type='scatter',mode='line',line=list(color='lightblue'),hoverinfo='none',showlegend=F),
                              list(as.integer(0)))%>%
            plotlyProxyInvoke("deleteTraces",
                              list(as.integer(1)))%>%
            plotlyProxyInvoke("addTraces",
                              list(x = realTimeReactives$distanceAdjMinMax,
                                   y = rep(realTimeReactives$stageMin,length(realTimeReactives$distanceAdjMinMax)),
                                   type='scatter',mode='line',fill='tonexty',fillcolor='lightblue',line=list(color='lightblue'),hoverinfo='none',showlegend=F),
                              list(as.integer(1)))
            
        }
      })# End observeEvent
      
      #. . 8.5 Render UI as a shinydashboard box ####
      output$realTimeUIOutput <- shiny::renderUI({
        shinydashboard::box(width=12,
                            
                            #. . . 8.5.1 Render title and high-level info ####
                            title = paste0("Results for Grafana CSV File"," at ",input$rtdvSite),
                            status = "primary", solidHeader = T,
                            
                            #. . . 8.5.2 Render timeseries plot ####
                            plotly::plotlyOutput("timeseries_rtdv"),
                            
                            #. . . 8.5.3 Render rating curve and cross section plots ####
                            shiny::fluidRow(shiny::column(width = 6,
                                                          plotly::plotlyOutput("rc_timeseries_rtdv")),
                                            shiny::column(width = 6,
                                                          plotly::plotlyOutput("xs_timeseries_rtdv"))),
                            tags$br(),
                            
                            #. . . 8.5.4 Render box with interpretation guide ####
                            shinydashboard::box(
                              width = 12,
                              title= "Click To View Guide for Interpreting the 'Real-Time Data Viewer' Outputs for A Grafana CSV Input",
                              solidHeader= F,
                              status= "primary",
                              collapsible = T,
                              collapsed = T,
                              tags$h4(shiny::HTML('<b>Reported Values: </b>')),
                              tags$p(shiny::HTML("If the user provides a <b>Grafana CSV File</b>, the raw pressure is first calibrated using the most recent calibration files, converted to water column height using a constant equation, converted to estimated stage using the most recent gauge~water column height regression, and converted to estimated discharge.")),
                              tags$h4(shiny::HTML('<b>The Visuals: </b>')),
                              tags$p(shiny::HTML("<b>Discharge Timeseries Plot</b> - The upper plot shows a timeseries of the estimated output discharge (w/ upper and lower total uncertainty) from the Grafana CSV upload.")),
                              tags$p(shiny::HTML("<b>Rating Curve Plot</b> - The lower lefthand plot shows the most recently-published stage discharge rating curve with the following additions:
                                                 <li>The estimated output discharge plotted as a horizontal <b>dashed black line</b></li>
                                                 <li>The current 3x median discharge value plotted as a horizontal <b><span style=\"color: red;\">dashed red line</span></b></li>
                                                 <li>The target gauge range (See the 'Target Gauge Height' tab) higlighted in <b><span style=\"color: darkgreen;\">green</span></b> showing (from lower to higher stage) the -30% threshold, the target gauge height, and the +10% threshold</li>")),
                              tags$p(shiny::HTML("<b>Discharge Cross Section Plot</b> - The lower righthand plot shows the most recently-surveyed discharge cross-section. The cross section is 'filled' to the level of the estimated discharge output.")),
                              tags$h4(shiny::HTML('<b>Linked Plots: </b>')),
                              tags$p(shiny::HTML("When hovering on the timeseries plot along the x-axis, the two lower plots will change the highlighted output discharge estimate as the position on the timeseries x-axis changes.
                                                 In the rating curve plot, the black dashed line will move up and down the y-axis (discharge) as the users hovers along the timeseries.
                                                 Equivalently, the cross-section plot will 'fill' and 'empty' as the user hovers along the timeseries."))
                            )# End shinydashboard box
        )# End shinydashboard box
      })# End renderUI
    }
  })# End observe
  
  # REAL-TIME DATA VIEWER -- END ####

}#end of server