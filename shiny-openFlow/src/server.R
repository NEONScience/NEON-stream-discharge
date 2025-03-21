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
  
  # SETTING AND FORMATTING FOR ENTIRE APP -- BEGIN ####
  
  #__Hide title ####
  shinyjs::hide("Title_CWE")

  #__Light/Dark Mode Switch (not functional) ####
  # shiny::observe(session$setCurrentTheme(
  #   if (base::isTRUE(input$dark_mode)) dark else light
  # ))
  
  # SETTING AND FORMATTING FOR ENTIRE APP -- END ####

  # TAB: TIMESERIES VIEWER -- BEGIN #####

  #__Set reactives ####
  plots <- shiny::reactiveValues()
  whichTab <- shiny::reactiveValues()

  #__Select site ID based on the domain ID chosen ####
  shiny::observe({x <- productList$siteID[productList$domain == input$domainId]
  shiny::updateSelectInput(session,"siteId",choices = unique(x))})
  
  #__Create site description output based on selected site ####
  shiny::observeEvent(input$siteId,{
    siteURL <- base::gsub("\\_inflow|\\_outflow","",base::paste0("https://www.neonscience.org/field-sites/",base::tolower(input$siteId)))
    domainURL <- base::paste0("https://www.neonscience.org/field-sites/about-field-sites")
    siteLink <- a("Click here", href=siteURL,target="_blank")
    domainLink <- a("Click here", href=domainURL,target="_blank")
    output$siteInfo <- shiny::renderUI({tagList("Site: ",base::gsub("\\_inflow|\\_outflow","",input$siteId), siteLink, "for site description",sep="\n")})
    output$domainInfo <- shiny::renderUI({tagList("Domain: ", domainLink, "for domain map and info",sep="\n")})
  })

  #__Click event to render PhenoCam image ####
  shiny::observeEvent(plotly::event_data(event = "plotly_click",source="timeseries_viewer"),{
    new_clickEvent <- plotly::event_data(event = "plotly_click",source="timeseries_viewer")
    if (!base::is.null(new_clickEvent)) {
      # phenoCamReactives$phenoURL <- NULL
      #formats date & time for phenocamGet
      dateTime <- stringr::str_replace(new_clickEvent$x, " ","T")
      dateTime <- base::paste0(dateTime,":00Z")
      #returns url for phenocam image
      phenoURL <- pheno.GET(dp.id="DP1.20002",
                            site.id=input$siteId,
                            domain.id=input$domainId,
                            date.time=dateTime)
      #formats date & time for bad request modal
      usrDateTime <- dateTime
      usrDateTime <- stringr::str_replace(usrDateTime, "T"," ")
      usrDateTime <- base::substr(usrDateTime,1,base::nchar(usrDateTime)-4)
      tookInfo <- ""
      #took handling
      if(input$siteId == "TOOK_inflow" || input$siteId == "TOOK_outflow"){
        tookInfo <- "Note: The phenocam image is NOT located at the inlet or outlet.
           The phenocam shows the main lake."
      }
      
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
  
  #__Get phenocam info for download handler ####
  phenoInfo <- NULL
  createPhenoInfo <- function(phenoURL,usrDateTime){
    usrDateTime <- stringr::str_replace(usrDateTime, " ","_")
    usrDateTime <- stringr::str_replace(usrDateTime, ":","-")
    usrDateTime <- base::paste0(usrDateTime,"-UTC")
    phenoInfo <- base::list("URL" = phenoURL, "dateTime" = usrDateTime)
    return(phenoInfo)
  }
  
  #__Download handler for downloading PhenoCam image ####
  output$downloadPheno <- shiny::downloadHandler(
    filename = function() {
      base::paste0("NEON.",input$domainId,".",input$siteId,".","DP1.20002","_",phenoInfo$dateTime,".jpg")
    },
    content = function(file) {
      utils::download.file(phenoInfo$URL,file,mode='wb')
    }
  )# End downloadHandler

  #__Observe event to generate outputs ####
  shiny::observeEvent(input$submit,{
    TS_siteID <- shiny::isolate(input$siteId)
    TS_startDate <- shiny::isolate(input$dateRange[[1]])
    TS_endDate <- shiny::isolate(input$dateRange[[2]])
    
    #__Generate metadata table ####
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
    # Create metadata table output
    output$table <- DT::renderDataTable({dat <- DT::datatable(metaD,  options = list(dom = 't'))},selection = 'single')
    
    #__Render continous discharge plot ####
    output$plot1 <- plotly::renderPlotly({
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
      # Plot continuous discharge and store in output
      plots$plot.cont.Q <- cont.Q.plot(site.id = TS_siteID,
                                       start.date = TS_startDate,
                                       end.date = TS_endDate,
                                       plot.imp.unit = impUnitInput,
                                       mode.dark = darkModeInput,
                                       plot.sci.rvw.QF = sciRvwQfInput,
                                       plot.q.stats = include.q.stats)
    })# End renderPlotly
    
    #__Render rating curve plot ####
    output$plot2 <- plotly::renderPlotly({
      #format flags
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
      # Plot rating curve(s) and store in outputs
      plots$plot.RC <- RC.plot(site.id = TS_siteID,
                               start.date = TS_startDate,
                               end.date = TS_endDate,
                               plot.imp.unit = impUnitInput,
                               mode.dark = darkModeInput)
    })# End renderPlotly
  },ignoreNULL = T)# End observeEvent

  #__Indentify which plot a user wishes to download ####
  shiny::observeEvent(input$selectedTab, {
    whichTab$currentTab = input$selectedTab
  })
  
  #__Download handler for downloading HTML plotly plots ####
  #sends the correct plot and data package name to download handler
  whichPlot <- function(){
    if(whichTab$currentTab == "Continuous Discharge"){
      downloadParam <- base::list("plotToWidget" = plots$plot.cont.Q, "dpName" = "DP4.00130")
    }
    else{
      downloadParam <- base::list("plotToWidget" = plots$plot.RC, "dpName" = "DP4.00133")
    }
    return(downloadParam)
  }
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
  
  # TAB: TIMESERIES VIEWER -- END #####
  
  # TAB: TARGET GAUGE HEIGHT -- BEGIN ####
  
  #__Select site ID based on the domain ID chosen ####
  shiny::observe({
    x<-Target_df$siteID[Target_df$domainID==input$XS_Domain]
    shiny::updateSelectInput(session, "XS_site", choices= unique(x))
  })# End observe
  
  #__Show results and render plots ####
  shiny::observeEvent(input$showResultsTargetGAG,{
    XS_site <- isolate(input$XS_site)
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Retreiving Target Gauge Heights & Building Plots",
      type = "info",
      btn_labels = NA, #prevents any buttons from appearing
      closeOnEsc = FALSE, #prevents closing the box with the Esc key
      closeOnClickOutside = FALSE # prevents the user from clicking outside the box
    )

    #__Pull in target gauge height information to render ####
    output$targetGaugeHeight <- shiny::renderText({paste0("Target Gauge Height (m): <b>",Target_df$targetGaugeHeight[Target_df$siteID==XS_site],"</b>")})
    output$highFlowPeriod <- shiny::renderText({paste0("Typical High Flow Period: <b>",Target_df$highFlowPeriod[Target_df$siteID==XS_site],"</b>")})
    output$targetGaugeHeightPlus10 <- shiny::renderText({paste0("Target + 10% (m): <b>",Target_df$targetGaugeHeightPlus10[Target_df$siteID==XS_site],"</b>")})
    output$targetGaugeHeightMinus30 <- shiny::renderText({paste0("Target - 30% (m): <b>",Target_df$targetGaugeHeightMinus30[Target_df$siteID==XS_site],"</b>")})
    
    #__Render rating curve with target range highlighted ####
    output$rc_targetGAG <- plotly::renderPlotly({
      # Build plot
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
    
    #__Render discharge cross-section with target range highlighted ####
    output$xs_targetGAG <- plotly::renderPlotly({
      # Build plot
      p <- XS.plot(site.id = XS_site,
                   target.gag.range = list(min=as.numeric(Target_df$targetGaugeHeightMinus30[Target_df$siteID==XS_site]),
                                           tar=as.numeric(Target_df$targetGaugeHeight[Target_df$siteID==XS_site]),
                                           max=as.numeric(Target_df$targetGaugeHeightPlus10[Target_df$siteID==XS_site])))
      p
    })
    
    shinyWidgets::closeSweetAlert(session = session)
  },ignoreNULL = T)# End observeEvent
  
  # TAB: TARGET GAUGE HEIGHT -- END ####
  
  # TAB: REAL-TIME DATA VIEWER -- BEGIN ####
  
  #__Set reactives ####
  realTimeReactives <- shiny::reactiveValues(estimatedStage=NA,
                                             estimatedDischarge=NA,
                                             totalUTop=NA,
                                             totalUBottom=NA,
                                             timeToProcess=NA,
                                             HgridMinMax=NA,
                                             distanceAdjMinMax=NA,
                                             stageMin=NA)
  
  #__Select site ID based on the domain ID chosen ####
  shiny::observe({x <- productList$siteID[productList$domain == input$rtdvDomain]
  shiny::updateSelectInput(session,"rtdvSite",choices = unique(x))})
  
  #__Show and hide data inputs based on the data type selected ####
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
  
  #__Process the data ####
  shiny::observeEvent(input$rtdvRun,{
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Converting Data & Rendering Outputs",
      type = "info",
      btn_labels = NA, #prevents any buttons from appearing
      closeOnEsc = FALSE, #prevents closing the box with the Esc key
      closeOnClickOutside = FALSE # prevents the user from clicking outside the box
    )
    
    #__Format inputs based on data type ####
    # Formatting for raw pressure data (formatting for raw staff gauge height will not come in until later)
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
    
    # Process pressure data through to stage (only for single pressure value or grafana csv upload)
    if(input$dataSource%in%c("Single Pressure Reading","Grafana CSV File")){
      #__Calibrate pressure data and convert to water column height ####
      # Build DPID
      L0DPNum <- ifelse(input$waterType=="Groundwater","20015",ifelse(input$trollType=="LevelTroll","20016","20054"))
      L0TermNum <- ifelse(input$waterType=="Groundwater","01376","01379")
      VER <- ifelse(input$waterType=="Groundwater","000","100")
      hormap <- DBI::dbReadTable(con,"hormap")
      HOR <- hormap$HOR[hormap$siteID==input$rtdvSite&hormap$endDate==max(hormap$endDate[hormap$siteID==input$rtdvSite])]
      DPID <- paste("NEON",input$rtdvDomain,input$rtdvSite,"DP0",L0DPNum,"001",L0TermNum,HOR,VER,"000",sep=".")
      startDate <- Sys.Date()
      endDate <- Sys.Date()
      # Attempt to pull calibration data
      calibrations <- try(openFlowInternal::get.cal(startDate = startDate,
                                                    endDate = endDate,
                                                    DPID = DPID,
                                                    session,
                                                    input,
                                                    output),silent = T)
      # If query was successful, get calibration coefficients
      if(attr(calibrations, "class") == "try-error"){
        # ADD ERROR HANDLING
      }else{
        calibration_info <- as.data.frame(rbind(calibrations$value[1:3]))
        colnames(calibration_info) <- c("calValCP0","calValCP1","calValCP2")
      }
      # Apply calibration coefficients
      calibratedPressure <- as.numeric(calibration_info$calValCP2)*pressureToProcess**2+as.numeric(calibration_info$calValCP1)*pressureToProcess+as.numeric(calibration_info$calValCP0)
      # Convert to water column height
      convKPatoPa <- 1000 #Pa per 1 kPa
      roe = 999 #kg/m^3 density of water
      grav = 9.80665 #m/s^2 gravity constant
      waterColumnHeight <- (calibratedPressure/(roe*grav)) * convKPatoPa
      
      #__Apply TROLL named location offsets to water column height data ####
      ## ZN NOTE 2025-01-04: currently, we are not doing this for provisional and unpublished data retrieved from the Pachyderm transition outputs. When this is reintroduced to the pipeline, I will add back into this code. If Lucas's Blue Heron code gets deleted, look back into the commit history to restore his code
      
      #__Convert water column height data to calculated stage ####
      # For surface water data only
      regressionData <- openFlowInternal::frmt.reg(input, output, session)
      regressionData <- regressionData[regressionData$regressionID==max(regressionData$regressionID[grepl(input$rtdvSite,regressionData$regressionID)]),]
      stageHeight <- (as.numeric(regressionData$regressionSlope)*waterColumnHeight)+as.numeric(regressionData$regressionIntercept)
    }else{
      timeToProcess <- NA
      stageHeight <- as.numeric(input$singleStaffGauge)
      
      # Get gauge named location offsets
      gaugeLocData <- openFlowInternal::gag.offset(input$rtdvSite,
                                                   session,
                                                   input,
                                                   output)
      #Error handling if no data or if GET failed
      if(attr(gaugeLocData, "class") == "try-error"){
        failureMessage <- "Gauge location history could not be retrieved"
        stop(failureMessage)
      }
      if (input$rtdvSite=="TOOK") {
        if(grepl("inflow",dischargeNamedLocation)){
          gaugeLocData <- gaugeLocData[grepl("inflow",gaugeLocData$namedLocation),]
        }else{
          if(grepl("outflow",dischargeNamedLocation)){
            gaugeLocData <- gaugeLocData[grepl("outflow",gaugeLocData$namedLocation),]
          }else{
            failureMessage <- "No inflow or outflow gauge location history records were retrieved for TOOK"
            stop(failureMessage)
          }
        }
      }
      if(length(gaugeLocData)<1){
        failureMessage <- "Zero (0) gauge location history records were retrieved"
        stop(failureMessage)
      }
      # Format the gauge named location data to apply offsets
      gaugeLocData <- try(suppressWarnings(openFlowInternal::frmt.gauge.name.loc.data(input$rtdvSite,
                                                                                      dataFrame = gaugeLocData)))
      
      # Apply gauge offset
      # Subset to the most recent named location
      gaugeLocData_nl <- gaugeLocData[gaugeLocData$namedLocation==gaugeLocData$namedLocation[gaugeLocData$endDate==max(gaugeLocData$endDate)],]
      # If there is only 1 entry, there is no offset, but if more, there is
      if(nrow(gaugeLocData_nl)>1){
        offset <- gaugeLocData_nl$refElevPlusZ[nrow(gaugeLocData_nl)] - gaugeLocData_nl$refElevPlusZ[1]
        stageHeight <- stageHeight+offset
      }
    }
    
    #__Convert stage to discharge using the most recently-published rating curve ####
    dbquery <- paste0("SELECT * FROM rcdata WHERE \"curveID\"  = (SELECT MAX(\"curveID\") FROM rcdata WHERE \"curveID\" LIKE ", "'%",input$rtdvSite,"%')")
    rcdata <- DBI::dbSendQuery(con,dbquery)
    rcData <- DBI::dbFetch(rcdata)
    
    estimatedDischarge <- NULL
    totalUTop <- NULL
    totalUBottom <- NULL
    for(i in 1:length(stageHeight)){
      estimatedDischarge <- c(estimatedDischarge,rcData$maxPostQ[which(abs(rcData$Hgrid-stageHeight[i])==min(abs(rcData$Hgrid-stageHeight[i])))])
      totalUTop = c(totalUTop,rcData$totalUTop[which(abs(rcData$Hgrid-stageHeight[i])==min(abs(rcData$Hgrid-stageHeight[i])))])
      totalUBottom = c(totalUBottom,rcData$totalUBottom[which(abs(rcData$Hgrid-stageHeight[i])==min(abs(rcData$Hgrid-stageHeight[i])))])
    }

    #__Set the data to reactive values ####
    realTimeReactives$estimatedStage <- stageHeight
    realTimeReactives$estimatedDischarge <- estimatedDischarge
    realTimeReactives$totalUTop <- totalUTop
    realTimeReactives$totalUBottom <- totalUBottom
    realTimeReactives$timeToProcess <- timeToProcess
    shinyWidgets::closeSweetAlert(session = session)
  },ignoreNULL = T)# End observeEvent
  
  #__Render UI for single pressure or staff gauge reading ####
  shiny::observe({
    if(all(!is.na(realTimeReactives$estimatedDischarge))&&length(realTimeReactives$estimatedDischarge)==1){
      # browser()
      #__Render rating curve with target range highlighted ####
      output$rc_rtdv <- plotly::renderPlotly({
        if(!input$rtdvSite%in%c("FLNT","BLWA","TOMB")){
          targets <- list(min=as.numeric(Target_df$targetGaugeHeightMinus30[Target_df$siteID==input$rtdvSite]),
                          tar=as.numeric(Target_df$targetGaugeHeight[Target_df$siteID==input$rtdvSite]),
                          max=as.numeric(Target_df$targetGaugeHeightPlus10[Target_df$siteID==input$rtdvSite]))
        }else{
          targets <- NULL
        }
        # Build plot
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
      })
      
      #__Render discharge cross-section with target range highlighted ####
      output$xs_rtdv <- plotly::renderPlotly({
        # Build plot
        p <- XS.plot(site.id = input$rtdvSite,
                     rtdv.values=realTimeReactives$estimatedStage)
        p
      })
      output$realTimeUIOutput <- shiny::renderUI({
        shinydashboard::box(width=12,
                            title = paste0("Results for ",input$dataSource," = ",ifelse(input$dataSource=="Single Staff Gauge Reading",input$singleStaffGauge,input$singlePressure)," at ",input$rtdvSite),
                            status = "primary", solidHeader = T,
                            tags$h3(shiny::HTML("Estimated Discharge = <b>",realTimeReactives$estimatedDischarge,"</b> L/s")),
                            tags$h3(shiny::HTML("Discharge Range w/ Total Uncertainty: <b>",realTimeReactives$totalUTop,"-",realTimeReactives$totalUBottom,"</b> L/s")),
                            shinydashboard::box(width=12,
                                                title = "Rating Curve & Cross Section Plots",
                                                status = "primary",solidHeader = T,
                                                plotly::plotlyOutput("rc_rtdv"),
                                                plotly::plotlyOutput("xs_rtdv")
                                                )# End shinydashboard box
                            )# End shinydashboard box
        })# End renderUI
    }
  })# End observe
  
  #__Render UI for single pressure or staff gauge reading ####
  shiny::observe({
    if(all(!is.na(realTimeReactives$estimatedDischarge))&&length(realTimeReactives$estimatedDischarge)>1){
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
      output$rc_timeseries_rtdv <- plotly::renderPlotly({
        if(!input$rtdvSite%in%c("FLNT","BLWA","TOMB")){
          targets <- list(min=as.numeric(Target_df$targetGaugeHeightMinus30[Target_df$siteID==input$rtdvSite]),
                          tar=as.numeric(Target_df$targetGaugeHeight[Target_df$siteID==input$rtdvSite]),
                          max=as.numeric(Target_df$targetGaugeHeightPlus10[Target_df$siteID==input$rtdvSite]))
        }else{
          targets <- NULL
        }
        # Build plot
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
      })
      #__Render discharge cross-section with target range highlighted ####
      output$xs_timeseries_rtdv <- plotly::renderPlotly({
        # Build plot
        p <- XS.plot(site.id = input$rtdvSite,
                     rtdv.values=realTimeReactives$estimatedStage[1],
                     p.source="xs_timeseries_rtdv")
        p_extract <- plotly::plotly_build(p)
        realTimeReactives$distanceAdjMinMax <- p_extract$x$data[[1]]$x
        realTimeReactives$stageMin <- min(p_extract$x$data[[2]]$y)
        p
      })
      # Observe hover events on the timeseries plot
      observeEvent(event_data("plotly_hover", source = "timeseries_rtdv"), {
        hover_data <- event_data("plotly_hover", source = "timeseries_rtdv")
        if (!is.null(hover_data)) {
          hover_value <- hover_data$y[1]
          
          # Update the scatter plot to move the dashed horizontal line
          plotlyProxy("rc_timeseries_rtdv", session) %>%
            plotlyProxyInvoke("deleteTraces", list(as.integer(11))) %>%
            plotlyProxyInvoke("addTraces", list(x = realTimeReactives$HgridMinMax,
                                                y = c(hover_value, hover_value),
                                                type = 'scatter',
                                                mode = 'lines',
                                                line = list(dash = 'dash', color = 'black')))
          
          # Update the xs plot as well
          # browser()
          # Convert hover discharge to stage
          hover_convertH <- realTimeReactives$estimatedStage[which(abs(realTimeReactives$estimatedDischarge-hover_value)==min(abs(realTimeReactives$estimatedDischarge-hover_value)))[1]]
          # Update plot
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
        
      })
      output$realTimeUIOutput <- shiny::renderUI({
        shinydashboard::box(width=12,
                            title = paste0("Results for Grafana CSV File"," at ",input$rtdvSite),
                            status = "primary", solidHeader = T,
                            plotly::plotlyOutput("timeseries_rtdv"),
                            shiny::fluidRow(shiny::column(width = 6,
                                                          plotly::plotlyOutput("rc_timeseries_rtdv")),
                                            shiny::column(width = 6,
                                                          plotly::plotlyOutput("xs_timeseries_rtdv")))
        )# End shinydashboard box
      })# End renderUI
    }
  })# End observe
  
  # TAB: REAL-TIME DATA VIEWER -- END ####

}#end of server