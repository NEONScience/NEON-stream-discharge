# Define server logic
server <- function(session, input, output) {
  
  # Select site ID based on the domain ID chosen
  shiny::observe({x <- productList$siteID[productList$domain == input$domainId]
  shiny::updateSelectInput(session,"siteId",choices = unique(x))})
  
  #handles light and dark mode switch
  shiny::observe(session$setCurrentTheme(
    if (base::isTRUE(input$dark_mode)) dark else light
  ))
  
  # Set date variables for app running (special consideration for TOOK)
  siteID <<- shiny::reactive(input$siteId)
  domainID <<- shiny::reactive(input$domainId)
  startDate <- shiny::reactive(base::format(input$dateRange[1]))
  endDate <- shiny::reactive(base::format(input$dateRange[2]))

  #phenoImage observe
  #displays phenocam image when point is clicked on graph
  #pulls image closest to selected date
  shiny::observe({
    new_clickEvent <- plotly::event_data(event = "plotly_click", source = "phenoDate")

    if (!base::is.null(new_clickEvent)) {
      #formats date & time for phenocamGet
      dateTime <- stringr::str_replace(new_clickEvent$x, " ","T")
      dateTime <- base::paste0(dateTime,":00Z")
      #returns url for phenocam image
      phenoURL <- pheno.GET(
        dp.id="DP1.20002",
        site.id=siteID,
        domain.id=domainID,
        date.time=dateTime)
      #formats date & time for bad request modal
      usrDateTime <- dateTime
      usrDateTime <- stringr::str_replace(usrDateTime, "T"," ")
      usrDateTime <- base::substr(usrDateTime,1,base::nchar(usrDateTime)-4)

      tookInfo <- ""
      #took handling
      if(siteID == "TOOK_inflow" || siteID == "TOOK_outflow"){
        tookInfo <- "Note: The phenocam image is NOT located at the inlet or outlet.
           The phenocam shows the main lake."
      }

      if(!base::is.null(phenoURL)){
        phenoInfo <<- createPhenoInfo(phenoURL,usrDateTime)
        shiny::showModal(shiny::modalDialog(
          title = "Phenocam Image",
          size = "l",
          tookInfo,
          tags$img(src = phenoURL),
          footer = shiny::downloadButton("downloadPheno",label = "Download Phenocam Image"),
          easyClose = TRUE))
      }
      else{
        shiny::showModal(shiny::modalDialog(
          title = "Phenocam Image",
          "No phenocam image available at ",siteID," for Date/Time",usrDateTime,
          size = "s",
          easyClose = TRUE))
      }
    }
  })
  
  output$downloadPheno <- shiny::downloadHandler(
    filename = function() {
      base::paste0("NEON.",domainID,".",siteID,".","DP1.20002","_",phenoInfo$dateTime,".jpg")
    },
    content = function(file) {
      utils::download.file(phenoInfo$URL,file,mode='wb')
    }
  )
  
  #gets phenocam info for download handler
  phenoInfo <- NULL
  createPhenoInfo <- function(phenoURL,usrDateTime){
    usrDateTime <- stringr::str_replace(usrDateTime, " ","_")
    usrDateTime <- stringr::str_replace(usrDateTime, ":","-")
    usrDateTime <- base::paste0(usrDateTime,"-UTC")
    phenoInfo <- base::list("URL" = phenoURL, "dateTime" = usrDateTime)
    return(phenoInfo)
  }
  
  shiny::observeEvent(input$siteId,{
    # Create site description output
    siteURL <- base::gsub("\\_inflow|\\_outflow","",base::paste0("https://www.neonscience.org/field-sites/",base::tolower(input$siteId)))
    domainURL <- base::paste0("https://www.neonscience.org/field-sites/about-field-sites")
    siteLink <- a("Click here", href=siteURL,target="_blank")
    domainLink <- a("Click here", href=domainURL,target="_blank")
    output$siteInfo <- shiny::renderUI({tagList("Site: ",base::gsub("\\_inflow|\\_outflow","",input$siteId), siteLink, "for site description",sep="\n")})
    output$domainInfo <- shiny::renderUI({tagList("Domain: ", domainLink, "for domain map and info",sep="\n")})
  })
  
  dates <- eventReactive(input$submit, {
    input$dateRange
  })
  
  # Download data, create summary table, and save output
  getPackage <- shiny::eventReactive(input$submit,{
    
    metaD <-  productList%>%
      dplyr::filter(siteID == input$siteId)%>%
      dplyr::select(upstreamWatershedAreaKM2,reachSlopePercent,averageBankfullWidthM,d50ParticleSizeMM)%>%
      dplyr::rename("Upstream watershed area (km^2)"= upstreamWatershedAreaKM2,
                    "Reach slope (%)" = reachSlopePercent,
                    "Mean bankfull width (m)"= averageBankfullWidthM,
                    "D50 particle size (mm)"=d50ParticleSizeMM) %>%
      dplyr::mutate_all(as.character)%>%
      tidyr::pivot_longer(c("Upstream watershed area (km^2)","Reach slope (%)","Mean bankfull width (m)","D50 particle size (mm)"),
                          names_to = "MetaData",
                          values_to = "Values")
    
    # Enter header for metadata table
    output$title <- shiny::renderText("Metadata Table")
    
    # Create metadata table output
    output$table <- DT::renderDataTable({dat <- DT::datatable(metaD,  options = list(dom = 't'))},selection = 'single')
    
    # Create site description output
    siteURL <- base::gsub("\\_inflow|\\_outflow","",base::paste0("https://www.neonscience.org/field-sites/",base::tolower(input$siteId)))
    url <- a("Click here", href=siteURL,target="_blank",style="text-decoration: none; hover:{font-size:150%;}")
    output$siteInfo <- shiny::renderUI({tagList("Site: ",base::gsub("\\_inflow|\\_outflow","",input$siteId), url, "for site description",sep="\n")})
    
    # Set date variables for app running (special consideration for TOOK)
    dateRange <- dates()
    siteID <<- input$siteId
    domainID <<- input$domainId
    startDate <- dateRange[[1]]
    endDate <- dateRange[[2]]
    # siteID <<- "LEWI"
    # domainID <<- "D02"
    # startDate <- "2022-10-01"
    # endDate <- "2022-10-31"

    # Code to stop the function if the app is on the external server and a user has selected a date range > 90 days
    if(constrain.dates&base::difftime(endDate,startDate,units="days")>90){
      shinyalert::shinyalert("Sorry! We are still in development...","At this time, the app cannot support downloads > 90 days. Please select a smaller date range.",type="error")
      stop("Requested time period must be no more than 90 days")
    }
    
    #progress bar for data downloads
    shiny::withProgress(message = 'Submit',detail = '', min = 0, max = 1 ,value = 0, {
      
      # Wrangle continuous discharge data
      shiny::incProgress(amount = 0.2,
                         message = "Wrangling continuous discharge data",
                         
                         detail = NULL,
                         session = shiny::getDefaultReactiveDomain())
      base::Sys.sleep(0.25)
      continuousDischarge_list$csd_continuousDischarge_allYears <- continuousDischarge_list$csd_continuousDischarge_allYears[continuousDischarge_list$csd_continuousDischarge_allYears$siteID==siteID
                                                                                                                             &continuousDischarge_list$csd_continuousDischarge_allYears$date>=startDate
                                                                                                                             &continuousDischarge_list$csd_continuousDischarge_allYears$date<base::as.Date(endDate)+1,]
      if(base::nrow(continuousDischarge_list$csd_continuousDischarge_allYears)==0){
        stop("There is no continuous discharge data published for this time range. Please modify selected dates.")
      }
      
      # Wrangle discrete discharge data
      shiny::incProgress(amount = 0.4,
                         message = "Wrangling discrete discharge data",
                         
                         detail = NULL,
                         session = shiny::getDefaultReactiveDomain())
      base::Sys.sleep(0.25)
      continuousDischarge_list$sdrc_gaugePressureRelationship_allYears <- continuousDischarge_list$sdrc_gaugePressureRelationship_allYears[continuousDischarge_list$sdrc_gaugePressureRelationship_allYears$siteID==siteID
                                                                                                                                           &continuousDischarge_list$sdrc_gaugePressureRelationship_allYears$date>=startDate
                                                                                                                                           &continuousDischarge_list$sdrc_gaugePressureRelationship_allYears$date<base::as.Date(endDate)+1,]
      continuousDischarge_list$sdrc_gaugeDischargeMeas_allYears <- continuousDischarge_list$sdrc_gaugeDischargeMeas_allYears[continuousDischarge_list$sdrc_gaugeDischargeMeas_allYears$siteID==siteID
                                                                                                                             &continuousDischarge_list$sdrc_gaugeDischargeMeas_allYears$date>=startDate
                                                                                                                             &continuousDischarge_list$sdrc_gaugeDischargeMeas_allYears$date<base::as.Date(endDate)+1,]
      
      # Wrangle precipitation data
      shiny::incProgress(amount = 0.6,
                         message = "Wrangling precipitation data",
                         
                         detail = NULL,
                         session = shiny::getDefaultReactiveDomain())
      base::Sys.sleep(0.25)
      continuousDischarge_list$ptp_allYears <- continuousDischarge_list$ptp_allYears[continuousDischarge_list$ptp_allYears$siteID==siteID
                                                                                     &continuousDischarge_list$ptp_allYears$endDateTime>=startDate
                                                                                     &continuousDischarge_list$ptp_allYears$endDateTime<base::as.Date(endDate)+1,]

    })#end of withProgress
    
  },ignoreInit = T)# End getPackage
  
  plots <- shiny::reactiveValues()
  whichTab <- shiny::reactiveValues()
  
  #download the correct graph according to tab
  shiny::observeEvent(input$selectedTab, {
    whichTab$currentTab = input$selectedTab
  })
  
  # Plotting continuous discharge with uncertainty
  output$plot1 <- plotly::renderPlotly({
    
    # Unpack the data frame from getPackage
    continuousDischarge_list <- getPackage()
    
    # Format QF inputs
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
    dateRange <- dates()
    site.id = input$siteId
    start.date = dateRange[[1]]
    end.date = dateRange[[2]]
    input.list = continuousDischarge_list
    lookup.table = productList
    plot.imp.unit = impUnitInput
    mode.dark = darkModeInput
    plot.sci.rvw.QF = sciRvwQfInput
    plot.q.stats = include.q.stats
    # site.id = siteID# Testing
    # start.date = startDate# Testing
    # end.date = endDate# Testing
    
    # Q Stats
    ptpSiteID <- lookup.table$ptpSite[lookup.table$siteID==site.id]
    if(plot.q.stats){
      medQ <- lookup.table$threeXMedQ[lookup.table$siteID==site.id]
      medQUnc <- lookup.table$threeXMedQUnc[lookup.table$siteID==site.id]
    }
    
    # Subset continuous discharge data for plotting
    continuousDischarge_sum <- input.list$csd_continuousDischarge_allYears
    
    # Subset gauge~pressure relationship data for plotting
    sdrc_gaugePressureRelationship <- input.list$sdrc_gaugePressureRelationship_allYears
    
    # Subset gauge~discharge pair data for plotting
    sdrc_gaugeDischargeMeas <- input.list$sdrc_gaugeDischargeMeas_allYears
    
    # Subset precipitation data for plotting
    ptp <- input.list$ptp_allYears
    
    # Pull in historic median Q from GCS
    histMedQ <- histMedQ%>%
      dplyr::filter(siteID==site.id)
    continuousDischarge_sum$monthDay <- base::gsub("[0-9]{4}\\-","",continuousDischarge_sum$date)
    continuousDischarge_sum$histMedQ <- NA
    for(i in 1:length(continuousDischarge_sum$histMedQ)){
      if(continuousDischarge_sum$monthDay[i]%in%histMedQ$monthDay){
        continuousDischarge_sum$histMedQ[i] <- histMedQ$medianQ[histMedQ$monthDay==continuousDischarge_sum$monthDay[i]]
      }else{
        continuousDischarge_sum$histMedQ[i] <- NA
      }
    }
    minYear <- base::unique(histMedQ$minYear)
    maxYear <- base::unique(histMedQ$maxYear)
    
    #axis units
    y1Units <- "(liters per second)"
    y2Units <- "(meter)"
    y3Units <- "(milimeter)"
    convLPStoCFS <- 0.035314
    convMtoFt <- 3.28084
    convMMtoIN <- 0.0393701
    
    #SI to imperial
    ##needs to be above plotly call so axis are created correctly
    if(plot.imp.unit){
      # Discharge and Gauge
      continuousDischarge_sum <- continuousDischarge_sum %>%
        dplyr::mutate(meanURemnUnc = meanURemnUnc*convLPStoCFS,
                      meanLRemnUnc = meanLRemnUnc*convLPStoCFS,
                      meanUParaUnc = meanUParaUnc*convLPStoCFS,
                      meanLParaUnc = meanLParaUnc*convLPStoCFS,
                      meanQ = meanQ*convLPStoCFS,
                      dischargeFinalQFSciRvw = dischargeFinalQFSciRvw*convLPStoCFS,
                      meanUHUnc = meanUHUnc*convMtoFt,
                      meanLHUnc = meanLHUnc*convMtoFt,
                      meanH = meanH*convMtoFt,
                      histMedQ = histMedQ*convLPStoCFS)
      sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
        dplyr::mutate(gauge_Height = gauge_Height*convMtoFt)
      sdrc_gaugeDischargeMeas <- sdrc_gaugeDischargeMeas%>%
        dplyr::mutate(streamDischarge = streamDischarge*convLPStoCFS,
                      gaugeHeight = gaugeHeight*convMtoFt)
      
      # Precipitation
      ptp <- ptp%>%
        dplyr::mutate(precipBulk = precipBulk*convMMtoIN,
                      precipExpUncert = precipExpUncert*convMMtoIN,
                      precipBulkLoUnc = precipBulkLoUnc*convMMtoIN,
                      precipBulkUpUnc = precipBulkUpUnc*convMMtoIN)
      
      # Constants
      medQ = medQ*convLPStoCFS
      medQUnc = medQUnc*convLPStoCFS
      
      y1Units <- "(Cubic Feet per second)"
      y2Units <- "(Feet)"
      y3Units <- "(Inches)"
    }
    
    #y-axis
    y1 <- base::list(side='left',
                     automargin=T,
                     title=stringr::str_c("Discharge ",y1Units),
                     tickfont=base::list(size=16),
                     titlefont=base::list(size=18),
                     showgrid=F,
                     zeroline=F)
    
    y2 <- base::list(side='right',
                     overlaying="y",
                     automargin=T,
                     title=stringr::str_c("Stage ",y2Units),
                     tickfont=base::list(size=16,color = '#CC79A7'),
                     titlefont=base::list(size=18,color = '#CC79A7'),
                     showgrid=F,
                     zeroline=F)
    
    y3 <- base::list(side='right',
                     overlaying="y",
                     automargin=T,
                     title=stringr::str_c("Percipitation ",y3Units),
                     tickfont=base::list(size=16,color = "#0072B2"),
                     titlefont=base::list(size=18,color = "#0072B2"),
                     showgrid=F,
                     zeroline=F,
                     anchor="free",
                     position=0.98)
    
    # Build plot layout
    method <- plotly::plot_ly(source = "phenoDate")%>%
      plotly::layout(
        yaxis = y1, yaxis2 = y2, yaxis3 = y3,
        xaxis=base::list(domain=c(0,.9),
                         tick=14,
                         automargin=T,
                         title="Date",
                         tickfont=base::list(size=16),
                         titlefont=base::list(size=18)),
        legend=base::list(x=-0.2,y=0.87,
                          font=base::list(size=14)),
        updatemenus=base::list(
          base::list(
            type='buttons',
            showactive=FALSE,
            buttons=base::list(
              base::list(label='Scale Discharge\n- Linear -',
                         method='relayout',
                         args=base::list(base::list(yaxis=base::list(type='linear',
                                                                     title=stringr::str_c("Discharge ",y1Units),
                                                                     tickfont=base::list(size=16),
                                                                     titlefont=base::list(size=18),
                                                                     showgrid=F,
                                                                     zeroline=F)))),
              base::list(label='Scale Discharge\n- Log -',
                         method='relayout',
                         args=base::list(base::list(yaxis=base::list(type='log',
                                                                     title=stringr::str_c("Discharge ",y1Units," - log"),
                                                                     tickfont=base::list(size=16),
                                                                     titlefont=base::list(size=18),
                                                                     showgrid=F,
                                                                     zeroline=F))))))))
    
    
    #dark mode styling
    dischargeColor <- "black"
    if(mode.dark){
      method <- method %>%
        plotly::layout(paper_bgcolor="#222",plot_bgcolor='#222',
                       font = base::list(color = 'white'))
      dischargeColor <- "white"
    }
    
    # Add Quality flags
    if(plot.sci.rvw.QF){
      method <- method %>%
        plotly::add_trace(data=continuousDischarge_sum,x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQFSciRvw,type='scatter',mode='none',fill = 'tozeroy',hoverinfo="none", showlegend= F, fillcolor = 'lightgray')
    }
    
    # Add base plot 
    method <- method %>%
      
      # Q Uncertainty
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$meanURemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='line',line=base::list(color='#D55E00'),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=F,legendgroup='group1',visible = "legendonly")%>%
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$meanLRemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#D55E00',hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=T,legendgroup='group1',visible = "legendonly")%>%
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$meanUParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='line',line=base::list(color='#E69F00'),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=F,legendgroup='group2',visible = "legendonly")%>%
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$meanLParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#E69F00',hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=T,legendgroup='group2',visible = "legendonly")%>%
      
      # H Uncertainty
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$meanUHUnc,name="Stage\nUncertainty",type='scatter',mode='line',line=base::list(color='#56B4E9'),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",yaxis='y2',showlegend=F,legendgroup='group3',visible = "legendonly")%>%
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$meanLHUnc,name="Stage\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#56B4E9',hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",yaxis='y2',showlegend=T,legendgroup='group3',visible = "legendonly")%>%
      
      # H and Q Series
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$meanQ, name="Continuous\nDischarge",type='scatter',mode='lines',line = base::list(color = dischargeColor),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",yaxis='y1',legendgroup='group4')%>%
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$meanH, name="Continuous\nStage",type='scatter',mode='lines',line = base::list(color = '#CC79A7'),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",yaxis='y2',showlegend=T,legendgroup='group5')%>%
      
      # Empirical H and Q
      plotly::add_trace(x=~sdrc_gaugeDischargeMeas$date,y=~sdrc_gaugeDischargeMeas$streamDischarge,name="Measured\nDischarge", type='scatter', mode='markers',marker = base::list(color = '#009E73',size=8,line = base::list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=T,legendgroup='group6')%>%
      plotly::add_trace(x=~sdrc_gaugeDischargeMeas$date,y=~sdrc_gaugeDischargeMeas$gaugeHeight,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=base::list(color="#F0E442",size=8,line = base::list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=F,legendgroup='group7')%>%
      plotly::add_trace(x=~sdrc_gaugePressureRelationship$date,y=~sdrc_gaugePressureRelationship$gauge_Height,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=base::list(color="#F0E442",size=8,line = base::list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=T,legendgroup='group7')%>%
      
      #Historical Med Q
      plotly::add_trace(x=~continuousDischarge_sum$date,y=~continuousDischarge_sum$histMedQ, name=stringr::str_c("Historic Median\nDischarge: ","\n",minYear,"-",maxYear),type='scatter',mode='lines',line = base::list(color = 'grey'),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",legendgroup='group8',visible = "legendonly")
    
    #Precipitation Data
    method <- method %>%
      plotly::add_trace(data=ptp,x=~endDateTime,y=~precipBulkLoUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='line',line=base::list(color='#431A74'),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=F,legendgroup='group9',visible = "legendonly")%>%
      plotly::add_trace(data=ptp,x=~endDateTime,y=~precipBulkUpUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#431A74',hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",showlegend=T,legendgroup='group9',visible = "legendonly") %>%
      plotly::add_trace(data=ptp,x=~endDateTime,y=~precipBulk,name=stringr::str_c("Continuous\nPrecipitation\nSite: ",ptpSiteID),yaxis = "y3",type='scatter',mode='lines',line = base::list(color = '#0072B2'),hovertemplate = "Date/UTC-Time: %{x}<br>Value: %{y}<br>Click to view image of stream",legendgroup='group10',visible = "legendonly")
    
    # Add the internal parameters
    if(plot.q.stats){
      method <- method%>%
        plotly::add_segments(x=~base::min(continuousDischarge_sum$date,na.rm = T),xend=~base::max(continuousDischarge_sum$date,na.rm = T),y=~medQ+medQUnc,yend=~medQ+medQUnc,line=base::list(color='grey',dash='dash'),name="3x Median Discharge\nPlus Uncertainty",showlegend=T,legendgroup='group11',visible = "legendonly",hovertemplate=paste0("Value: ",medQ+medQUnc))
      
      if(!plot.imp.unit){
        method <- method%>%
          plotly::layout(
            title=list(text=base::paste0("<br><b>3x Median Discharge = ",base::round(medQ,digits = 0)," +/- ",base::round(medQUnc,digits = 1)," L/s"),
                       xanchor="left",
                       xref="paper",
                       x=0.02))
      }else{
        method <- method%>%
          plotly::layout(
            title=list(text=base::paste0("<br><b>3x Median Discharge = ",base::round(medQ,digits = 0)," +/- ",base::round(medQUnc,digits = 1)," ft^3/s"),
                       xanchor="left",
                       xref="paper",
                       x=0.02))
      }
    }
    
    plots$plot.cont.Q <- method
    

  })# End plot1
  
  # Plotting rating curve(s) with uncertainty
  output$plot2 <- plotly::renderPlotly({
    
    # Unpack the list of curve IDs from getPackage
    continuousDischarge_list <- getPackage()
    
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
    dateRange <- dates()
    site.id = input$siteId
    start.date = dateRange[[1]]
    end.date = dateRange[[2]]
    input.list = continuousDischarge_list
    plot.imp.unit = impUnitInput
    mode.dark = darkModeInput
    
    # Get data
    curveIDs_temp <- unique(rcPlotData$rcGaugings$curveID[grep(site.id,rcPlotData$rcGaugings$curveID)])
    curveIDs <- temp[grep(paste(seq(format(as.Date(start.date),"%Y"),format(as.Date(end.date),"%Y")),collapse="|"),temp)]
    if(length(curveIDs)==0){
      curveIDs <- max(curveIDs_temp)
    }

    if(base::all(!base::is.na(curveIDs))){
      rcData <- rcPlotData$rcData%>%
        dplyr::filter(curveID%in%curveIDs)
      rcGaugings <- rcPlotData$rcGaugings%>%
        dplyr::filter(curveID%in%curveIDs)
      
      # Add each rating curve based on the vector of unique rating curve IDs
      for(i in 1:base::length(base::unique(rcData$curveID))){
        currentCurveID <- base::unique(rcData$curveID)[i]
        rcData_curveID <- rcData%>%
          dplyr::filter(curveID==currentCurveID)
        rcGaugings_curveID <- rcGaugings%>%
          dplyr::filter(curveID==currentCurveID)
        
        #axis units
        x1Units <- "(meter)"
        y1Units <- "(liters per second)"
        convLPStoCFS <- 0.035314
        convMtoFt <- 3.28084
        
        if(plot.imp.unit){
          rcData_curveID <- rcData_curveID %>%
            dplyr::mutate(Hgrid = Hgrid*convMtoFt,
                          totalUTop = totalUTop*convLPStoCFS,
                          totalUBottom = totalUBottom*convLPStoCFS,
                          pramUTop = pramUTop*convLPStoCFS,
                          pramUBottom = pramUBottom*convLPStoCFS,
                          maxPostQ = maxPostQ*convLPStoCFS)
          rcGaugings_curveID <- rcGaugings_curveID %>%
            dplyr::mutate(H = H*convMtoFt,
                          Q = Q*convLPStoCFS)
          
          x1Units <- "(Feet)"
          y1Units <- "(Cubic Feet per second)"
        }
        
        # Build plot layout
        rcPlot <- plotly::plot_ly(data=rcData)%>%
          plotly::layout(
            xaxis=base::list(tick=14,
                             automargin=T,
                             title=stringr::str_c("Stage ", x1Units),
                             tickfont=base::list(size=16),
                             titlefont=base::list(size=18)),
            yaxis=base::list(automargin=T,
                             title=stringr::str_c("Discharge ", y1Units),
                             # range=c(0,base::max(rcData$totalUBottom)*1.05),
                             tickfont=base::list(size=16),
                             titlefont=base::list(size=18),
                             showgrid=T,
                             zeroline=T),
            legend=base::list(x=-0.2,y=0.87,
                              font=base::list(size=14)),
            updatemenus=base::list(
              base::list(
                type='buttons',
                showactive=FALSE,
                buttons=base::list(
                  base::list(label='Scale Discharge\n- Linear -',
                             method='relayout',
                             args=base::list(base::list(yaxis=base::list(type='linear',
                                                                         title=stringr::str_c("Discharge ", y1Units),
                                                                         # range=c(0,base::max(rcData$totalUBottom)*1.05),
                                                                         tickfont=base::list(size=16),
                                                                         titlefont=base::list(size=18),
                                                                         showgrid=T,
                                                                         zeroline=T)))),
                  base::list(label='Scale Discharge\n- Log -',
                             method='relayout',
                             args=base::list(base::list(yaxis=base::list(type='log',
                                                                         title=stringr::str_c("Discharge ",y1Units," - log"),
                                                                         # range=c(0,base::log10(base::max(rcData$totalUBottom)*1.05)),
                                                                         tickfont=base::list(size=16),
                                                                         titlefont=base::list(size=18),
                                                                         showgrid=T,
                                                                         zeroline=T))))))))
        
        #dark mode styling
        ratingColor <- "black"
        if(mode.dark){
          rcPlot <- rcPlot%>%
            plotly::layout(paper_bgcolor="#222",plot_bgcolor='#222',
                           font = base::list(color = 'white'))
          ratingColor <- "white"
        }
        
        rcPlot <- rcPlot%>%
          # Total Uncertainty
          plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=base::list(color='#D55E00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
          plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='#D55E00',line=base::list(color='#D55E00'),hovertemplate = "Stage(m): %{x} <br> Discharge(lps): %{y}",showlegend=T,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
          # Parametric Uncertainty
          plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=base::list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
          plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='#E69F00',line=base::list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
          # Max Post Q
          plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~maxPostQ,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='line',line=base::list(color=ratingColor),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=T,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))%>%
          # Empirical H/Q Pairs
          plotly::add_trace(data=rcGaugings_curveID,x=~H,y=~Q,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='markers',marker=base::list(color=ratingColor),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))
      }
    }else{
      rcPlot <- plotly::plotly_empty()%>%
        plotly::layout(
          title=base::list(
            text = "No Stage-discharge rating curves (DP4.00133.001)\n data available for this site.",
            yref = "paper",
            y = 0.5,
            font=base::list(size=28)
          )
        )
      #dark mode styling
      ratingColor <- "black"
      if(mode.dark){
        rcPlot <- rcPlot%>%
          plotly::layout(paper_bgcolor="#222",plot_bgcolor='#222',
                         font = base::list(color = 'white'))
        ratingColor <- "white"
      }
    }
    
    plots$plot.RC <- rcPlot
    
  })# End plot2
  
  #download handler for plotly download functionality
  output$downloadPlotly <- shiny::downloadHandler(
    filename = function() {
      downloadParam <- whichPlot()
      #file name format NEON.DOMAIN.SITE.DP4.0013[0,3]_STARTDATE_ENDDATE.html
      base::paste0("NEON.",domainID,".",siteID,".",downloadParam$dpName,"_",input$dateRange[1],"_",input$dateRange[2],".html")
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
  )
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
  
}#end of server
