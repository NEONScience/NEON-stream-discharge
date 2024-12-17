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
  shiny::observe({
    new_clickEvent <- plotly::event_data(event = "plotly_click")
    
    if (!base::is.null(new_clickEvent)) {
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
    
    #__Generate metadata table ####
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
      plots$plot.cont.Q <- cont.Q.plot(site.id = input$siteId,
                                       start.date = input$dateRange[[1]],
                                       end.date = input$dateRange[[2]],
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
      plots$plot.RC <- RC.plot(site.id = input$siteId,
                               start.date = input$dateRange[[1]],
                               end.date = input$dateRange[[2]],
                               plot.imp.unit = impUnitInput,
                               mode.dark = darkModeInput)
    })# End renderPlotly
  },ignoreInit = T)# End observeEvent

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
      base::paste0("NEON.",input$domainId,".",input$siteId,".",downloadParam$dpName,"_",input$dateRange[1],"_",input$dateRange[2],".html")
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
  
  # TAB: TIMESERIES VIEWER -- BEGIN #####
  
  
  #####################################################################################################
  #                           Beginnging of Cross Section framework                                   #
  #####################################################################################################
  
  # Select site ID based on the domain ID chosen
  shiny::observe({
    x<-Target_df$Site[Target_df$Domain==input$XS_Domain]
    shiny::updateSelectInput(session, "XS_site", choices= unique(x))
    
  })  
  #change datatable column names
  colnames(Target_df)<- c(
    "Domain",
    "Site",
    "TargetGaugeHeight",
    "TargetGaugeHeight10",
    "TargetGaugeHeight30",
    "TypicalHighFlowPeriod"
  )
  
  
  # observe({
  #   req(input$XS_site)
  #   filter_target<- Target_df[Target_df$Site==input$XS_site, ]
  
  
  #Observe row selection and display details
  observe({
    if(input$XS_site!= "No Site Selected")
    {
      
      filter_target<- Target_df %>%
        filter(Site==input$XS_site)
      
      
      
      if (nrow(filter_target) > 0) {
        output$TargetGaugeHeights <- renderUI({
          fluidRow(
            column(6,
                   tags$div(
                     tags$h2("Target Gauge Height (m):", HTML(paste0("<b>",filter_target$TargetGaugeHeight,"</b>"))),
                     tags$h2("Target Gauge Height - 10% (m):", HTML(paste0("<b>",filter_target$TargetGaugeHeight10,"<b>"))))),
            column(6,
                   tags$div( 
                     tags$h2("Target Gauge Height - 30% (m):", HTML(paste0("<b>",filter_target$TargetGaugeHeight30, "<b>"))),
                     tags$h2("Typical High Flow Period:", HTML(paste0("<b>",filter_target$TypicalHighFlowPeriod, "</b>")))))
          ) 
        })
        
        
      } else {
        output$TargetGaugeHeights <- renderUI({
          tags$h3("Data not available")
        })
      }
    } else {
      output$TargetGaugeHeights <- renderUI({
        tags$h3("Please Select a Domain and Site to see Gauge Height Information")
      })
    }
    
  })
  
##plotting DSCXS for CUPE
  #Reactive expression that triggers Show Plot button when clicked
  xs_plot<-eventReactive(input$ShowPlot,{
    dischargePlot
  #   dischargePlot <- CUPE_XS.plot%>%
  #     #add_trace(y= c1Gauge,name = 'Control 1 Gauge Height',mode='lines',line = list(width = 3, dash='dash')) %>%
  #     #add_trace(y= c2Gauge,name = 'Control 2 Gauge Height',mode='lines',line = list(width = 3, dash='dash')) %>%
  #     add_trace(y= baseFlowGauge,name = 'Baseflow Gauge Height',text = baseFlowGauge,mode='lines',line = list(width = 3, dash='dash')) %>%
  #     add_trace(y= bankfullGauge,name = 'Bankfull Gauge Height',text = bankfullGauge,mode='lines',line = list(width = 3, dash='dash')) %>%
  #     add_trace(y= peakStage,name = 'Peak Stage',text = bankfullGauge,mode='lines',line = list(width = 3, dash='dash')))
  # for(i in 1:nrow(buckets10)){
  #   (dischargePlot <- dischargePlot%>%
  #      add_trace(y= c(buckets_10per[i]),name = paste0('10% Bin ',i),mode='lines',line = list(width = 1, dash='dash',color='black')))
  # }
  })
#Render the plot output
  output$xsdsc<- renderPlotly({
    xs_plot()
  
})
 
  
  # # Render the text outputs based on the reactive values
  # output$TargetGaugeHeight <- renderText({filter_target$TargetGaugeHeight})
  # output$TargetGaugeHeight10 <- renderText({filter_target$TargetGaugeHeight10 })
  # output$TargetGaugeHeight30 <- renderText({filter_target$TargetGaugeHeight30 })
  # output$TypicalHighFlowPeriod <- renderText({filter_target$TypicalHighFlowPeriod })
 
  #####################################################################################################
  #                           Beginnging of Blue Heron framework                                      #
  #####################################################################################################
  
  #Observe changes in water type selected GW = ground water and SW = surface water
  observeEvent(input$waterType,{ 
    if(input$waterType == "GW")
    {
      #Updates the location values to match GWW locations. 301 represents GWW1 for example.
      updateSelectInput(session = session, inputId = "HOR", label = "Select location", choices = 301:308)
      shinyjs::show("wellDepth")
      shinyjs::show("cableLength")
      shinyjs::show("optionalGWWMessage")
    } else {
      #Updates the location values to match SW locations. 101 represents S1, 131 is also S1 but the selection is site dependent whether its a standalone troll or not.
      updateSelectInput(session = session, inputId = "HOR", label = "Select location", choices = c(101,102,110,131,132))
      shinyjs::show("trollType")
      shinyjs::hide("optionalGWWMessage")
      shinyjs::hide("wellDepth")
      shinyjs::hide("cableLength")
    }
  }) #End observeEvent for waterType
  
  #Observe the what data source is
  observeEvent(input$dataSource, {
    
    #simple if else toggles a textInput that allows a single pressure reading to inputed
    if(input$dataSource == "L0 Data Query")
    {
      shinyjs::hide("singlePressure")
      shinyjs::hide("grafanaFile")
    }else if(input$dataSource == "Grafana CSV File"){
      shinyjs::hide("singlePressure")
      shinyjs::show("grafanaFile")
    } else if(input$dataSource == "Instant Pressure Reading")
    {
      shinyjs::show("singlePressure")
      shinyjs::hide("grafanaFile")
    }
  }) #end observeEvent for the type of data selection from input$dataSource
  
  observeEvent(input$rtdvRun, {
    shinyjs::show("GaugeHeightLoadBar")
    updateTabsetPanel(session, "calculatedStageTimeSeries", selected = "CG_timeSeries")

    realTimeDataViewer(input, output, session)
  })
  # Select site ID based on the domain ID chosen
  shiny::observe({x <- productList$siteID[productList$domain == input$rtdvDomain]
  shiny::updateSelectInput(session,"rtdvSite",choices = unique(x))})
  
}#end of server
# Run the app ----
#shiny::shinyApp(ui = ui, server = server)  ###MV added this according to previous commit
