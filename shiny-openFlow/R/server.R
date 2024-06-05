#server function ----
server <- function(session, input, output) {
  
  # Select site ID based on the domain ID chosen
  shiny::observe({x <- productList$siteID[productList$domain == input$domainId]
  shiny::updateSelectInput(session,"siteId",choices = unique(x))})
  
  #handles light and dark mode switch
  shiny::observe(session$setCurrentTheme(
    if (base::isTRUE(input$dark_mode)) dark else light
  ))
  
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
      phenoURL <- neonStageQplot::pheno.GET(dp.id="DP1.20002",
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
          tags$img(
            src = phenoURL),
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
  
  # Download data, create summary table, and save output
  getPackage <- shiny::eventReactive(input$submit,{
    
    # # Manually set input variables for local testing - comment out when running app
    # input <- base::list()
    # input$siteId <- "MCRA"
    # input$domainId <- "D11"
    # input$dateRange[[1]] <- "2022-08-01"
    # input$dateRange[[2]] <- "2022-08-30"
    # input$apiToken <- NA
    # output <- base::list()
    # include.q.stats <-  T
    
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
    siteID <<- input$siteId
    domainID <<- input$domainId
    startDate <- base::format(input$dateRange[1])
    endDate <- base::format(input$dateRange[2])
    if(!grepl('internal', HOST)){
      #external app - use api token from user
      apiToken <- input$apiToken
    }
    
    # ZN 2024-03-04 - should not need this anymore after containerization and deployment to GCS
    # # Code to stop the function if the app is on the external server and a user has selected a date range > 90 days
    # if(constrain.dates&base::difftime(endDate,startDate,units="days")>90){
    #   shinyalert::shinyalert("Sorry! We are still in development...","At this time, the app cannot support downloads > 90 days. Please select a smaller date range.",type="error")
    #   stop("Requested time period must be no more than 90 days")
    # }
    
    #progress bar for data downloads
    shiny::withProgress(message = 'Submit',detail = '', min = 0, max = 1 ,value = 0, {
      
      shiny::incProgress(amount = 0.50,
                         message = "Pulling data from neonUtilities",
                         
                         detail = NULL,
                         session = shiny::getDefaultReactiveDomain())
      base::Sys.sleep(0.25)
      
      # Download and process NEON data
      continuousDischarge_list <- neonStageQplot::get.cont.Q.NEON.API(site.id = siteID,
                                                                      start.date = startDate,
                                                                      end.date = endDate,
                                                                      api.token = apiToken,
                                                                      include.q.stats = include.q.stats)
      
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
    # if(input$qctrFlag == TRUE){
    #   finalQfInput <- T
    # }else{
    #   finalQfInput <- F
    # }
    if(input$qctrFlagScRv == TRUE){
      sciRvwQfInput <- T
    }else{
      sciRvwQfInput <- F
    }
    # if(input$precipQctrFlag == TRUE){
    #   precipQctrFlag <- T
    # }else{
    #   precipQctrFlag <- F
    # }
    # if(input$precipQctrFlagScRv == TRUE){
    #   precipQctrFlagScRv <- T
    # }else{
    #   precipQctrFlagScRv <- F
    # }
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
    plots$plot.cont.Q <- neonStageQplot::cont.Q.plot(site.id = input$siteId,
                                                     start.date = input$dateRange[[1]],
                                                     end.date = input$dateRange[[2]],
                                                     input.list = continuousDischarge_list,
                                                     plot.imp.unit = impUnitInput,
                                                     mode.dark = darkModeInput,
                                                     # plot.final.QF = finalQfInput,
                                                     plot.sci.rvw.QF = sciRvwQfInput,
                                                     # plot.precip.final.QF = precipQctrFlag,
                                                     # plot.precip.sci.rvw.QF = precipQctrFlagScRv,                                          
                                                     plot.q.stats = include.q.stats)
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
    plots$plot.RC <- neonStageQplot::RC.plot(site.id = input$siteId,
                                             start.date = input$dateRange[[1]],
                                             end.date = input$dateRange[[2]],
                                             input.list = continuousDischarge_list,
                                             plot.imp.unit = impUnitInput,
                                             mode.dark = darkModeInput)
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