##############################################################################################
#' NEON continuous discharge and stage-discharge rating curve data visualization app

#' @name run.RC.cont.Q.plot

#' @author
#' Divine Aseaku \email{divineaseaku@gmail.com} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description  An interactive app which  plots continuous discharge, Stage discharge and
#' rating curves using language R Data is downloaded from the NEON data portal using
#' neonUltilities and ploting is done at 20mins time interval

#' @return This function launches a shiny app to interactively plot published NEON data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export run.RC.cont.Q.plot

# changelog and author contributions / copyrights
#   Divine Aseaku (2021-08-04)
#     original creation
#   Zachary L. Nickerson (2021-09-16)
#     updates for plotting provisional 2021 data and TOOK inlet and outlet
#   Zachary L. Nickerson (2022-02-04)
#     updates for plotting TOMB-USGS discharge data
##############################################################################################
# # Source packages and set options
options(stringsAsFactors = F)

run.RC.cont.Q.plot <-function(){

  # Read in refernce table from Github
  # setwd("~/Github/NEON-stream-discharge/L4Discharge/AOSApp") # Code for testing locally - comment out when running app
  productList <- readr::read_csv(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/master/shinyApp/aqu_dischargeDomainSiteList.csv"))

  # Develop the User Interface
  ui <- shiny::fluidPage(style = "padding:25px; margin-bottom: 30px;",
                         tags$head(tags$style("#shiny-modal img { max-width: 100%; }")),#####modal scaling 
                         shiny::titlePanel("API TEST NEON Continuous discharge (DP4.00130.001) and Stage-discharge rating curves (DP4.00133.001) data visualization application"),
                         shiny::fluidRow(shiny::column(3,
                                         shiny::fluidRow("Welcome! This application allows you view and interact with NEON's Continuous discharge",tags$a(href="https://data.neonscience.org/data-products/DP4.00130.001", "(DP4.00130.001)", target="_blank"), "and Stage-discharge rating curves",tags$a(href="https://data.neonscience.org/data-products/DP4.00133.001", "(DP4.00133.001)", target="_blank")," data products. Select a site and date range and the app will download data from the NEON Data Portal and plot continuous and discrete stage and discharge timeseries data and all rating curves used in the development of the timeseries data."),
                                         shiny::fluidRow(style = "background-color:#F8F8F8; height:auto;margin-top: 15px;padding: 15px;",
                                                         shiny::selectInput("domainId","Domain ID",productList$domain),
                                                         shiny::selectInput("siteId","Select Site ID",NULL),
                                                         shiny::dateRangeInput("dateRange","Date range:",
                                                                               startview="month",
                                                                               min="2016-01-01",
                                                                               start="2019-01-01",end="2019-01-31",
                                                                               format="yyyy-mm-dd"),
                                                         shiny::actionButton(inputId="submit","Submit"),
                                                         shiny::checkboxInput("qctrFlag", "Include Final Quality Flag", FALSE),
                                                         shiny::checkboxInput("qctrFlagScRv", "Include Science Review Quality Flag", FALSE)),
                                         shiny::hr(),
                                         shiny::fluidRow(shiny::uiOutput("siteInfo" )),
                                         shiny::hr(),
                                         shiny::fluidRow(shiny::textOutput("title"),
                                                         DT::dataTableOutput("table"))),#end of first col
                                         shiny::column(9,
                                         shiny::tabsetPanel(type = "tabs",
                                                            shiny::tabPanel("Continuous Discharge",
                                                                            shinycssloaders::withSpinner(plotly::plotlyOutput("plot1",height="800px"),
                                                                                                         color = "#00ADD7"),
                                                                            style = "background-color:#F8F8F8;"),
                                                            shiny::tabPanel("Rating Curve(s)",
                                                                            shinycssloaders::withSpinner(plotly::plotlyOutput("plot2",height="800px"),
                                                                                                         color = "#00ADD7"))))#end of second col
                           )#end of fluid row
    ) # end of ui and fluidPage


  #server function
  server <- function(session, input, output) {
    
    #Global Vars
    old_clickEvent <- 0
    site <- NULL
    domain <- NULL
    
    # Select site ID based on the domain ID chosen
    shiny::observe({x <- productList$siteID[productList$domain == input$domainId]
    shiny::updateSelectInput(session,"siteId",choices = unique(x))})

    
    # phenoImage
    #displays phenocam image when point is clicked on graph
    #pulls image closest to selected date
    observe({
      new_clickEvent <- event_data(event = "plotly_click", source = "phenoDate")
      new_value <- ifelse(is.null(new_clickEvent),"0",new_clickEvent$x)
      
      #compares clickEvents to keep phenoimage from appearing on submit click
      if (old_clickEvent!=new_value) {
        old_clickEvent <<- new_value
        dateTime <- stringr::str_replace(new_clickEvent$x, " ","T")
        dateTime <- paste0(dateTime,":00Z")
        phenocamImg <- phenocamGET(site,domain,dateTime)
        
        #shows modal from Bad GET request
        if(is.null(phenocamImg$url)){
          usrDateTime <- dateTime
          usrDateTime <- stringr::str_replace(usrDateTime, "T"," ")
          usrDateTime <- substr(usrDateTime,1,nchar(usrDateTime)-4)
          phenoModalBad(usrDateTime)
          
          #shows modal from Good GET request
        }else{
          showModal(phenoModalGood(phenocamImg))
        }
      }
    })
    
    #modal for good phenocamGET request
    phenoModalGood <- function(phenocamImg)
    {modalDialog(
      title = "Phenocam Image",
      "To download the image right click on the image and click 'Save image as...'",
      size = "l",
      tags$img(
        src = phenocamImg$url),
      easyClose = TRUE)}
    
    #modal for bad phenocamGET request
    phenoModalBad <- function(usrDateTime)
    {modalDialog(
      title = "Phenocam Image",
      "No phenocam image available at ",site," for Date/Time",usrDateTime,
      size = "s",
      easyClose = TRUE)}
    
    
    #phenocamGET returns url to phenocam image
    phenocamGET <- function(site,domain,dateTime){
      ###API Call
      siteID <- site
      domainID <- domain
      #UTC dateTime
      dateTime <- dateTime
      
      ###Test GET
      # siteID <- "PRIN"
      # domainID <- "D11"
      # #UTC dateTime
      # dateTime <- "2021-12-01T18:00:00Z"
      
      phenoGET <- httr::content(httr::GET(url = paste0("https://phenocam.sr.unh.edu/neonapi/imageurl/NEON.",domainID,".",siteID,".DP1.20002/",dateTime,"/")),
                                encoding = "UTF-8")
      return(phenoGET)
    }
   
    # Download data, create summary table, and save output
    getPackage <- shiny::eventReactive(input$submit,{
      
      # Define site-specific metadata for rendering
      metaD <-  productList%>%
        dplyr::filter(siteID == input$siteId)%>%
        dplyr::select(upstreamWatershedAreaKM2,reachSlopeM,averageBankfullWidthM,d50ParticleSizeMM)%>%
        dplyr::rename("Upstream watershed area (km^2)"= upstreamWatershedAreaKM2,
                      "Reach slope (m)" = reachSlopeM,
                      "Mean bankfull width (m)"= averageBankfullWidthM,
                      "D50 particle size (mm)"=d50ParticleSizeMM) %>%
        dplyr::mutate_all(as.character)%>%
        tidyr::pivot_longer(c("Upstream watershed area (km^2)","Reach slope (m)","Mean bankfull width (m)","D50 particle size (mm)"),
                            names_to = "MetaData",
                            values_to = "Values")

      # Enter header for metadata table
      output$title <- shiny::renderText("Metadata Table")

      # Create metadata table output
      output$table <- DT::renderDataTable({dat <- DT::datatable(metaD,  options = list(dom = 't'))},selection = 'single')
      
      # Manually set input variables for local testing - comment out when running app
      # input <- base::list()
      # input$siteId <- "TOMB"
      # input$dateRange[[1]] <- "2020-01-01"
      # input$dateRange[[2]] <- "2020-03-31"

      # Set site variables (special considerations for TOOK)
      if (stringr::str_detect(input$siteId,"TOOK")) {
        site <<- "TOOK"
      }else{
        site <<- input$siteId
      }
      domain <<- input$domainId

      # Create site description output
      siteURL <- base::paste0("https://www.neonscience.org/field-sites/",base::tolower(site))
      url <- a("Click here", href=siteURL,target="_blank",style="text-decoration: none; hover:{font-size:150%;}")
      output$siteInfo <- shiny::renderUI({tagList("Site: ",input$siteId, url, "for site description",sep="\n")})

      # Set date variables for app running (special consideration for TOOK)
      startDate <- base::format(input$dateRange[1])
      endDate <- base::format(input$dateRange[2])

      # Rating curve data queries need to span an entire water year to ensure we are getting all the appropriate data
      searchIntervalStartDate <- base::as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = startDate)$startDate)
      searchIntervalEndDate <- base::as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = endDate)$endDate)

      #progress bar for data downloads
      shiny::withProgress(message = 'Submit',detail = '', min = 0, max = 1 ,value = 0, {
        shiny::incProgress(amount = 0.33,
                           message = "Downloading DP4.00130.001",
                           detail = NULL,
                           session = shiny::getDefaultReactiveDomain())
        base::Sys.sleep(0.25)

        # Get continuous discharge data from the NEON API
        DP4.00130.001 <- neonUtilities::loadByProduct(
          dpID="DP4.00130.001",
          package = "expanded",
          check.size = F,
          site = site,
          startdate = base::format(base::as.POSIXct(startDate),"%Y-%m"),
          enddate = base::format(base::as.POSIXct(endDate),"%Y-%m"))

        shiny::incProgress(amount = 0.33,
                           message = "Downloading DP4.00133.001",
                           detail = NULL,
                           session = shiny::getDefaultReactiveDomain())
        base::Sys.sleep(0.25)

        # Get rating curve data from the NEON API
        if(site!="TOMB"){
          DP4.00133.001 <- neonUtilities::loadByProduct(
            dpID="DP4.00133.001",
            package = "basic",
            check.size = F,
            site = site
            # startdate = searchIntervalStartDate,
            # enddate = searchIntervalEndDate
          )
        }

        #updating progress bar
        shiny::incProgress(amount = 0.33,
                           message = "Processing data... ",
                           detail = NULL,
                           session = shiny::getDefaultReactiveDomain())
        base::Sys.sleep(0.25)

        # Format gauge-discharge measurement data
        if(site!="TOMB"){
          sdrc_gaugeDischargeMeas <- DP4.00133.001$sdrc_gaugeDischargeMeas
          if (input$siteId=="TOOK_inlet") {
            sdrc_gaugeDischargeMeas <- sdrc_gaugeDischargeMeas%>%
              dplyr::filter(stringr::str_detect(curveID,"TKIN"))
          }else{
            if(input$siteId=="TOOK_outlet"){
              sdrc_gaugeDischargeMeas <- sdrc_gaugeDischargeMeas%>%
                dplyr::filter(stringr::str_detect(curveID,"TKOT"))
            }
          }
          sdrc_gaugeDischargeMeas <- sdrc_gaugeDischargeMeas%>%
            tidyr::separate(gaugeEventID,c("site","date"),5,remove = F)%>%
            dplyr::mutate(date=base::paste0(base::as.Date(date,format="%Y%m%d")," 20:00:00"))%>%
            dplyr::select(date,gaugeHeight,streamDischarge)
          sdrc_gaugeDischargeMeas$date <- base::as.character(sdrc_gaugeDischargeMeas$date)
        }

        # Format continuous discharge data
        if(site!="TOMB"){
          csd_continuousDischarge <- DP4.00130.001$csd_continuousDischarge
          csd_continuousDischarge$date <- lubridate::round_date(csd_continuousDischarge$endDate, "20 mins")
          if (input$siteId=="TOOK_inlet") {
            csd_continuousDischarge <- csd_continuousDischarge%>%
              dplyr::filter(stringr::str_detect(curveID,"TKIN"))
          }else{
            if(input$siteId=="TOOK_outlet"){
              csd_continuousDischarge <- csd_continuousDischarge%>%
                dplyr::filter(stringr::str_detect(curveID,"TKOT"))
            }
          }
        }else{
          csd_continuousDischarge <- DP4.00130.001$csd_continuousDischargeUSGS
          csd_continuousDischarge$date <- csd_continuousDischarge$endDate
        }

        # Format gauge-pressure relationship data
        if(site!="TOMB"){
          sdrc_gaugePressureRelationship <- DP4.00130.001$sdrc_gaugePressureRelationship
          if(!base::is.null(sdrc_gaugePressureRelationship)){
            if (input$siteId=="TOOK_inlet") {
              sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
                dplyr::filter(stringr::str_detect(regressionID,"TKIN"))
            }else{
              if(input$siteId=="TOOK_outlet"){
                sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
                  dplyr::filter(stringr::str_detect(regressionID,"TKOT"))
              }
            }
            sdrc_gaugePressureRelationship$date <- base::paste0(base::as.Date(sdrc_gaugePressureRelationship$gaugeCollectDate)," 20:00:00")
            sdrc_gaugePressureRelationship$date <- base::as.character(sdrc_gaugePressureRelationship$date)
            sdrc_gaugePressureRelationship$gauge_Height <- sdrc_gaugePressureRelationship$gaugeHeight
            sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
              dplyr::select(gauge_Height, date)
          }
        }

        if(site!="TOMB"){
          #creating summary table for variables and  uncertainties to be included
          continuousDischarge_sum <- csd_continuousDischarge%>%
            dplyr::group_by(date)%>%
            dplyr::summarize(meanQ=base::mean(maxpostDischarge,na.rm = T),
                             meanH=base::mean(equivalentStage,na.rm = T),
                             meanHUnc=base::mean(stageUnc,na.rm = T),
                             meanURemnUnc=base::mean(withRemnUncQUpper2Std,na.rm = T),
                             meanLRemnUnc=base::mean(withRemnUncQLower2Std,na.rm = T),
                             meanUParaUnc=base::mean(withParaUncQUpper2Std,na.rm = T),
                             meanLParaUnc=base::mean(withParaUncQLower2Std,na.rm = T),
                             dischargeFinalQF=base::sum(dischargeFinalQF,na.rm = T),
                             dischargeFinalQFSciRvw=base::sum(dischargeFinalQFSciRvw,na.rm = T))%>%
            dplyr::mutate(meanLHUnc=meanH-meanHUnc,
                          meanUHUnc=meanH+meanHUnc)
          continuousDischarge_sum$date <- base::as.character(continuousDischarge_sum$date)

          # Mutate the QF fields for plotting - QF will only be plotted if >20% records in mean are flagged
          continuousDischarge_sum$dischargeFinalQF[continuousDischarge_sum$dischargeFinalQF<4] <- 0
          continuousDischarge_sum$dischargeFinalQF[continuousDischarge_sum$dischargeFinalQF>=4] <- base::max(continuousDischarge_sum$meanURemnUnc,na.rm = T)
          continuousDischarge_sum$dischargeFinalQFSciRvw[continuousDischarge_sum$dischargeFinalQFSciRvw<4] <- 0
          continuousDischarge_sum$dischargeFinalQFSciRvw[continuousDischarge_sum$dischargeFinalQFSciRvw>=4] <- base::max(continuousDischarge_sum$meanURemnUnc,na.rm = T)

          #joining gauge discharge vars to continuous summary table
          continuousDischarge_sum <- dplyr::full_join(continuousDischarge_sum, sdrc_gaugeDischargeMeas, by="date")

          #joining guagepressure to  continuoussummary table
          if(!base::is.null(sdrc_gaugePressureRelationship)){
            continuousDischarge_sum <- dplyr::full_join(continuousDischarge_sum, sdrc_gaugePressureRelationship, by="date")
          }else{
            continuousDischarge_sum$gauge_Height <- NA
          }

          # Subset the summary data frame to only those records in the selected date range
          continuousDischarge_sum <- continuousDischarge_sum%>%
            dplyr::filter(date>=startDate&date<=endDate)

          # Create a vector of unique rating curve IDs
          curveIDs <- base::unique(csd_continuousDischarge$curveID)
        }else{
          continuousDischarge_sum <- csd_continuousDischarge%>%
            dplyr::mutate(meanQ=usgsDischarge,
                          meanH=NA,
                          meanLHUnc=NA,
                          meanUHUnc=NA,
                          meanUParaUnc=withRegressionUncQUpper2Std,
                          meanLParaUnc=withRegressionUncQLower2Std,
                          meanURemnUnc=NA,
                          meanLRemnUnc=NA,
                          dischargeFinalQF=NA,
                          streamDischarge=NA,
                          gaugeHeight=NA,
                          gauge_Height=NA)
          curveIDs <- NA
        }

        # Make an output list
        continuousDischarge_list <- base::list(
          continuousDischarge_sum,
          curveIDs
        )
        return(continuousDischarge_list)

      })#end of withProgress
      

    },ignoreInit = T)# End getPackage

    # Plotting continuous discharge with uncertainty
    output$plot1 <- plotly::renderPlotly({
      
      # Unpack the data frame from getPackage
      continuousDischarge_list <- getPackage()
      continuousDischarge_sum <- continuousDischarge_list[[1]]
      
      # Build plot layout
      method <- plotly::plot_ly(data=continuousDischarge_sum, source = "phenoDate")%>% 
        layout(
          xaxis=list(tick=14,
                     automargin=T,
                     title="Date",
                     tickfont=list(size=16),
                     titlefont=list(size=18),
                     range=c(base::format(shiny::isolate({input$dateRange[1]}) ),base::format(shiny::isolate({input$dateRange[2]}) ))),
          yaxis=list(side='left',
                     automargin=T,
                     title='Discharge (liters per second)',
                     tickfont=list(size=16),
                     titlefont=list(size=18),
                     showgrid=F,
                     zeroline=F),
          yaxis2=list(side='right',
                      overlaying="y",
                      automargin=T,
                      title="Stage (meter)",
                      tickfont=list(size=16),
                      titlefont=list(size=18),
                      showgrid=F,
                      zeroline=F),
          legend=list(x=-0.2,y=0.87,
                      font=list(size=14)),
          updatemenus=list(
            list(
              type='buttons',
              buttons=list(
                list(label='Scale Discharge\n- Linear -',
                     method='relayout',
                     args=list(list(yaxis=list(type='linear',
                                               title='Discharge (liters per second)',
                                               tickfont=list(size=16),
                                               titlefont=list(size=18),
                                               showgrid=F,
                                               zeroline=F)))),
                list(label='Scale Discharge\n- Log -',
                     method='relayout',
                     args=list(list(yaxis=list(type='log',
                                               title='Discharge (liters per second) - log',
                                               tickfont=list(size=16),
                                               titlefont=list(size=18),
                                               showgrid=F,
                                               zeroline=F))))))))
      


      # Add Quality flags
      if(input$qctrFlag == TRUE){
        method <- method %>%
          plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQF,type='scatter',mode='none',fill = 'tozeroy',showlegend= F, hoverinfo="none", fillcolor = 'lightgray')
      }
      if(input$qctrFlagScRv == TRUE){
        method <- method %>%
          plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQFSciRvw,type='scatter',mode='none',fill = 'tozeroy',hoverinfo="none", showlegend= F, fillcolor = 'lightgray')
      }
      
      
      # Add base plot
      method <- method %>%
        # Q Uncertainty
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanURemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='line',line=list(color='red'),showlegend=F,legendgroup='group1')%>%
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLRemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'red',showlegend=T,legendgroup='group1')%>%
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='line',line=list(color='lightpink'),showlegend=F,legendgroup='group2')%>%
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightpink',showlegend=T,legendgroup='group2')%>%

        # H Uncertainty
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUHUnc,name="Stage\nUncertainty",type='scatter',mode='line',line=list(color='lightblue'),yaxis='y2',showlegend=F,legendgroup='group3')%>%
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLHUnc,name="Stage\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightblue',yaxis='y2',showlegend=T,legendgroup='group3')%>%

        # H and Q Series
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanQ, name="Continuous\nDischarge",type='scatter',mode='lines',line = list(color = 'black'),showlegend=T,legendgroup='group4')%>%
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanH, name="Continuous\nStage",type='scatter',mode='lines',line = list(color = 'blue'),yaxis='y2',showlegend=T,legendgroup='group5')%>%

        # Empirical H and Q
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~streamDischarge,name="Measured\nDischarge", type='scatter', mode='markers',marker = list(color = 'purple',size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group6')%>%
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gaugeHeight,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=list(color="orange",size=8,line = list(color = "black",width = 1)),showlegend=F,legendgroup='group7')%>%
        plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gauge_Height,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=list(color="orange",size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group7')
      
    })# End plot1

    # method

    # Plotting rating curve(s) with uncertainty
    output$plot2 <- plotly::renderPlotly({

      # Unpack the list of curve IDs from getPackage
      continuousDischarge_list <- getPackage()
      curveIDs <- continuousDischarge_list[[2]]

      if(!is.na(curveIDs)){
        # Get the data for plotting
        utils::download.file(
          "https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/master/shinyApp/rcPlottingData.rds",
          "rcPlottingData.rds",
          method = "curl"
        )
        rcPlotData <- base::readRDS("rcPlottingData.rds")
        rcData <- rcPlotData$rcData%>%
          dplyr::filter(curveID%in%curveIDs)
        rcGaugings <- rcPlotData$rcGaugings%>%
          dplyr::filter(curveID%in%curveIDs)
        base::rm("rcPlottingData.rds")

        # Build plot layout
        rcPlot <- plotly::plot_ly(data=rcData)%>%
          layout(
            xaxis=list(tick=14,
                       automargin=T,
                       title="Stage (meter)",
                       tickfont=list(size=16),
                       titlefont=list(size=18)),
            yaxis=list(automargin=T,
                       title="Discharge (liters per second)",
                       range=c(0,base::max(rcData$totalUBottom)*1.05),
                       tickfont=list(size=16),
                       titlefont=list(size=18),
                       showgrid=T,
                       zeroline=T),
            legend=list(x=-0.2,y=0.87,
                        font=list(size=14)),
            updatemenus=list(
              list(
                type='buttons',
                buttons=list(
                  list(label='Scale Discharge\n- Linear -',
                       method='relayout',
                       args=list(list(yaxis=list(type='linear',
                                                 title="Discharge (liters per second)",
                                                 range=c(0,base::max(rcData$totalUBottom)*1.05),
                                                 tickfont=list(size=16),
                                                 titlefont=list(size=18),
                                                 showgrid=T,
                                                 zeroline=T)))),
                  list(label='Scale Discharge\n- Log -',
                       method='relayout',
                       args=list(list(yaxis=list(type='log',
                                                 title="Discharge (liters per second) - log",
                                                 range=c(0,base::log10(base::max(rcData$totalUBottom)*1.05)),
                                                 tickfont=list(size=16),
                                                 titlefont=list(size=18),
                                                 showgrid=T,
                                                 zeroline=T))))))))

        # Add each rating curve based on the vector of unique rating curve IDs
        for(i in 1:length(unique(rcData$curveID))){
          currentCurveID <- unique(rcData$curveID)[i]
          rcData_curveID <- rcData%>%
            filter(curveID==currentCurveID)
          rcGaugings_curveID <- rcGaugings%>%
            filter(curveID==currentCurveID)
          rcPlot <- rcPlot%>%
            # Total Uncertainty
            plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=list(color='red'),showlegend=F,legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
            plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='red',line=list(color='red'),showlegend=T,legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
            # Parametric Uncertainty
            plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=list(color='lightpink'),showlegend=F,legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
            plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='lightpink',line=list(color='lightpink'),showlegend=F,legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
            # Max Post Q
            plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~maxPostQ,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='line',line=list(color='black'),showlegend=T,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))%>%
            # Empirical H/Q Pairs
            plotly::add_trace(data=rcGaugings_curveID,x=~H,y=~Q,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='markers',marker=list(color='black'),showlegend=F,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))
        }
      }else{
        rcPlot <- plotly::plotly_empty()%>%
          layout(
            title=list(
              text = "No Stage-discharge rating curves (DP4.00133.001)\n data available for this site.",
              yref = "paper",
              y = 0.5,
              font=list(size=28)
            )
          )
      }

      rcPlot

    })# End plot2

  }#end of server

  # Run the app ----
  shiny::shinyApp(ui = ui, server = server)

}



