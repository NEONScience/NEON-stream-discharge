##############################################################################################
#' @title YOUR TITLE

#' @author
#' YOUR NAME \email{EMAIL@battelleecology.org} \cr

#' @description BRIEF DESCRIPTION

#' @return OUTPUT DESCRIPTION

# changelog and author contributions / copyrights
#   YOUR NAME (YYYY-MM-DD)
#     original creation
##############################################################################################

# Source packages and set options
library(dplyr)
library(tidyverse)
library(readr)
library(plotly)
library(neonUtilities)
library(shinyWidgets)
library(stageQCurve)
library(DT)
library(shinycssloaders)
library(lubridate, warn.conflicts = FALSE)
options(stringsAsFactors = F)

# Read in NEON site and domain list
setwd("~/Github/NEON-stream-discharge/L4Discharge/AOSApp")
productList <- read.csv("aqu_dischargeDomainSiteList.csv")

#https://data.neonscience.org/data-products/DP4.00133.001
# 
# URL for DP4.00130.001
# https://data.neonscience.org/data-products/DP4.00130.001


# user interface
ui <- fluidPage(style = "padding:25px; margin-bottom: 30px;",
                shiny::titlePanel("NEON Continous discharge (DP4.00130.001)and Stage-discharge rating curves (DP4.00133.001)) data visualization application"),
                fluidRow(
                  column(3,  
                         fluidRow("Welcome! This application allows you view and interact with NEON's Continuous discharge",tags$a(href="https://data.neonscience.org/data-products/DP4.00130.001", "(DP4.00130.001)", target="_blank"), "and Stage-discharge rating curves",tags$a(href="https://data.neonscience.org/data-products/DP4.00133.001", "(DP4.00133.001)", target="_blank")," data products. Select a site and date range and the app will download data from the NEON Data Portal and plot continuous and discrete stage and discharge timeseries data and all rating curves used in the development of the timeseries data."),
                         fluidRow(style = "background-color:#F8F8F8; height:auto;margin-top: 15px;padding: 15px;",
                                  selectInput("domainId","Domain ID",productList$Domain),
                                  selectInput("siteId","Select Site ID",NULL),
                                  dateRangeInput("dateRange","Date range:",
                                                 startview="month",
                                                 min="2016-00-01",
                                                 start="2018-04-01",end="2018-11-01", 
                                                 format="yyyy-mm-dd"),
                                  
                                  actionButton(inputId="submit","Submit"),
                                  checkboxInput("qctrFlag", "Include Final Quality Flag", FALSE),
                                  checkboxInput("qctrFlagScRv", "Include Science Review Quality Flag", FALSE)
                         ),
                         shiny::hr(),
                         fluidRow(
                           uiOutput("siteInfo" )
                         ),
                         shiny::hr(),
                         fluidRow(
                           textOutput("title"),
                           DT::dataTableOutput("table")
                         )
                  ),#end of first col
                  column(9,
                         tabsetPanel(type = "tabs",
                                     tabPanel("Continuous Discharge",withSpinner(plotlyOutput("plot1",height="800px"), color = "#00ADD7"), style = "background-color:#F8F8F8;"),
                                     tabPanel("Rating Curve(s)",withSpinner(plotlyOutput("plot2",height="800px"), color = "#00ADD7"))
                           )#end of tabsetPanel
                    )#end of second col
                  )#end of fluid row
  ) # end of ui and fluidPage


#server function
server <- function(session, input, output) {
  
  shiny::observe({x <- productList$Site.Code[productList$Domain == input$domainId]
  updateSelectInput(session,"siteId",choices = unique(x))
  })
  
  getPackage <- shiny::eventReactive(input$submit,{
    # Define site-specific metadata and site description for rendering
    metaD <-  productList%>%
      filter(Site.Code == input$siteId)%>%
      select(upstreamWatershedAreaKM2,reachSlopeM,averageBankfullWidthM,d50ParticleSizeMM)%>%
      rename("Upstream watershed area (km^2)"= upstreamWatershedAreaKM2,"Reach slope (m)" = reachSlopeM, "Mean bankfull width (m)"= averageBankfullWidthM, "D50 particle size (mm)"=d50ParticleSizeMM) %>% 
      pivot_longer(c("Upstream watershed area (km^2)","Reach slope (m)","Mean bankfull width (m)","D50 particle size (mm)"),names_to = "MetaData", values_to = "Values")
    # Enter header for metadata table
    output$title <- renderText("Metadata Table")
    # Create metadata table output
    output$table <- DT::renderDataTable({
      dat <- datatable(metaD,  options = list(dom = 't'))
      return(dat)
    },selection = 'single')
    # Create site description output
    
    href <- "https://www.neonscience.org/field-sites/"
    url <- a("Click here", href= href)
   # <a href="https://yourURL.com/code= [[Username|_4]]"
    
    output$siteInfo <- renderUI({
      tagList("Site: ",input$siteId, url,"for site description",  sep="\n")
    })
    
    # # Manually set input variables for local testing
    # input <- list()
    # input$siteId <- "CARI"
    # input$dateRange[[1]] <- "2019-01-01"
    # input$dateRange[[2]] <- "2019-12-31"

    # Set variables for app running (special consideration for TOOK)
    if (stringr::str_detect(input$siteId,"TOOK")) {
      site <- "TOOK"
    }else{
      site <- input$siteId
    }
    startDate <- format(input$dateRange[1])
    endDate <- format(input$dateRange[2])
    
    # Rating curve data queries need to span an entire water year to ensure we are getting all the appropriate data
    searchIntervalStartDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = startDate)$startDate)
    searchIntervalEndDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = endDate)$endDate)
    
    #progress bar for data downloads
    withProgress(message = 'Submit',detail = '', min = 0, max = 1 ,value = 0, {
      
      incProgress(amount = 0.33, message = "Downloading DP4.00130.001", detail = NULL,session = getDefaultReactiveDomain())
      Sys.sleep(0.25)
      
      # Get continuous discharge data from the NEON API
      DP4.00130.001 <- neonUtilities::loadByProduct(
        dpID="DP4.00130.001",
        package = "expanded",
        check.size = F,
        site = site,
        startdate = format(as.POSIXct(startDate),"%Y-%m"),
        enddate = format(as.POSIXct(endDate),"%Y-%m")
      )
      
      incProgress(amount = 0.33, message = "Downloading DP4.00133.001", detail = NULL,session = getDefaultReactiveDomain())
      Sys.sleep(0.25)
      
      # Get rating curve data from the NEON API
      DP4.00133.001 <- neonUtilities::loadByProduct(
        dpID="DP4.00133.001",
        package = "basic",
        check.size = F,
        site = site,
        startdate = searchIntervalStartDate,
        enddate = searchIntervalEndDate
      )
      
      #updating progress bar
      incProgress(amount = 0.33, message = "Processing data... ", detail = NULL, session = getDefaultReactiveDomain())
      Sys.sleep(0.25)
      
      # Format gauge-discharge measurement data
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
        dplyr::mutate(date=paste0(as.Date(date,format="%Y%m%d")," 20:00:00"))%>%
        dplyr::select(date,gaugeHeight,streamDischarge)
      sdrc_gaugeDischargeMeas$date <- as.character(sdrc_gaugeDischargeMeas$date)
      
      # Format continuous discharge data
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
      
      # Format gauge-pressure relationship data
      sdrc_gaugePressureRelationship <- DP4.00130.001$sdrc_gaugePressureRelationship
      if(!is.null(sdrc_gaugePressureRelationship)){
        if (input$siteId=="TOOK_inlet") {
          sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
            dplyr::filter(stringr::str_detect(regressionID,"TKIN"))
        }else{
          if(input$siteId=="TOOK_outlet"){
            sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
              dplyr::filter(stringr::str_detect(regressionID,"TKOT"))
          }
        }
        sdrc_gaugePressureRelationship$date <- paste0(as.Date(sdrc_gaugePressureRelationship$gaugeCollectDate)," 20:00:00")
        sdrc_gaugePressureRelationship$date <- as.character(sdrc_gaugePressureRelationship$date)
        sdrc_gaugePressureRelationship$gauge_Height <- sdrc_gaugePressureRelationship$gaugeHeight
        sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship%>%
          dplyr::select(gauge_Height, date)
      }
      
      #creating summary table for variables and  uncertainties to be included
      continuousDischarge_sum <- csd_continuousDischarge%>%
        dplyr::group_by(date)%>%
        dplyr::summarize(meanQ=mean(maxpostDischarge,na.rm = T),
                         meanH=mean(equivalentStage,na.rm = T),
                         meanHUnc=mean(stageUnc,na.rm = T),
                         meanURemnUnc=mean(withRemnUncQUpper2Std,na.rm = T),
                         meanLRemnUnc=mean(withRemnUncQLower2Std,na.rm = T),
                         meanUParaUnc=mean(withParaUncQUpper2Std,na.rm = T),
                         meanLParaUnc=mean(withParaUncQLower2Std,na.rm = T),
                         dischargeFinalQF=sum(dischargeFinalQF,na.rm = T),
                         dischargeFinalQFSciRvw=sum(dischargeFinalQFSciRvw,na.rm = T))%>%
        dplyr::mutate(meanLHUnc=meanH-meanHUnc,
                      meanUHUnc=meanH+meanHUnc)
      continuousDischarge_sum$date <- as.character(continuousDischarge_sum$date)
      
      # Mutate the QF fields for plotting - QF will only be plotted if >20% records in mean are flagged
      continuousDischarge_sum$dischargeFinalQF[continuousDischarge_sum$dischargeFinalQF<4] <- 0
      continuousDischarge_sum$dischargeFinalQF[continuousDischarge_sum$dischargeFinalQF>=4] <- max(continuousDischarge_sum$meanURemnUnc,na.rm = T)
      continuousDischarge_sum$dischargeFinalQFSciRvw[continuousDischarge_sum$dischargeFinalQFSciRvw<4] <- 0
      continuousDischarge_sum$dischargeFinalQFSciRvw[continuousDischarge_sum$dischargeFinalQFSciRvw>=4] <- max(continuousDischarge_sum$meanURemnUnc,na.rm = T)
      
      #joining gauge discharge vars to continuous summary table
      continuousDischarge_sum <- full_join(continuousDischarge_sum, sdrc_gaugeDischargeMeas, by="date")
      
      #joining guagepressure to  continuoussummary table
      if(!is.null(sdrc_gaugePressureRelationship)){
        continuousDischarge_sum <- dplyr::full_join(continuousDischarge_sum, sdrc_gaugePressureRelationship, by="date")
      }else{
        continuousDischarge_sum$gauge_Height <- NA
      }
      
      # Subset the summary data frame to only those records in the selected date range
      continuousDischarge_sum <- continuousDischarge_sum%>%
        dplyr::filter(date>=startDate&date<=endDate)
      
      # Create a vector of unique rating curve IDs
      curveIDs <- unique(csd_continuousDischarge$curveID)
      
      # Make an output list
      continuousDischarge_list <- list(
        continuousDischarge_sum,
        curveIDs        
      )
      return(continuousDischarge_list)
      
    })#end of withProgress
    
  },ignoreInit = T)# End getPackage    
  
  # Plotting continuous discharge with uncertainty
  output$plot1 <-renderPlotly({
    
    # Unpack the data frame from getPackage
    continuousDischarge_list <- getPackage()
    continuousDischarge_sum <- continuousDischarge_list[[1]]
    
    # Build plot layout
    method <- plot_ly(data=continuousDischarge_sum)%>%
      layout(
        xaxis=list(tick=14,
                   automargin=T,
                   title="Date",
                   tickfont=list(size=16),
                   titlefont=list(size=18),
                   range=c(format(isolate({input$dateRange[1]}) ),format(isolate({input$dateRange[2]}) ))),
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
        add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQF,type='scatter',mode='none',fill = 'tozeroy',showlegend= F, hoverinfo="none", fillcolor = 'lightgray')
    }
    if(input$qctrFlagScRv == TRUE){
      method <- method %>% 
        add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQFSciRvw,type='scatter',mode='none',fill = 'tozeroy',hoverinfo="none", showlegend= F, fillcolor = 'lightgray')
    }
    
    # Add base plot
    method <- method %>% 
      # Q Uncertainty
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanURemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='line',line=list(color='red'),showlegend=F,legendgroup='group1')%>%
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLRemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'red',showlegend=T,legendgroup='group1')%>%
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='line',line=list(color='lightpink'),showlegend=F,legendgroup='group2')%>%
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightpink',showlegend=T,legendgroup='group2')%>%
    
      # H Uncertainty
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUHUnc,name="Stage\nUncertainty",type='scatter',mode='line',line=list(color='lightgreen'),yaxis='y2',showlegend=F,legendgroup='group3')%>%
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLHUnc,name="Stage\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightgreen',yaxis='y2',showlegend=T,legendgroup='group3')%>%
    
      # H and Q Series
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanQ, name="Continuous\nDischarge",type='scatter',mode='lines',line = list(color = 'black'),showlegend=T,legendgroup='group4')%>%
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanH, name="Continuous\nStage",type='scatter',mode='lines',line = list(color = 'green'),yaxis='y2',showlegend=T,legendgroup='group5')%>%
    
      # Empirical H and Q
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~streamDischarge,name="Measured\nDischarge", type='scatter', mode='markers',marker = list(color = 'blue',size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group6')%>%
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gaugeHeight,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=list(color="purple",size=8,line = list(color = "black",width = 1)),showlegend=F,legendgroup='group7')%>%
      add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gauge_Height,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=list(color="purple",size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group7')
  })# End plot1
  
  # Plotting rating curve(s) with uncertainty
  output$plot2 <-renderPlotly({
    
    # Unpack the list of curve IDs from getPackage
    continuousDischarge_list <- getPackage()
    curveIDs <- continuousDischarge_list[[2]]
    
    # Get the data for plotting
    rcPlotData <- readRDS("rcPlottingData.rds")
    rcData <- rcPlotData$rcData%>%
      dplyr::filter(curveID%in%curveIDs)
    rcGaugings <- rcPlotData$rcGaugings%>%
      dplyr::filter(curveID%in%curveIDs)

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
                   range=c(0,max(rcData$totalUBottom)*1.05),
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
                                             range=c(0,max(rcData$totalUBottom)*1.05),
                                             tickfont=list(size=16),
                                             titlefont=list(size=18),
                                             showgrid=T,
                                             zeroline=T)))),
              list(label='Scale Discharge\n- Log -',
                   method='relayout',
                   args=list(list(yaxis=list(type='log',
                                             title="Discharge (liters per second) - log",
                                             range=c(0,log10(max(rcData$totalUBottom)*1.05)),
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
        add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUTop,name=paste0(gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=list(color='red'),showlegend=F,legendgroup=paste0(currentCurveID," Uncertainty"))%>%
        add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUBottom,name=paste0(gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='red',line=list(color='red'),showlegend=T,legendgroup=paste0(currentCurveID," Uncertainty"))%>%
        # Parametric Uncertainty
        add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUTop,name=paste0(gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=list(color='lightpink'),showlegend=F,legendgroup=paste0(currentCurveID," Uncertainty"))%>%
        add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUBottom,name=paste0(gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='lightpink',line=list(color='lightpink'),showlegend=F,legendgroup=paste0(currentCurveID," Uncertainty"))%>%
        # Max Post Q
        add_trace(data=rcData_curveID,x=~Hgrid,y=~maxPostQ,name=paste0(gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='line',line=list(color='black'),showlegend=T,legendgroup=paste0(currentCurveID," Rating Curve w/ Gaugings"))%>%
        # Empirical H/Q Pairs
        add_trace(data=rcGaugings_curveID,x=~H,y=~Q,name=paste0(gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='markers',marker=list(color='black'),showlegend=F,legendgroup=paste0(currentCurveID," Rating Curve w/ Gaugings"))
    }
    
    rcPlot

  })# End plot2

}#end of server

# Run the app ----
shiny::shinyApp(ui = ui, server = server)




