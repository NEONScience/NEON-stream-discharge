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
library(lubridate, warn.conflicts = FALSE)
options(stringsAsFactors = F)

# Read in NEON site and domain list
setwd("~/Github/NEON-stream-discharge-divine/L4Discharge/AOSApp")
productList <- read.csv("aqu_dischargeDomainSiteList.csv")

# user interface
ui <- fluidPage(
  shiny::titlePanel("NEON Continous discharge (DP4.00130.001) and Stage-discharge rating curves (DP4.00133.001) data visualization application"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(id = "form",
      selectInput("domainId","Domain ID",productList$Domain),
      selectInput("siteId","Select Site ID",NULL),
      dateRangeInput("dateRange","Date range:",
                     startview="month",
                     min="2016-01-01",
                     start="2019-01-01",end="2019-01-31", 
                     format="yyyy-mm-dd"),
      actionButton(inputId="submit","Submit"),
      shiny::br(),
      shiny::p(),
      width = 3
    ), #end of sidebarPanel
    shiny::mainPanel(plotlyOutput("plott",height="900px")) # end mainPanel
  )# end of sidebarLayout
) # end of ui and fluidPage
  
#server function
server <- function(session, input, output) {
  shiny::observe({x <- productList$Site.Code[productList$Domain == input$domainId]
           updateSelectInput(session,"siteId",choices = unique(x))})

  getPackage <- shiny::eventReactive(input$submit,{
  
    # site <- "WALK"
    # startDate <- "2019-01-01"
    # endDate <- "2019-12-31"
    site <- input$siteId
    startDate <- format(input$dateRange[1])
    endDate <- format(input$dateRange[2])
    # Rating curve data queries need to span an entire water year to ensure we are getting all the appropriate data
    searchIntervalStartDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = startDate)$startDate)
    searchIntervalEndDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = endDate)$endDate)

    # Get continuous discharge data from the NEON API
    DP4.00130.001 <- neonUtilities::loadByProduct(
      dpID="DP4.00130.001",
      package = "expanded",
      check.size = F,
      site = site,
      startdate = format(as.POSIXct(startDate),"%Y-%m"),
      enddate = format(as.POSIXct(endDate),"%Y-%m")
    )

    # Get rating curve data from the NEON API
    DP4.00133.001 <- neonUtilities::loadByProduct(
      dpID="DP4.00133.001",
      package = "basic",
      check.size = F,
      site = site,
      startdate = searchIntervalStartDate,
      enddate = searchIntervalEndDate
    )

    # Format gauge-discharge measurement data
    sdrc_gaugeDischargeMeas <- DP4.00133.001$sdrc_gaugeDischargeMeas%>%
      tidyr::separate(gaugeEventID,c("site","date"),5,remove = F)%>%
      dplyr::mutate(date=paste0(as.Date(date,format="%Y%m%d")," 20:00:00"))%>%
      dplyr::select(date,gaugeHeight,streamDischarge)
    sdrc_gaugeDischargeMeas$date <- as.character(sdrc_gaugeDischargeMeas$date)

    # Format continuous discharge data
    csd_continuousDischarge <- DP4.00130.001$csd_continuousDischarge
    csd_continuousDischarge$date <- lubridate::round_date(csd_continuousDischarge$endDate, "20 mins")

    # Format gauge-pressure relationship data
    sdrc_gaugePressureRelationship <- DP4.00130.001$sdrc_gaugePressureRelationship
    if(!is.null(sdrc_gaugePressureRelationship)){
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
                       meanLParaUnc=mean(withParaUncQLower2Std,na.rm = T))%>%
      dplyr::mutate(meanLHUnc=meanH-meanHUnc,
                    meanUHUnc=meanH+meanHUnc)
    continuousDischarge_sum$date <- as.character(continuousDischarge_sum$date)

    #joining gauge discharge vars to continuous summary table
    continuousDischarge_sum <- full_join(continuousDischarge_sum, sdrc_gaugeDischargeMeas, by="date")

    #joining guagepressure to  continuoussummary table
    if(!is.null(sdrc_gaugePressureRelationship)){
      continuousDischarge_sum <- dplyr::full_join(continuousDischarge_sum, sdrc_gaugePressureRelationship, by="date")
    }else{
      continuousDischarge_sum$gauge_Height <- NA
    }

    return(continuousDischarge_sum)
    
  },ignoreInit = T)# End getPackage    

  #plotting with uncertainty
  output$plott <- renderPlotly({
    continuousDischarge_sum <- getPackage()

    plot_ly(data=continuousDischarge_sum)%>%
    
    # Q Uncertainty
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanURemnUnc,name="Q: Remn Unc Top",type='scatter',mode='line',line=list(color='red'),showlegend=T,legendgroup='group1')%>%
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLRemnUnc,name="Q: Remn Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'red',showlegend=T,legendgroup='group1')%>%
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUParaUnc,name="Q: Para Unc Top",type='scatter',mode='line',line=list(color='lightpink'),showlegend=T,legendgroup='group1')%>%
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLParaUnc,name="Q: Para Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightpink',showlegend=T,legendgroup='group1')%>%
    
    # H Uncertainty
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUHUnc,name="H: Unc Top",type='scatter',mode='line',line=list(color='lightgreen'),yaxis='y2',showlegend=T,legendgroup='group2')%>%
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLHUnc,name="H: Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightgreen',yaxis='y2',showlegend=T,legendgroup='group2')%>%
    
    # H and Q Series
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanQ, name="Q: Flow Series",type='scatter',mode='lines',line = list(color = 'black'),showlegend=T,legendgroup='group3')%>%
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanH, name="H: Stage Series",type='scatter',mode='lines',line = list(color = 'green'),yaxis='y2',showlegend=T,legendgroup='group4')%>%
    
    # Empirical H and Q
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~streamDischarge,name="Q: Measured", type='scatter', mode='markers',marker = list(color = 'blue',size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group5')%>%
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gaugeHeight,name='H: Measured (RC)',type='scatter',mode='markers',yaxis='y2',marker=list(color="purple",size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group6')%>%
    add_trace(x=~as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gauge_Height,name='H: Measured Guage Pressure',type='scatter',mode='markers',yaxis='y2',marker=list(color="orange",size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group6')%>%
    
    layout(
      xaxis=list(tick=14,
                 automargin=T,
                 title="Date",
                 tickfont=list(size=16),
                 titlefont=list(size=18),
                 range=c(startDate,endDate)),
                 # range=c(format(input$dateRange[1]),format(input$dateRange[2]))),
      yaxis=list(side='left',
                 automargin=T,
                 title='Q (lps)',
                 tickfont=list(size=16),
                 titlefont=list(size=18),
                 showgrid=FALSE,
                 zeroline=FALSE),
      yaxis2=list(side='right',
                  overlaying="y",
                  automargin=T,
                  title="H (m)",
                  tickfont=list(size=16),
                  titlefont=list(size=18),
                  showgrid=FALSE,
                  zeroline=FALSE),
      legend=list(orientation="h",
                  x=0.5,y=1,
                  xanchor="center",
                  font=list(size=14)),
      updatemenus=list(
             list(
               type='buttons',
               buttons=list(
                 list(label='Scale Q - Linear',
                      method='relayout',
                      args=list(list(yaxis=list(type='linear',title='Q (lps)',titlefont=list(size=18))))),
                 list(label='Scale Q - Log',
                      method='relayout',
                      args=list(list(yaxis=list(type='log',title='Q (lps) - log',titlefont=list(size=18)))))))))
  })
}#end of server

# Run the app ----
shiny::shinyApp(ui = ui, server = server)