#instaling and loading dependencies1

# install.packages('shinythemes')
# install.packages('ecocomDP')
# install.packages('plotly')
#install.packages('readr')
# library(shiny)
# 
#  library(dplyr)
# # library(devtools)
# library(tidyverse)
# # library(ecocomDP)
# library(readr)
#  library(plotly)
# library(shinyWidgets)
# library(shinythemes)
#-----------------------------------------------

library(dplyr)
library(tidyverse)
library(readr)
library(plotly)
library(neonUtilities)
library(shinyWidgets)
library(stageQCurve)
library(lubridate, warn.conflicts = FALSE)
options(stringsAsFactors = F)


productList <- read.csv("aqu_dischargeDomainSiteList.csv")



  # user interface
  ui <- fluidPage(
    #background color
    # shinyWidgets::setBackgroundColor(color = "#e6f5ff", 727477
    #                                  gradient = "linear",
    #                                  direction = "bottom"),
    
    shiny::titlePanel("Customize  Discharge Time Series "),
    
    shiny::sidebarLayout(
      
      
      shiny::sidebarPanel(
               
        
          id = "form",
        
          selectInput(inputId ="domainId", "Domain ID",
                      
                      choices =productList$Domain),
                  
          selectInput("siteId", "Select Site ID", choices = NULL ),

          dateRangeInput("dateRange", "Date range:",
                         startview = "decade",
                         min    = "2013-01-01",
                         start = "2018-10-01", end = "2019-09-30", 
          ),
          
      
          #textOutput('startDate'),
    
          actionButton(inputId="submit", "Submit", class = "btn-primary"),
        
    
        
        shiny::br(),
        
        
        shiny::p(),
      ), #end of sidebarPanel
      
      shiny::mainPanel(
        
      plotlyOutput('plott')
      ) # end mainPanel
    )# end of sidebarLayout
    
  ) # end of ui and fluidPage
  


  
  #server function
  server <- function(session, input, output) {
    
    observe({
        x <- productList$Site.Code[productList$Domain == input$domainId]
        updateSelectInput(session,"siteId",choices = unique(x))
        
      })
    # 
    output$startDate <- renderText(
      paste(input$dateRange[1])
    )

    
    #downloading data package from Neon portal storing in getPackage()
    getPackage <- eventReactive(input$submit, {
   
      #setting vars
      site <- input$siteId
      endDate <- format(input$dateRange[1])
      startDate <- format(input$dateRange[2])
      
    # Get continuous discharge data from the NEON API
      # Rating curve data queries need to span an entire water year to ensure we are getting all the appropriate data
      searchIntervalStartDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = startDate)$startDate)
      searchIntervalEndDate <- as.character(stageQCurve::def.calc.WY.strt.end.date(searchIntervalStartDate = endDate)$endDate)
      
      # Get continuous discharge data from the NEON API
      DP4.00130.001<- neonUtilities::loadByProduct(
        dpID="DP4.00130.001",
        package = "expanded",
        check.size = F,
        site = site,
        startdate = format(as.POSIXct(startDate),"%Y-%m"),
        enddate = format(as.POSIXct(startDate),"%Y-%m")
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

      # ## trigger GET DATA NEON API call
      # data <- get_data()
      
      ## move to "Processing data..." AFTER get_data() completes
     # incProgress(amount = 0.5, message = "Processing data...")
      
      #extract date and site in gaugeDischargeMeas from DP4.00133.001
      sdrc_gaugeDischargeMeas <- DP4.00133.001$sdrc_gaugeDischargeMeas%>%
        separate(gaugeEventID,c("site","date"),5,remove = F)%>%
        mutate(date=paste0(as.Date(date,format="%Y%m%d")," 20:00:00"))
      
      #extract continuous discharge data and gauge-pressure relationship data from DP4.00130.001
      csd_continuousDischarge <- DP4.00130.001$csd_continuousDischarge
      sdrc_gaugePressureRelationship <- DP4.00130.001$sdrc_gaugePressureRelationship
      
      # rounding endDate to the nearest 20 minute minute
      csd_continuousDischarge$roundDate <- lubridate::round_date(csd_continuousDischarge$endDate, "20 mins")
      sdrc_gaugePressureRelationship$newDate <- lubridate::round_date(sdrc_gaugePressureRelationship$endDate, "20 mins")
      
      #creating summary table for variables and  uncertainties to be included
      continuousDischarge_sum <- csd_continuousDischarge%>%
        group_by(roundDate)%>%
        summarize(meanQ=mean(maxpostDischarge,na.rm = T),
                  meanH=mean(equivalentStage,na.rm = T),
                  meanHUnc=mean(stageUnc,na.rm = T),
                  meanURemnUnc=mean(withRemnUncQUpper2Std,na.rm = T),
                  meanLRemnUnc=mean(withRemnUncQLower2Std,na.rm = T),
                  meanUParaUnc=mean(withParaUncQUpper2Std,na.rm = T),
                  meanLParaUnc=mean(withParaUncQLower2Std,na.rm = T))%>%
        mutate(meanLHUnc=meanH-meanHUnc,
               meanUHUnc=meanH+meanHUnc)
      
      
      
      sdrc_gaugeDischargeMeas$date <- as.POSIXct(sdrc_gaugeDischargeMeas$date)
      continuousDischarge_sum$roundDate <- as.POSIXct(continuousDischarge_sum$roundDate)
      sdrc_gaugePressureRelationship$newDate <- as.POSIXct(sdrc_gaugePressureRelationship$newDate)
      
      #changing var name guageHeight of guagePressureRelationship
      sdrc_gaugePressureRelationship$guage_Height <- sdrc_gaugePressureRelationship$gaugeHeight 
      sdrc_gaugePressureRelationship <- sdrc_gaugePressureRelationship %>% 
        select(guage_Height, newDate)
      
      
      #joining gauge discharge vars to continuous summary table
      continuousDischarge_sum <- full_join(continuousDischarge_sum, sdrc_gaugeDischargeMeas, by = c("roundDate" = "date")) %>% 
        select(roundDate, meanH, meanQ, meanHUnc, meanURemnUnc,meanLRemnUnc,
               meanUParaUnc,meanLParaUnc,meanLHUnc,meanUHUnc, gaugeHeight,streamDischarge)
      
      #joining guagepressure to  continuoussummary table
      continuousDischarge_sum <- full_join(continuousDischarge_sum, sdrc_gaugePressureRelationship, by =c("roundDate" = "newDate")) 
      
      return(continuousDischarge_sum)

    }) ##### getPackage
    
    
    
     
      
   
 
    


    #plotting with uncertainty
    output$plott <- renderPlotly({
      continuousDischarge_sum <- getPackage()
      
       plot_ly(data=continuousDischarge_sum)%>%
      
      # Q Uncertainty
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanURemnUnc,name="Q: Remn Unc Top",type='scatter',mode='line',line=list(color='red'),showlegend=T,legendgroup='group1')%>%
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanLRemnUnc,name="Q: Remn Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'red',showlegend=T,legendgroup='group1')%>%
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanUParaUnc,name="Q: Para Unc Top",type='scatter',mode='line',line=list(color='lightpink'),showlegend=T,legendgroup='group1')%>%
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanLParaUnc,name="Q: Para Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightpink',showlegend=T,legendgroup='group1')%>%
      
      # H Uncertainty
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanUHUnc,name="H: Unc Top",type='scatter',mode='line',line=list(color='lightgreen'),yaxis='y2',showlegend=T,legendgroup='group2')%>%
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanLHUnc,name="H: Unc Bottom",type='scatter',mode='none',fill = 'tonexty',fillcolor = 'lightgreen',yaxis='y2',showlegend=T,legendgroup='group2')%>%
      
      # H and Q Series
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanQ, name="Q: Flow Series",type='scatter',mode='lines',line = list(color = 'black'),showlegend=T,legendgroup='group3')%>%
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~meanH, name="H: Stage Series",type='scatter',mode='lines',line = list(color = 'green'),yaxis='y2',showlegend=T,legendgroup='group4')%>%
      
      # Empirical H and Q
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~streamDischarge,name="Q: Measured", type='scatter', mode='markers',marker = list(color = 'blue',size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group5')%>%
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~guage_Height,name='H: Measured (RC)',type='scatter',mode='markers',yaxis='y2',marker=list(color="purple",size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group6')%>%
      add_trace(x=~as.POSIXct(roundDate,format="%Y-%m-%d %H:%M:%S"),y=~guage_Height,name='H: Measured Guage Pressure',type='scatter',mode='markers',yaxis='y2',marker=list(color="orange",size=8,line = list(color = "black",width = 1)),showlegend=T,legendgroup='group6')%>%
      
      
      
      
      layout( #title = paste0(site," -- Continuous Discharge Time Series"),
             xaxis=list(tick=14,title="Date Time", range=c(format(input$dateRange[1]),format(input$dateRange[2])) ), #setting limit for  continuous discharge on the x-axis
             yaxis=list(side='left',
                        title='Discharge (lps)',
                        showgrid=FALSE,
                        zeroline=FALSE),
             yaxis2=list(side='right',
                         overlaying="y",
                         title='Stage (m)',
                         showgrid=FALSE,
                         zeroline=FALSE),
             #-------------
             updatemenus=list(
               list(
                 type='buttons',
                 buttons=list(
                   list(label='linear',
                        method='relayout',
                        args=list(list(yaxis=list(type='linear')))),
                   list(label='log',
                        method='relayout',
                        args=list(list(yaxis=list(type='log')))))))
      )#end of layout
    #create an html plot
   # htmlwidgets::saveWidget(as_widget(plott),paste0(site,"_continuousQ_allWYs.html"))



      
      
      
      
    })
     







    
  }#end of server
    
    
  # Run the app ----
  shiny::shinyApp(ui = ui, server = server)