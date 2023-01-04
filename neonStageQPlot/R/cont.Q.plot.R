##############################################################################################
#' Plot NEON Hydrology Data Using Outputs from neonStageQPlot::get.cont.Q.NEON.API

#' @name cont.Q.plot

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr
#' James Ross \email{ross.james94@gmail.com} \cr

#' @description  This function will generate a plotly plot of NEON hydrology data to be
#' rendered in the openFlow shiny app. The function can also be used outside the app, but should
#' only be used with the outputs from neonStageQPlot::get.cont.Q.NEON.API.

#' @param site.id Required: NEON AQU site ID selected by the shiny app user [string]
#' @param start.date Required: Search interval start date (YYYY-MM-DD) selected by the shiny
#' app user [string]
#' @param end.date Required: Search interval end date (YYYY-MM-DD) selected by the shiny app
#' user [string]
#' @param input.list Required: List containing the data used in plotting; this list should be
#' the outputs from neonStageQPlot::get.cont.Q.NEON.API [list]
#' @param lookup.table Required: Data frame site-level metadata and constants [dataframe]
#' @param plot.imp.unit Defaults to FALSE: Idicator of plotting data in metric or imperial
#' units [boolean]
#' @param plot.sci.rvw.QF Dafults to FALSE: Indicator of plotting the finalDischargeQFSciRvw
#' field from the DP4.00130.001 data product [boolean]
#' @param plot.q.stats Defaults to FALSE: Include values for 3x median discharge and 25-75%
#' flow in the plot. Statistics are calculated from the time range selected by the user and
#' exclude records that contain a science review quality flag (dischargeFinalQFSciRvw) [boolean]
#' @param mode.dark Defaults to FALSE: Indicator of plotting data in light or dark mode. NOTE:
#' This argument is only used in the openFlow shiny app. [boolean]

#' @return Returns a plotly plot object

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @importFrom dplyr %>%

#' @export cont.Q.plot

# changelog and author contributions / copyrights
#   Zachary L. Nickerson (2022-07-25)
#     original creation
#   Zachary L. Nickerson (2022-08-09)
#     removed dependency on measurements
##############################################################################################
base::options(stringsAsFactors = F)
utils::globalVariables(c('histMedQ','meanURemnUnc','meanLRemnUnc','meanUParaUnc','meanLParaUnc','meanQ','streamDischarge','meanUHUnc','meanLHUnc','meanH','gaugeHeight','gauge_Height','priPrecipBulkLoUnc','priPrecipBulkUpUnc','priPrecipBulk','secPrecipBulkLoUnc','secPrecipBulkUpUnc','secPrecipBulk'))

cont.Q.plot <-function(site.id,
                       start.date,
                       end.date,
                       input.list,
                       lookup.table,
                       plot.imp.unit=F,
                       mode.dark=F,
                       plot.sci.rvw.QF=F,
                       plot.q.stats=F){

  # site.id=siteID
  # start.date=startDate
  # end.date=endDate
  # input.list=continuousDischarge_list
  # lookup.table=productList
  # plot.imp.unit=F
  # mode.dark=F
  # plot.sci.rvw.QF=F
  # plot.q.stats=F
  
  if(base::missing(site.id)){
    stop('must provide site.id for plotting continuous discharge')
  }
  if(base::missing(start.date)){
    stop('must provide start.date for plotting continuous discharge')
  }
  if(base::missing(end.date)){
    stop('must provide end.date for plotting continuous discharge')
  }
  if(base::missing(input.list)){
    stop('must provide input.list for plotting continuous discharge')
  }
  
  # Read in the lookup table and get constants
  
  # Q Stats
  ptpSiteID <- lookup.table$ptpSite[lookup.table$siteID==site.id]
  print("Fail here9")
  if(plot.q.stats){
    medQ <- lookup.table$threeXMedQ[lookup.table$siteID==site.id]
    medQUnc <- lookup.table$threeXMedQUnc[lookup.table$siteID==site.id]
  }

  # Subset continuous discharge data for plotting
  continuousDischarge_sum <- input.list$csd_continuousDischarge_allYears
  print("Fail here8")
  
  print("Fail here8.1")
  # Subset gauge~pressure relationship data for plotting
  sdrc_gaugePressureRelationship <- input.list$sdrc_gaugePressureRelationship_allYears
  
  print("Fail here8.2")
  # Subset gauge~discharge pair data for plotting
  sdrc_gaugeDischargeMeas <- input.list$sdrc_gaugeDischargeMeas_allYears
  
  print("Fail here8.3")
  # Subset precipitation data for plotting
  ptp <- input.list$ptp_allYears
  
  print("Fail here8.4")
  # Pull in historic median Q from GCS
  histMedQ <- base::readRDS(base::url("https://storage.neonscience.org/neon-geobath-files/NEON_MEDIAN_Q_SHINY_APP_THROUGH_WY2020_VC.rds","rb"))
  histMedQ <- histMedQ%>%
    dplyr::filter(siteID==site.id)
  continuousDischarge_sum$monthDay <- base::gsub("[0-9]{4}\\-","",continuousDischarge_sum$date)
  continuousDischarge_sum$histMedQ <- NA
  print("Fail here7")
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
  
  print("Fail here1")
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
  
  print("Fail here1.5")
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
  print("Fail here1.75")
  dischargeColor <- "black"
  if(mode.dark){
    method <- method %>%
      plotly::layout(paper_bgcolor="#222",plot_bgcolor='#222',
                     font = base::list(color = 'white'))
    dischargeColor <- "white"
  }
  print("Fail here2")
  
  # Add Quality flags
  if(plot.sci.rvw.QF){
    method <- method %>%
      plotly::add_trace(data=continuousDischarge_sum,x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQFSciRvw,type='scatter',mode='none',fill = 'tozeroy',hoverinfo="none", showlegend= F, fillcolor = 'lightgray')
  }
  print("Fail here3")
  
  # Add base plot 
  ### LEFT OFF HERE BECAUSE IT IS NOT RECOGNIZING THE COLUMNS IN THE DATA FRAME ###
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
  #plots whichever data is available using isPrimaryPtp bool
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
  print("Fail here4")
  
  return(method)
}
