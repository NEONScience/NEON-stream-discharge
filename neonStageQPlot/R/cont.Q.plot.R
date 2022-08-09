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
                       plot.imp.unit=F,
                       mode.dark=F,
                       # plot.final.QF=F,
                       plot.sci.rvw.QF=F,
                       # plot.precip.final.QF=F,
                       # plot.precip.sci.rvw.QF=F,
                       plot.q.stats=F){
  
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
  
  # Get data
  continuousDischarge_sum <- input.list$continuousDischarge_sum%>%
    dplyr::filter(!is.na(meanQ))
  isPrimaryPtp <- input.list$precipitationSite$isPrimaryPtp
  precipSiteID <- input.list$precipitationSite$gaugeID
  histMedQMinYear <- input.list$histMedQYearRange$minYear
  histMedQMaxYear <- input.list$histMedQYearRange$maxYear
  
  #3x internal dataGet#
  if(plot.q.stats){
    medQ <- base::as.numeric(input.list$dischargeStats$medQ)
    twentyFiveQ <- base::as.numeric(input.list$dischargeStats$twentyFiveQ)
    seventyFiveQ <- base::as.numeric(input.list$dischargeStats$seventyFiveQ)
  }
  
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
    continuousDischarge_sum <- continuousDischarge_sum %>%
      
      #Discharge
      dplyr::mutate(histMedQ = histMedQ*convLPStoCFS) %>%
      dplyr::mutate(meanURemnUnc = meanURemnUnc*convLPStoCFS) %>%
      dplyr::mutate(meanLRemnUnc = meanLRemnUnc*convLPStoCFS) %>%
      dplyr::mutate(meanUParaUnc = meanUParaUnc*convLPStoCFS) %>%
      dplyr::mutate(meanLParaUnc = meanLParaUnc*convLPStoCFS) %>%
      dplyr::mutate(meanQ = meanQ*convLPStoCFS) %>%
      dplyr::mutate(streamDischarge = streamDischarge*convLPStoCFS) %>%
      dplyr::mutate(dischargeFinalQFSciRvw = dischargeFinalQFSciRvw*convLPStoCFS) %>%

      #Stage
      dplyr::mutate(meanUHUnc = meanUHUnc*convMtoFt) %>%
      dplyr::mutate(meanLHUnc = meanLHUnc*convMtoFt) %>%
      dplyr::mutate(meanH = meanH*convMtoFt) %>%
      dplyr::mutate(gaugeHeight = gaugeHeight*convMtoFt) %>%
      dplyr::mutate(gauge_Height = gauge_Height*convMtoFt) %>%

    #Precipitation
    if(isPrimaryPtp){
      continuousDischarge_sum <- continuousDischarge_sum %>%
        dplyr::mutate(priPrecipBulkLoUnc = priPrecipBulkLoUnc*convMMtoIN) %>%
        dplyr::mutate(priPrecipBulkUpUnc = priPrecipBulkUpUnc*convMMtoIN) %>%
        dplyr::mutate(priPrecipBulk = priPrecipBulk*convMMtoIN)
    }else{
      continuousDischarge_sum <- continuousDischarge_sum %>%
        dplyr::mutate(secPrecipBulkLoUnc = secPrecipBulkLoUnc*convMMtoIN) %>%
        dplyr::mutate(secPrecipBulkUpUnc = secPrecipBulkUpUnc*convMMtoIN) %>%
        dplyr::mutate(secPrecipBulk = secPrecipBulk*convMMtoIN)
    }
    
    #3x internal
    if(plot.q.stats){
      medQ <- medQ*convLPStoCFS
      twentyFiveQ <- twentyFiveQ*convLPStoCFS
      seventyFiveQ <- seventyFiveQ*convLPStoCFS
    }
    
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
  method <- plotly::plot_ly(data=continuousDischarge_sum, source = "phenoDate")%>%
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
  # if(plot.final.QF){
  #   method <- method %>%
  #     plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQF,type='scatter',mode='none',fill = 'tozeroy',showlegend= F, hoverinfo="none", fillcolor = 'lightgray')
  # }
  if(plot.sci.rvw.QF){
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQFSciRvw,type='scatter',mode='none',fill = 'tozeroy',hoverinfo="none", showlegend= F, fillcolor = 'lightgray')
  }
  # #wraps precip flag data to prevent plotting when data does not exist
  # if(isPrimaryPtp & plot.precip.final.QF){
  #   method <- method %>%
  #     plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~priPrecipFinalQF,type='scatter',mode='none',fill = 'tozeroy',showlegend= F, hoverinfo="none", fillcolor = 'gray')
  # }
  #
  # #wraps precip flag data to prevent plotting when data does not exist
  # if(isPrimaryPtp==FALSE & plot.precip.sci.rvw.QF){
  #   method <- method %>%
  #     plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~secPrecipSciRvwQF,type='scatter',mode='none',fill = 'tozeroy',hoverinfo="none", showlegend= F, fillcolor = 'gray')
  # }
  
  # Add base plot
  method <- method %>%
    
    # Q Uncertainty
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanURemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='line',line=base::list(color='#D55E00'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group1',visible = "legendonly")%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLRemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#D55E00',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group1',visible = "legendonly")%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='line',line=base::list(color='#E69F00'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group2',visible = "legendonly")%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#E69F00',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group2',visible = "legendonly")%>%
    
    # H Uncertainty
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUHUnc,name="Stage\nUncertainty",type='scatter',mode='line',line=base::list(color='#56B4E9'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",yaxis='y2',showlegend=F,legendgroup='group3',visible = "legendonly")%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLHUnc,name="Stage\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#56B4E9',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",yaxis='y2',showlegend=T,legendgroup='group3',visible = "legendonly")%>%
    
    # H and Q Series
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanQ, name="Continuous\nDischarge",type='scatter',mode='lines',line = base::list(color = dischargeColor),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",yaxis='y1',legendgroup='group4')%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanH, name="Continuous\nStage",type='scatter',mode='lines',line = base::list(color = '#CC79A7'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",yaxis='y2',showlegend=T,legendgroup='group5')%>%
    
    # Empirical H and Q
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~streamDischarge,name="Measured\nDischarge", type='scatter', mode='markers',marker = base::list(color = '#009E73',size=8,line = base::list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group6')%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gaugeHeight,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=base::list(color="#F0E442",size=8,line = base::list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group7')%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gauge_Height,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=base::list(color="#F0E442",size=8,line = base::list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group7')%>%
    
    #Historical Med Q
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~histMedQ, name=stringr::str_c("Historic Median\nDischarge: ","\n",base::min(histMedQMinYear),"-",base::max(histMedQMaxYear)),type='scatter',mode='lines',line = base::list(color = 'grey'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",legendgroup='group8',visible = "legendonly")
  
  #Precipitation Data
  #plots whichever data is available using isPrimaryPtp bool
  if(isPrimaryPtp){
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~priPrecipBulkLoUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='line',line=base::list(color='#431A74'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group9',visible = "legendonly")%>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~priPrecipBulkUpUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#431A74',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group9',visible = "legendonly") %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~priPrecipBulk,name=stringr::str_c("Continuous\nPrecipitation\nSite: ",precipSiteID),yaxis = "y3",type='scatter',mode='lines',line = base::list(color = '#0072B2'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",legendgroup='group10',visible = "legendonly")
  }else{
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~secPrecipBulkLoUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='line',line=base::list(color='#431A74'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group9',visible = "legendonly")%>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~secPrecipBulkUpUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#431A74',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group9',visible = "legendonly") %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~secPrecipBulk,name=stringr::str_c("Continuous\nPrecipitation\nSite: ",precipSiteID),yaxis = "y3",type='scatter',mode='lines',line = base::list(color = '#0072B2'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",legendgroup='group10',visible = "legendonly")
  }
  
  # Add the internal parameters
  if(plot.q.stats){
    method <- method%>%
      plotly::add_segments(x=~base::min(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),xend=~base::max(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),y=~medQ,yend=~medQ,line=base::list(color='grey',dash='dash'),name="3x Median\nDischarge",showlegend=T,legendgroup='group11',visible = "legendonly")%>%
      plotly::add_segments(x=~base::min(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),xend=~base::max(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),y=~twentyFiveQ,yend=~twentyFiveQ,line=base::list(color=dischargeColor,dash='dash'),name="25-75%\nischarge",showlegend=F,legendgroup='group12',visible = "legendonly")%>%
      plotly::add_segments(x=~base::min(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),xend=~base::max(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),y=~seventyFiveQ,yend=~seventyFiveQ,line=base::list(color=dischargeColor,dash='dash'),name="25-75%\nDischarge",showlegend=T,legendgroup='group12',visible = "legendonly")
    
    if(!plot.imp.unit){
      method <- method%>%
        plotly::layout(
          title=list(text=base::paste0("<br><b>3x Median Discharge = ",base::round(medQ,digits = 0)," L/s<br>25-75% Discharge: ",base::round(twentyFiveQ,digits = 0)," - ",base::round(seventyFiveQ,digits = 0)," L/s<b>"),
                     xanchor="left",
                     xref="paper",
                     x=0.02))
    }else{
      method <- method%>%
        plotly::layout(
          title=list(text=base::paste0("<br><b>3x Median Discharge = ",base::round(medQ,digits = 0)," cfs<br>25-75% Discharge: ",base::round(twentyFiveQ,digits = 0)," - ",base::round(seventyFiveQ,digits = 0)," cfs<b>"),
                     xanchor="left",
                     xref="paper",
                     x=0.02))
    }
  }
  
  return(method)
}
