##############################################################################################
#' Plot NEON Continuous Discharge (DP4.00130.001) Data

#' @name plot.cont.Q

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr
#' James M. Ross \email{ross.james94@gmail.com} \cr

#' @description  This function will generate a plotly plot of NEON Continuous Discharge
#' (DP4.00130.001) data to be rendered in the discharge visualization shiny app

#' @param site.id Required: NEON AQU site ID selected by the shiny app user [string]
#' @param start.date Required: Search interval start date (YYYY-MM-DD) selected by the shiny
#' app user [string]
#' @param end.date Required: Search interval end date (YYYY-MM-DD) selected by the shiny app
#' user [string]
#' @param input.list Required: List containing the data used in plotting [list]
#' @param plot.final.QF Required: Indicator of plotting the finalDischargeQF field from the
#' DP4.00130.001 data product [boolean]
#' @param plot.sci.rvw.QF Required: Indicator of plotting the finalDischargeQFSciRvw field
#' from the DP4.00130.001 data product [boolean]

#' @return Returns a plotly plot object

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export plot.cont.Q

# changelog and author contributions / copyrights
#   Zachary L. Nickerson (2022-06-20)
#     original creation
#   James M. Ross (2022-07-01)
#     updated graph colors to be more colorblind friendly
#   James M. Ross (2022-07-06)
#     added historical medQ to graph
##############################################################################################
# # Source packages and set options
options(stringsAsFactors = F)

plot.cont.Q <-function(site.id,start.date,end.date,input.list,plot.final.QF,plot.sci.rvw.QF,plot.imp.unit){

  if(missing(site.id)){
    stop('must provide site.id for plotting continuous discharge')
  }
  if(missing(start.date)){
    stop('must provide start.date for plotting continuous discharge')
  }
  if(missing(end.date)){
    stop('must provide end.date for plotting continuous discharge')
  }
  if(missing(input.list)){
    stop('must provide input.list for plotting continuous discharge')
  }
  if(missing(plot.final.QF)){
    stop('must provide plot.final.QF for plotting continuous discharge')
  }
  if(missing(plot.sci.rvw.QF)){
    stop('must provide plot.sci.rvw.QF for plotting contninuous discharge')
  }
  if(missing(plot.imp.unit)){
    stop('must provide plot.imp.unit for plotting contninuous discharge')
  }

  # Get data
  continuousDischarge_sum <- input.list$continuousDischarge_sum
  histMedQMinYear <- input.list$histMedQYearRange$minYear
  histMedQMaxYear <- input.list$histMedQYearRange$maxYear

  #axis units
  y1Units <- "(liters per second)"
  y2Units <- "(meter)"

  #SI to imperial tied to button
  if(plot.imp.unit){
    continuousDischarge_sum <- continuousDischarge_sum %>%
      #Discharge
      mutate(histMedQImp = conv_unit(histMedQ,"l_per_sec","ft3_per_sec")) %>%
      mutate(meanURemnUnc = conv_unit(meanURemnUnc,"l_per_sec","ft3_per_sec")) %>%
      mutate(meanLRemnUnc = conv_unit(meanLRemnUnc,"l_per_sec","ft3_per_sec")) %>%
      mutate(meanUParaUnc = conv_unit(meanUParaUnc,"l_per_sec","ft3_per_sec")) %>%
      mutate(meanLParaUnc = conv_unit(meanLParaUnc,"l_per_sec","ft3_per_sec")) %>%
      mutate(meanQ = conv_unit(meanQ,"l_per_sec","ft3_per_sec")) %>%
      mutate(streamDischarge = conv_unit(streamDischarge,"l_per_sec","ft3_per_sec")) %>%

      #Stage
      mutate(meanUHUnc = conv_unit(meanUHUnc,"m","ft")) %>%
      mutate(meanLHUnc = conv_unit(meanLHUnc,"m","ft")) %>%
      mutate(meanH = conv_unit(meanH,"m","ft")) %>%
      mutate(gaugeHeight = conv_unit(gaugeHeight,"m","ft")) %>%
      mutate(gauge_Height = conv_unit(gauge_Height,"m","ft"))

    y1Units <- "(Cubic Feet per second)"
    y2Units <- "(feet)"
  }

  # Build plot layout
  method <- plotly::plot_ly(data=continuousDischarge_sum, source = "phenoDate")%>%
    layout(
      xaxis=list(tick=14,
                 automargin=T,
                 title="Date",
                 tickfont=list(size=16),
                 titlefont=list(size=18)#,
                 # range=c(base::format(shiny::isolate({input$dateRange[1]}) ),base::format(shiny::isolate({input$dateRange[2]}) ))
                 ),
      yaxis=list(side='left',
                 automargin=T,
                 title=str_c("Discharge ",y1Units),
                 tickfont=list(size=16),
                 titlefont=list(size=18),
                 showgrid=F,
                 zeroline=F),
      yaxis2=list(side='right',
                  overlaying="y",
                  automargin=T,
                  title=str_c("Stage ",y2Units),
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
  if(plot.final.QF){
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQF,type='scatter',mode='none',fill = 'tozeroy',showlegend= F, hoverinfo="none", fillcolor = 'lightgray')
  }
  if(plot.sci.rvw.QF){
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~dischargeFinalQFSciRvw,type='scatter',mode='none',fill = 'tozeroy',hoverinfo="none", showlegend= F, fillcolor = 'lightgray')
  }

  # Add base plot
  method <- method %>%

    #Historical Med Q
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~histMedQ, name=str_c("Historic Median\nDischarge: ","\n", histMedQMinYear,"-",histMedQMaxYear),type='scatter',mode='lines',line = list(color = 'grey'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",legendgroup='group8',visible = "legendonly")%>%

    # Q Uncertainty
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanURemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='line',line=list(color='#D55E00'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group1',visible = "legendonly")%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLRemnUnc,name="Discharge\nRemnant\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#D55E00',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group1',visible = "legendonly")%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='line',line=list(color='#E69F00'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group2',visible = "legendonly")%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLParaUnc,name="Discharge\nParametric\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#E69F00',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group2',visible = "legendonly")%>%

    # H Uncertainty
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanUHUnc,name="Stage\nUncertainty",type='scatter',mode='line',line=list(color='#56B4E9'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",yaxis='y2',showlegend=F,legendgroup='group3',visible = "legendonly")%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanLHUnc,name="Stage\nUncertainty",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#56B4E9',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",yaxis='y2',showlegend=T,legendgroup='group3',visible = "legendonly")%>%

    # H and Q Series
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanQ, name="Continuous\nDischarge",type='scatter',mode='lines',line = list(color = 'black'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",legendgroup='group4')%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~meanH, name="Continuous\nStage",type='scatter',mode='lines',line = list(color = '#CC79A7'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",yaxis='y2',showlegend=T,legendgroup='group5')%>%

    # Empirical H and Q
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~streamDischarge,name="Measured\nDischarge", type='scatter', mode='markers',marker = list(color = '#009E73',size=8,line = list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group6')%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gaugeHeight,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=list(color="#F0E442",size=8,line = list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group7')%>%
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gauge_Height,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=list(color="#F0E442",size=8,line = list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group7')

  return(method)

}
