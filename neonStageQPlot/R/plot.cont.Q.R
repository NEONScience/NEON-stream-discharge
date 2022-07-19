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
#' @param include.q.stats Defaults to FALSE: Include values for 3x median discharge and 25-75%
#' flow in the plot. Statistics are calculated from the time range selected by the user and
#' exclude records that contain a science review quality flag (dischargeFinalQFSciRvw) [boolean]

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

plot.cont.Q <-function(site.id,
                       start.date,
                       end.date,
                       input.list,
                       plot.final.QF,
                       plot.sci.rvw.QF,
                       plot.precip.final.QF,
                       plot.precip.sci.rvw.QF,
					   plot.q.stats=F){

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
  if(missing(plot.precip.final.QF)){
    stop('must provide plot.precip.final.QF for plotting precipitation')
  }
  if(missing(plot.precip.sci.rvw.QF)){
    stop('must provide plot.precip.sci.rvw.QF for plotting precipitation')
  }


  precipitationa <- 5

  #symbolics
  isPrimaryPtp <- 2
  precipSite <- 1
  csd_summary <- 1
  ptpData <- 3

  #symbolic
  histMedQYearRange <- 3
  csd_summary <- 1
  minYear <- 1
  maxYear <- 2
  dischargeStats <- 4
  medQValue <- 1
  twentyFiveQValue <- 2
  seventyFiveQValue <- 3

  # Get data
  continuousDischarge_sum <- input.list[[csd_summary]]
  ptpDataset <- input.list[[ptpData]]
  precipSiteID <- ptpDataset[[precipSite]]
  histMedQMinYear <- input.list[[histMedQYearRange]][minYear]
  histMedQMaxYear <- input.list[[histMedQYearRange]][maxYear]

  y2 <- list(side='right',
              overlaying="y",
              automargin=T,
              title="Stage (meter)",
              tickfont=list(size=16,color = '#CC79A7'),
              titlefont=list(size=18,color = '#CC79A7'),
              showgrid=F,
              zeroline=F)

  y3 <- list(side='right',
              overlaying="y",
              automargin=T,
              title="Percipitation (milimeter)",
              tickfont=list(size=16,color = "#0072B2"),
              titlefont=list(size=18,color = "#0072B2"),
              showgrid=F,
              zeroline=F,
              anchor="free",
              position=0.98)


  # Build plot layout
  method <- plotly::plot_ly(data=continuousDischarge_sum, source = "phenoDate")%>%
    layout(
      yaxis2 = y2, yaxis3 = y3,
      xaxis=list(domain=c(0,.9),
                 tick=14,
                 automargin=T,
                 title="Date",
                 tickfont=list(size=16),
                 titlefont=list(size=18)#,
                 # range=c(base::format(shiny::isolate({input$dateRange[1]}) ),base::format(shiny::isolate({input$dateRange[2]}) ))
                 ),
      yaxis=list(side='left',
                 automargin=T,
                 title='Discharge (liters per second)',
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
                                           tickfont=list(size=16,color = "black"),
                                           titlefont=list(size=18,color = "black"),
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
  #wraps precip flag data to prevent plotting when data does not exist
  if(ptpDataset[[isPrimaryPtp]] & plot.precip.final.QF){
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~priPrecipFinalQF,type='scatter',mode='none',fill = 'tozeroy',showlegend= F, hoverinfo="none", fillcolor = 'gray')
  }

  #wraps precip flag data to prevent plotting when data does not exist
  if(ptpDataset[[isPrimaryPtp]]==FALSE & plot.precip.sci.rvw.QF){
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~secPrecipSciRvwQF,type='scatter',mode='none',fill = 'tozeroy',hoverinfo="none", showlegend= F, fillcolor = 'gray')
  }
  
  if(plot.q.stats){
	medQ <- base::as.numeric(input.list[[dischargeStats]][medQValue])
	twentyFiveQ <- base::as.numeric(input.list[[dischargeStats]][twentyFiveQValue])
    seventyFiveQ <- base::as.numeric(input.list[[dischargeStats]][seventyFiveQValue])
  }

  # Add base plot
  method <- method %>%

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
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~gauge_Height,name='Measured\nGauge\nHeight',type='scatter',mode='markers',yaxis='y2',marker=list(color="#F0E442",size=8,line = list(color = "black",width = 1)),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group7')%>%

    #Historical Med Q
    plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~histMedQ, name=str_c("Historic Median\nDischarge: ","\n", histMedQMinYear,"-",histMedQMaxYear),type='scatter',mode='lines',line = list(color = 'grey'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",legendgroup='group8',visible = "legendonly")

  # Add the internal parameters
  if(plot.q.stats){
    method <- method%>%
      plotly::add_segments(x=~base::min(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),xend=~base::max(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),y=~medQ,yend=~medQ,line=list(color='grey',dash='dash'),name="3x Median\nDischarge",showlegend=T,legendgroup='group9',visible = "legendonly")%>%
      plotly::add_segments(x=~base::min(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),xend=~base::max(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),y=~twentyFiveQ,yend=~twentyFiveQ,line=list(color='black',dash='dash'),name="25-75%\nischarge",showlegend=F,legendgroup='group10',visible = "legendonly")%>%
      plotly::add_segments(x=~base::min(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),xend=~base::max(base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),na.rm = T),y=~seventyFiveQ,yend=~seventyFiveQ,line=list(color='black',dash='dash'),name="25-75%\nDischarge",showlegend=T,legendgroup='group10',visible = "legendonly")%>%
      plotly::layout(
        title=list(text=base::paste0("<br><b>3x Median Discharge = ",base::round(medQ,digits = 1)," L/s<br>25-75% Discharge: ",base::round(twentyFiveQ,digits = 1)," - ",base::round(seventyFiveQ,digits = 1)," L/s<b>"),
                   xanchor="left",
                   xref="paper",
                   x=0.1))
  }


  #Precipitation Data
  #plots whichever data is available using isPrimaryPtp bool
  if(ptpDataset[[isPrimaryPtp]]){
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~priPrecipBulkLoUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='line',line=list(color='#431A74'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group9',visible = "legendonly")%>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~priPrecipBulkUpUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#431A74',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group9',visible = "legendonly") %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~priPrecipBulk,name=str_c("Continuous\nPrecipitation\nSite: ",precipSiteID),yaxis = "y3",type='scatter',mode='lines',line = list(color = '#0072B2'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",legendgroup='group8',visible = "legendonly")
  }
  else{
    method <- method %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~secPrecipBulkLoUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='line',line=list(color='#431A74'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=F,legendgroup='group9',visible = "legendonly")%>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~secPrecipBulkUpUnc,name="Continuous\nPrecipitation\nUncertainty",yaxis = "y3",type='scatter',mode='none',fill = 'tonexty',fillcolor = '#431A74',hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",showlegend=T,legendgroup='group9',visible = "legendonly") %>%
      plotly::add_trace(x=~base::as.POSIXct(date,format="%Y-%m-%d %H:%M:%S"),y=~secPrecipBulk,name=str_c("Continuous\nPrecipitation\nSite: ",precipSiteID),yaxis = "y3",type='scatter',mode='lines',line = list(color = '#0072B2'),hovertemplate = "Date/UTC-Time: %{x} <br> Value: %{y}",legendgroup='group8',visible = "legendonly")
  }

  return(method)
}
