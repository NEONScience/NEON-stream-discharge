##############################################################################################
#' Plot NEON Stage-Discharge Rating Curves (DP4.00133.001) Data

#' @name RC.plot

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr
#' James M. Ross \email{ross.james94@gmail.com} \cr

#' @description  This function will generate a plotly plot of NEON Stage-Discharge Rating
#' Curves (DP4.00130.001) data to be rendered in the openFlow shiny app. 

#' @param site.id Required: NEON AQU site ID selected by the shiny app user [string]
#' @param start.date Required: Search interval start date (YYYY-MM-DD) selected by the shiny
#' app user [string]
#' @param end.date Required: Search interval end date (YYYY-MM-DD) selected by the shiny app
#' user [string]
#' @param plot.imp.unit Defaults to FALSE: Idicator of plotting data in metric or imperial
#' units [boolean]
#' @param mode.dark Defaults to FALSE: Indicator of plotting data in light or dark mode. NOTE:
#' This argument is only used in the openFlow shiny app. [boolean]
#' @param target.gag.range description
#' @param show.legend description

#' @return Returns a plotly plot object

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @importFrom dplyr %>%

#' @export RC.plot

# changelog and author contributions / copyrights
#   Zachary L. Nickerson (2022-07-25)
#     original creation
#   Zachary L. Nickerson (2022-08-09)
#     removed dependency on measurements
#   Zachary L. Nickerson (2023-02-13)
#     updates to include 3x median discharge
##############################################################################################
base::options(stringsAsFactors = F)
RC.plot <-function(site.id,
                   start.date,
                   end.date,
                   plot.imp.unit=F,
                   mode.dark=F,
                   target.gag.range=NULL,
                   med.3x=NULL,
                   rtdv.values=NULL,
                   show.legend=T,
                   uncertainty.visibility="legendonly",
                   p.source=NA
                   ){

  if(base::missing(site.id)){
    stop('must provide site.id for plotting continuous discharge')
  }
  if(base::missing(start.date)){
    stop('must provide start.date for plotting continuous discharge')
  }
  if(base::missing(end.date)){
    stop('must provide end.date for plotting continuous discharge')
  }
  
  if(site.id=="TOOK_inflow"){
    site.id <- "TKIN"
  }
  if(site.id=="TOOK_outflow"){
    site.id <- "TKOT"
  }
  
  # Get data
  startDateFormat <- format(as.POSIXct(start.date),"%Y-%m-%d 00:00:00")
  endDateFormat <- format(as.POSIXct(end.date)+86400,"%Y-%m-%d 00:00:00")
  dbquery <- sprintf("SELECT * FROM contqsum WHERE \"siteID\" = '%s' AND \"date\" > timestamp '%s' AND \"date\" < timestamp '%s'",site.id,startDateFormat,endDateFormat)
  contqsum <- DBI::dbSendQuery(con,dbquery)
  contqsum <- DBI::dbFetch(contqsum)
  curveIDs <- unique(contqsum$curveID)
  # DB query
  dbquery <- sprintf("SELECT * FROM rcdata")
  rcdata <- DBI::dbSendQuery(con,dbquery)
  rcData <- DBI::dbFetch(rcdata)
  dbquery <- sprintf("SELECT * FROM rcgaugings")
  rcgaugings <- DBI::dbSendQuery(con,dbquery)
  rcGaugings <- DBI::dbFetch(rcgaugings)
  # Subset to curves
  if(any(!is.na(curveIDs))){
    curveIDs <- curveIDs[!is.na(curveIDs)]
    rcData <- rcData[rcData$curveID%in%curveIDs,]
    rcGaugings <- rcGaugings[rcGaugings$curveID%in%curveIDs,]
    p.title <- "Rating Curve(s)"
  }else{
    curveIDs <- max(unique(rcData$curveID[grepl(site.id,rcData$curveID)]))
    rcData <- rcData[rcData$curveID%in%curveIDs,]
    rcGaugings <- rcGaugings[rcGaugings$curveID%in%curveIDs,]
    p.title <- paste("Rating Curve: ",curveIDs)
  }

  if(base::all(!base::is.na(curveIDs))){
    # Add each rating curve based on the vector of unique rating curve IDs
    for(i in 1:base::length(base::unique(rcData$curveID))){
      currentCurveID <- base::unique(rcData$curveID)[i]
      rcData_curveID <- rcData%>%
        dplyr::filter(curveID==currentCurveID)
      rcGaugings_curveID <- rcGaugings%>%
        dplyr::filter(curveID==currentCurveID)

      #axis units
      x1Units <- "(meter)"
      y1Units <- "(liters per second)"
      convLPStoCFS <- 0.035314
      convMtoFt <- 3.28084

      if(plot.imp.unit){
        rcData_curveID <- rcData_curveID %>%
          dplyr::mutate(Hgrid = Hgrid*convMtoFt,
                        totalUTop = totalUTop*convLPStoCFS,
                        totalUBottom = totalUBottom*convLPStoCFS,
                        pramUTop = pramUTop*convLPStoCFS,
                        pramUBottom = pramUBottom*convLPStoCFS,
                        maxPostQ = maxPostQ*convLPStoCFS)
        rcGaugings_curveID <- rcGaugings_curveID %>%
          dplyr::mutate(H = H*convMtoFt,
                        Q = Q*convLPStoCFS)

        x1Units <- "(Feet)"
        y1Units <- "(Cubic Feet per second)"
      }

      if(i==1){
        # Build plot layout
        rcPlot <- plotly::plot_ly(data=rcData, source = p.source)%>%
          plotly::layout(
            title = list(text=p.title,
                         x=0,
                         xanchor="left"),
            xaxis=base::list(tick=14,
                             automargin=T,
                             title=stringr::str_c("Stage ", x1Units),
                             tickfont=base::list(size=16),
                             titlefont=base::list(size=18)),
            yaxis=base::list(automargin=T,
                             title=stringr::str_c("Discharge ", y1Units),
                             # range=c(0,base::max(rcData$totalUBottom)*1.05),
                             tickfont=base::list(size=16),
                             titlefont=base::list(size=18),
                             showgrid=T,
                             zeroline=T),
            legend=base::list(orientation = "h",
                              y=-0.15,
                              font=base::list(size=14)),
            updatemenus=base::list(
              base::list(
                type='buttons',
                direction = "right",
                xanchor = 'right',
                yanchor = "top",
                # pad = list('r'= 0, 't'= 10, 'b' = 10),
                x = 1,
                y = 1.15,
                showactive=FALSE,
                buttons=base::list(
                  base::list(label='Scale Discharge\n- Linear -',
                             method='relayout',
                             args=base::list(base::list(yaxis=base::list(type='linear',
                                                                         title=stringr::str_c("Discharge ", y1Units),
                                                                         # range=c(0,base::max(rcData$totalUBottom)*1.05),
                                                                         tickfont=base::list(size=16),
                                                                         titlefont=base::list(size=18),
                                                                         showgrid=T,
                                                                         zeroline=T)))),
                  base::list(label='Scale Discharge\n- Log -',
                             method='relayout',
                             args=base::list(base::list(yaxis=base::list(type='log',
                                                                         title=stringr::str_c("Discharge ",y1Units," - log"),
                                                                         # range=c(0,base::log10(base::max(rcData$totalUBottom)*1.05)),
                                                                         tickfont=base::list(size=16),
                                                                         titlefont=base::list(size=18),
                                                                         showgrid=T,
                                                                         zeroline=T))))))))
      }

      #dark mode styling
      ratingColor <- "black"
      if(mode.dark){
        rcPlot <- rcPlot%>%
          plotly::layout(paper_bgcolor="#222",plot_bgcolor='#222',
                         font = base::list(color = 'white'))
        ratingColor <- "white"
      }

      rcPlot <- rcPlot%>%
        # Total Uncertainty
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=base::list(color='#D55E00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible=uncertainty.visibility,legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='#D55E00',line=base::list(color='#D55E00'),hovertemplate = "Stage(m): %{x} <br> Discharge(lps): %{y}",showlegend=show.legend,visible=uncertainty.visibility,legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        # Parametric Uncertainty
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=base::list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible=uncertainty.visibility,legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='#E69F00',line=base::list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible=uncertainty.visibility,legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        # Max Post Q
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~maxPostQ,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='line',line=base::list(color=ratingColor),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=show.legend,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))%>%
        # Empirical H/Q Pairs
        plotly::add_trace(data=rcGaugings_curveID,x=~H,y=~Q,text=~eventID,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='markers',marker=base::list(color=ratingColor),hovertemplate = "%{text} <br> Stage: %{x} <br> Discharge: %{y}",showlegend=F,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))
    }
  }else{
    rcPlot <- plotly::plotly_empty()%>%
      plotly::layout(
        title=base::list(
          text = "No Stage-discharge rating curves\n(DP4.00133.001)\n data available for this site.",
          yref = "paper",
          y = 0.5,
          font=base::list(size=28)
        )
      )
    #dark mode styling
    ratingColor <- "black"
    if(mode.dark){
      rcPlot <- rcPlot%>%
        plotly::layout(paper_bgcolor="#222",plot_bgcolor='#222',
                       font = base::list(color = 'white'))
      ratingColor <- "white"
    }
  }
  
  # Add target gauge range if the input list is provided
  if(!is.null(target.gag.range)){
    gagRangeTrace <- data.frame(Hgrid=c(0,target.gag.range$min-0.00001,target.gag.range$min-0.00001,target.gag.range$min,target.gag.range$max,target.gag.range$max+0.00001,target.gag.range$max+0.00001,max(rcData$Hgrid)),
                                rangeRibbon=c(0,0,max(rcData$totalUBottom),max(rcData$totalUBottom),max(rcData$totalUBottom),max(rcData$totalUBottom),0,0))
    rcPlot <- rcPlot%>%
      plotly::add_trace(data=gagRangeTrace,x=~Hgrid,y=~rangeRibbon,name="Target Range",type='scatter',mode='none',fill="tozeroy",fillcolor='rgba(172, 255, 115, 0.5)',legendgroup="target",showlegend=show.legend,hoverinfo='none')%>%
      plotly::add_segments(x=target.gag.range$min,xend=target.gag.range$min,y=0,yend=max(rcData$totalUBottom),name="Target - 30%",line=list(color='darkgreen',dash='dash'),legendgroup="target",showlegend=F,hoverinfo='none')%>%
      plotly::add_segments(x=target.gag.range$tar,xend=target.gag.range$tar,y=0,yend=max(rcData$totalUBottom),name="Target Gauge",line=list(color='darkgreen',dash='dash'),legendgroup="target",showlegend=F,hoverinfo='none')%>%
      plotly::add_segments(x=target.gag.range$max,xend=target.gag.range$max,y=0,yend=max(rcData$totalUBottom),name="Target + 10%",line=list(color='darkgreen',dash='dash'),legendgroup="target",showlegend=F,hoverinfo='none')
  }
  
  # Add bar for 3x median Q if the input is provided
  if(!is.null(med.3x)){
    rcPlot <- rcPlot%>%
      plotly::add_segments(x=0,xend=max(rcData$Hgrid),y=med.3x,yend=med.3x,name="3x Median Q",line=list(color='red',dash='dash'),legendgroup="3xmed",showlegend=show.legend)
  }
  
  # Add the current value from the real time data viewer, if provided
  if(!is.null(rtdv.values)){
    rcPlot <- rcPlot%>%
      plotly::add_segments(x=0,xend=max(rcData$Hgrid),y=rtdv.values,yend=rtdv.values,name="Real-Time Q",line=list(color='black',dash='dash'),legendgroup="rtdv",showlegend=show.legend)
  }

  return(rcPlot)

}
