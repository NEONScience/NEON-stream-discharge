##############################################################################################
#' Plot NEON Stage-Discharge Rating Curves (DP4.00133.001) Data

#' @name RC.plot

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr
#' James M. Ross \email{ross.james94@gmail.com} \cr

#' @description  This function will generate a plotly plot of NEON Stage-Discharge Rating
#' Curves (DP4.00130.001) data to be rendered in the openFlow shiny app. The function can
#' also be used outside the app, but should only be used with the outputs from
#' neonStageQPlot::get.cont.Q.NEON.API.

#' @param site.id Required: NEON AQU site ID selected by the shiny app user [string]
#' @param start.date Required: Search interval start date (YYYY-MM-DD) selected by the shiny
#' app user [string]
#' @param end.date Required: Search interval end date (YYYY-MM-DD) selected by the shiny app
#' user [string]
#' @param input.list Required: List containing the data used in plotting; this list should be
#' the outputs from neonStageQPlot::get.cont.Q.NEON.API [list]
#' @param plot.imp.unit Defaults to FALSE: Idicator of plotting data in metric or imperial
#' units [boolean]
#' @param mode.dark Defaults to FALSE: Indicator of plotting data in light or dark mode. NOTE:
#' This argument is only used in the openFlow shiny app. [boolean]

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
utils::globalVariables(c("curveID","Hgrid","maxPostQ","pramUTop","pramUBottom","totalUTop","totalUBottom","H","uH","bH","bHindx","Q","uQ","bQ","bQindx"))

RC.plot <-function(site.id,
                   start.date,
                   end.date,
                   input.list,
                   plot.imp.unit=F,
                   mode.dark=F){

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
  curveIDs <- input.list[[2]]

  if(base::all(!base::is.na(curveIDs))){
    rcPlotData <- base::readRDS(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/ZN_internalAppUpdates/shiny-openFlow/rcPlottingData.rds","rb"))
    rcData <- rcPlotData$rcData%>%
      dplyr::filter(curveID%in%curveIDs)
    rcGaugings <- rcPlotData$rcGaugings%>%
      dplyr::filter(curveID%in%curveIDs)

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
        rcPlot <- plotly::plot_ly(data=rcData)%>%
          plotly::layout(
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
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=base::list(color='#D55E00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='#D55E00',line=base::list(color='#D55E00'),hovertemplate = "Stage(m): %{x} <br> Discharge(lps): %{y}",showlegend=T,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        # Parametric Uncertainty
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=base::list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='#E69F00',line=base::list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        # Max Post Q
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~maxPostQ,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='line',line=base::list(color=ratingColor),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=T,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))%>%
        # Empirical H/Q Pairs
        plotly::add_trace(data=rcGaugings_curveID,x=~H,y=~Q,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='markers',marker=base::list(color=ratingColor),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))
    }
  }else{
    rcPlot <- plotly::plotly_empty()%>%
      plotly::layout(
        title=base::list(
          text = "No Stage-discharge rating curves (DP4.00133.001)\n data available for this site.",
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

  return(rcPlot)

}
