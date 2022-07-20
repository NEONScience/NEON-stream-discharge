##############################################################################################
#' Plot NEON Stage-Discharge Rating Curves (DP4.00133.001) Data

#' @name plot.RC

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr
#' James M. Ross \email{ross.james94@gmail.com} \cr

#' @description  This function will generate a plotly plot of NEON Stage-Discharge Rating
#' Curves (DP4.00130.001) data to be rendered in the discharge visualization shiny app

#' @param site.id Required: NEON AQU site ID selected by the shiny app user [string]
#' @param start.date Required: Search interval start date (YYYY-MM-DD) selected by the shiny
#' app user [string]
#' @param end.date Required: Search interval end date (YYYY-MM-DD) selected by the shiny app
#' user [string]
#' @param input.list Required: List containing the curve IDs used in plotting [list]


#' @return Returns a plotly plot object

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export plot.RC

# changelog and author contributions / copyrights
#   Zachary L. Nickerson (2022-06-20)
#     original creation
#   James M. Ross (2022-07-01)
#     updated graph colors to be more colorblind friendly
##############################################################################################
# # Source packages and set options
options(stringsAsFactors = F)

plot.RC <-function(site.id,start.date,end.date,input.list,mode.dark,plot.imp.unit){

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

  # Get data
  curveIDs <- input.list[[2]]

  if(all(!is.na(curveIDs))){
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

    # Add each rating curve based on the vector of unique rating curve IDs
    for(i in 1:length(unique(rcData$curveID))){
      currentCurveID <- unique(rcData$curveID)[i]
      rcData_curveID <- rcData%>%
        filter(curveID==currentCurveID)
      rcGaugings_curveID <- rcGaugings%>%
        filter(curveID==currentCurveID)

      #axis units
      x1Units <- "(meter)"
      y1Units <- "(liters per second)"

      if(plot.imp.unit){
        rcData_curveID <- rcData_curveID %>%

          mutate(totalUTop = conv_unit(totalUTop,"l_per_sec","ft3_per_sec")) %>%
          mutate(totalUBottom = conv_unit(totalUBottom,"l_per_sec","ft3_per_sec")) %>%
          mutate(pramUTop = conv_unit(pramUTop,"l_per_sec","ft3_per_sec")) %>%
          mutate(pramUBottom = conv_unit(pramUBottom,"l_per_sec","ft3_per_sec")) %>%
          mutate(maxPostQ = conv_unit(maxPostQ,"l_per_sec","ft3_per_sec"))

        rcGaugings_curveID <- rcGaugings_curveID %>%

          mutate(Q = conv_unit(Q,"l_per_sec","ft3_per_sec"))

        x1Units <- "(Feet)"
        y1Units <- "(Cubic Feet per second)"
      }

    # Build plot layout
    rcPlot <- plotly::plot_ly(data=rcData)%>%
      layout(
        xaxis=list(tick=14,
                   automargin=T,
                   title=str_c("Stage ", x1Units),
                   tickfont=list(size=16),
                   titlefont=list(size=18)),
        yaxis=list(automargin=T,
                   title=str_c("Discharge ", y1Units),
                   # range=c(0,base::max(rcData$totalUBottom)*1.05),
                   tickfont=list(size=16),
                   titlefont=list(size=18),
                   showgrid=T,
                   zeroline=T),
        legend=list(x=-0.2,y=0.87,
                    font=list(size=14)),
        updatemenus=list(
          list(
            type='buttons',
            showactive=FALSE,
            buttons=list(
              list(label='Scale Discharge\n- Linear -',
                   method='relayout',
                   args=list(list(yaxis=list(type='linear',
                                             title=str_c("Discharge ", y1Units),
                                             # range=c(0,base::max(rcData$totalUBottom)*1.05),
                                             tickfont=list(size=16),
                                             titlefont=list(size=18),
                                             showgrid=T,
                                             zeroline=T)))),
              list(label='Scale Discharge\n- Log -',
                   method='relayout',
                   args=list(list(yaxis=list(type='log',
                                             title=str_c("Discharge ",y1Units," - log"),
                                             # range=c(0,base::log10(base::max(rcData$totalUBottom)*1.05)),
                                             tickfont=list(size=16),
                                             titlefont=list(size=18),
                                             showgrid=T,
                                             zeroline=T))))))))

    #dark mode styling
    ratingColor <- "black"
    if(mode.dark){
      rcPlot <- rcPlot %>% layout(paper_bgcolor="#222",plot_bgcolor='#222',
                                  font = list(color = 'white'))
      ratingColor <- "white"
    }


      rcPlot <- rcPlot%>%
        # Total Uncertainty
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=list(color='#D55E00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~totalUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='#D55E00',line=list(color='#D55E00'),hovertemplate = "Stage(m): %{x} <br> Discharge(lps): %{y}",showlegend=T,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        # Parametric Uncertainty
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUTop,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',line=list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~pramUBottom,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nUncertainty"),type='scatter',mode='line',fill='tonexty',fillcolor='#E69F00',line=list(color='#E69F00'),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,visible='legendonly',legendgroup=base::paste0(currentCurveID," Uncertainty"))%>%
        # Max Post Q
        plotly::add_trace(data=rcData_curveID,x=~Hgrid,y=~maxPostQ,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='line',line=list(color=ratingColor),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=T,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))%>%
        # Empirical H/Q Pairs
        plotly::add_trace(data=rcGaugings_curveID,x=~H,y=~Q,name=base::paste0(base::gsub("\\."," WY",currentCurveID),"\nRating Curve\nw/ Gaugings"),type='scatter',mode='markers',marker=list(color=ratingColor),hovertemplate = "Stage: %{x} <br> Discharge: %{y}",showlegend=F,legendgroup=base::paste0(currentCurveID," Rating Curve w/ Gaugings"))
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
    #dark mode styling
    ratingColor <- "black"
    if(mode.dark){
      rcPlot <- rcPlot %>% layout(paper_bgcolor="#222",plot_bgcolor='#222',
                                  font = list(color = 'white'))
      ratingColor <- "white"
    }
  }

  return(rcPlot)

}
