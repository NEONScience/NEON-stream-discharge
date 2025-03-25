##############################################################################################
#' Plot NEON Discharge Cross-Section (DP4.00131.001) Data

#' @name XS.plot

#' @author
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description  This function will generate a plotly plot of NEON Discharge Cross-Section 
#' (DP4.00131.001) data to be rendered in the openFlow shiny app. 

#' @param site.id Required: NEON AQU site ID selected by the shiny app user [string]
#' @param target.gag.range description

#' @return Returns a plotly plot object

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @importFrom dplyr %>%

#' @export XS.plot

# changelog and author contributions / copyrights
#   Zachary L. Nickerson (2025-01-02)
#     original creation
##############################################################################################
base::options(stringsAsFactors = F)
XS.plot <-function(site.id,
                   target.gag.range=NULL,
                   rtdv.values=NULL,
                   p.source=NA
){
  
  if(base::missing(site.id)){
    stop('must provide site.id for plotting continuous discharge')
  }
  
  # Get the transect data fomr openflow DB
  dsctransect <- DBI::dbReadTable(con,"dsctransect")
  dsctransect <- dsctransect[dsctransect$siteID==site.id,]
  dsctransect <- dsctransect[order(dsctransect$distanceAdjusted),]
  p.title <- paste(site.id,unique(dsctransect$surveyEndDate),"Survey")
  
  # Create the base plot
  xsPlot <- plotly::plot_ly(data=dsctransect,source=p.source)%>%
    plotly::layout(
      title = list(text=p.title,
                   x=0,
                   xanchor="left"),
      xaxis=base::list(tick=14,
                       automargin=T,
                       zeroline=F,
                       title="Adjusted Distance (m)",
                       tickfont=base::list(size=16),
                       titlefont=base::list(size=18)),
      yaxis=base::list(automargin=T,
                       title="Gauge Height (m)",
                       tickfont=base::list(size=16),
                       titlefont=base::list(size=18),
                       showgrid=T,
                       zeroline=F),
      legend=base::list(orientation = "h",
                        y=-0.15,
                        font=base::list(size=14)))
    
  # Determine the value used for the stage fill of the plot
  if(!is.null(rtdv.values)){
    stage.fill <- rtdv.values
  }else{
    if(!is.null(target.gag.range)){
      stage.fill <- target.gag.range$tar
    }
  }
  
  # Fill out the plot
  xsPlot <- xsPlot%>%
    plotly::add_trace(x=~distanceAdjusted,y=~stage.fill,type='scatter',mode='line',line=list(color='lightblue'),hoverinfo='none',showlegend=F)%>%
    plotly::add_trace(x=~distanceAdjusted,y=min(dsctransect$gaugeHeight),type='scatter',mode='line',fill='tonexty',fillcolor='lightblue',line=list(color='lightblue'),hoverinfo='none',showlegend=F)%>%
    plotly::add_trace(x=~distanceAdjusted,y=~gaugeHeight,text=~surveyPointID,name="Survey\nPoint ID",type='scatter',mode='line',line=list(color='brown'),hovertemplate = "Distance: %{x} <br> Gauge Height: %{y} <br> %{text}",showlegend=F)%>%
    plotly::add_trace(x=~distanceAdjusted,y=min(dsctransect$gaugeHeight)-((max(dsctransect$gaugeHeight)-min(dsctransect$gaugeHeight))*.10),type='scatter',mode='line',fill='tonexty',fillcolor='brown',line=list(color='black'),hoverinfo='none',showlegend=F)
  
  # Add target gauge ranges if the targets are entered
  if(!is.null(target.gag.range)){
    xsPlot <- xsPlot%>%
      plotly::add_segments(x=min(dsctransect$distanceAdjusted),xend=max(dsctransect$distanceAdjusted),y=target.gag.range$min,yend=target.gag.range$min,name="Target Range",line=list(color="black",dash='dash'),legendgroup="target",showlegend=F)%>%
      plotly::add_segments(x=min(dsctransect$distanceAdjusted),xend=max(dsctransect$distanceAdjusted),y=target.gag.range$max,yend=target.gag.range$max,name="Target Range",line=list(color="black",dash='dash'),legendgroup="target",showlegend=F)
  }
  
  return(xsPlot)
}