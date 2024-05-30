##############################################################################################
#' openFlow: NEON Hydrologic Data Visualization App

#' @author
#' James Ross \email{ross.james94@gmail.com} \cr
#' Divine Aseaku \email{divineaseaku@gmail.com} \cr
#' Zachary L. Nickerson \email{nickerson@battelleecology.org} \cr

#' @description  This application allows you view and interact with select hydrologic data 
#' collected at 29 stream, river, and lake inflow/outflow sites across the National Ecological
#' Observatory Network (NEON). NEON spans the continential United States and includes 
#' monitoring sites in Alaska and Puerto Rico. In this app you can visually explore data from 
#' the following NEON Data Products: Stage-discharge rating curves (DP4.00133.001), Continuous 
#' discharge (DP4.00130.001), Precipitation (DP1.00006.001), and Land-water interface images 
#' (DP1.20002.001)

#' @return This function launches a shiny app to interactively plot published NEON data

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Divine Aseaku (2021-08-04)
#     original creation
#   Zachary L. Nickerson (2021-09-16)
#     updates for plotting provisional 2021 data and TOOK inflow and outflow
#   Zachary L. Nickerson (2022-02-04)
#     updates for plotting TOMB-USGS discharge data
#   James M. Ross (2022-07-25)
#     major updates include plotting NEON precipitation data, rendering NEON PhenoCam images 
#     via a click event, download buttons for plots and images, aesthetic updates to color 
#     paletes, axes, and app themes, and an 'About the App' tab.
#   Zachary L. Nickerson (2022-08-09)
#     remove package dependencies 'stageQCurve' and 'measurements'
##############################################################################################
# # Source packages and set options
options(stringsAsFactors = F)

library(shiny)
library(plotly)
library(neonUtilities)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(lubridate)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(htmlwidgets)
library(httr)
library(bslib)
library(shinyalert)
library(devtools)
library(markdown)
if(!require(neonStageQplot)){
  devtools::install_github(repo = "NEONScience/NEON-stream-discharge/neonStageQPlot", dependencies = TRUE, force = TRUE)
  library(neonStageQplot)
}else{
  library(neonStageQplot)
}

# User Interface ----
ui <- shiny::fluidPage(title = 'openFlow',
                       theme = bslib::bs_theme(version = 4),
                       style = "padding:25px;",
                       tags$head(tags$style("#shiny-modal img { max-width: 100%; }")),#####modal scaling
                       shiny::titlePanel(shiny::fluidRow(shiny::column(10, img(src = "app-logo.png",width = 300,height = 150)), 
                                                         shiny::column(2, img(src = "logo-NEON-NSF.png",width = 200,height = 75)))),# End of title panel
                       shiny::fluidRow(shiny::column(2,
                                                     shiny::fluidRow(shiny::selectInput("domainId","Domain ID",productList$domain)),
                                                     shiny::fluidRow(shiny::uiOutput("domainInfo")),
                                                     shiny::br(),
                                                     shiny::fluidRow(shiny::selectInput("siteId","Select Site ID",NULL)),
                                                     shiny::fluidRow(shiny::uiOutput("siteInfo")),
                                                     shiny::br(),
                                                     shiny::fluidRow(shiny::dateRangeInput("dateRange","Date range:",
                                                                                           startview="month",
                                                                                           min="2010-01-01",
                                                                                           start=lubridate::floor_date(base::Sys.Date()-14,"month")-base::months(1),
                                                                                           end=lubridate::floor_date(base::Sys.Date()-14,"month")-1,
                                                                                           format="yyyy-mm-dd"),
                                                                     shiny::textInput("apiToken", "NEON API Token (Optional)")),
                                                     shiny::br(),
                                                     shiny::fluidRow(shiny::actionButton(inputId="submit","Submit")),
                                                     shiny::br(),
                                                     shiny::fluidRow(# shiny::checkboxInput("qctrFlag", "Include Final Quality Flag for Discharge(light gray)", FALSE),
                                                       shiny::checkboxInput("qctrFlagScRv", "Include Discharge Science Review Quality Flags", FALSE),
                                                       # shiny::checkboxInput("precipQctrFlag", "Include Final Quality Flag for Precipitation(gray)", FALSE),
                                                       # shiny::checkboxInput("precipQctrFlagScRv", "Include Science Review Quality Flag for Precipitation(gray)", FALSE),
                                                       shiny::checkboxInput("impUnitFlag", "Convert to Imperial Units", FALSE),
                                                       shiny::checkboxInput("dark_mode", "Show in Dark Mode")),
                                                     shiny::hr(),
                                                     shiny::fluidRow(shiny::conditionalPanel(
                                                       #checks that one of the graphs has been loaded
                                                       condition = "output.plot1 != null || output.plot2 != null",
                                                       shiny::downloadButton("downloadPlotly", "Download Graph"))),
                                                     shiny::br(),
                                                     shiny::fluidRow(shiny::textOutput("title"),
                                                                     DT::dataTableOutput("table"))),#end of first col
                                       shiny::column(10,
                                                     shiny::tabsetPanel(type = "tabs",id = "selectedTab",
                                                                        shiny::tabPanel("Continuous Discharge",
                                                                                        shinycssloaders::withSpinner(plotly::plotlyOutput("plot1",height="800px"))),
                                                                        shiny::tabPanel("Rating Curve(s)",
                                                                                        shinycssloaders::withSpinner(plotly::plotlyOutput("plot2",height="800px"),
                                                                                                                     color = "#00ADD7")),
                                                                        shiny::tabPanel("About the App",
                                                                                        shiny::includeMarkdown(readmeFile))))#end of second col
                       )#end of fluid row
) # end of ui and fluidPage
