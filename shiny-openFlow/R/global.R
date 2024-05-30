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

#global ----

# Read in reference table from Github
# setwd("~/Github/NEON-stream-discharge/L4Discharge/AOSApp") # Code for testing locally - comment out when running app
#Global Vars
productList <- readr::read_csv(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/main/shiny-openFlow/aqu_dischargeDomainSiteList.csv"))
siteID <- NULL
domainID <- NULL

#change settings depending on HOST - internal app vs external app
HOST <- Sys.getenv('HOST')
message(paste('HOST =',HOST))
if(grepl('internal',HOST)){
  apiToken <- Sys.getenv('NEON_TOKEN')
  readmeFile <- 'about_internal.Rmd'
  include.q.stats <- TRUE
  # constrain.dates <- FALSE
}else{
  #external 
  #don't set apiToken here as input$apiToken doesn't exist yet - see in server
  readmeFile <- 'about.Rmd'
  include.q.stats <- FALSE
  # constrain.dates <- TRUE
}

# include.q.stats <- T # Include Q Stats: Set to TRUE if on internal server, and FALSE if on external server
# constrain.dates <- F # Constrain Dates: Set to TRUE if on external serer, and FALSE if in Github or on internal server
# readmeFile <- "about_internal.Rmd"

light <- bslib::bs_theme(version = 4,bootswatch = "flatly")
dark <- bslib::bs_theme(version = 4,bootswatch = "darkly")