options(stringsAsFactors = F)

library(shiny)
library(shinyjs)
library(plotly)
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
library(shinydashboard)
library(XML)
library(DBI)
library(RPostgres)
require(openFlowInternal)

#global ----
pass <- 0

# Set the working directory to the current files location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Gather list of function files to be sourced
file_sources <- list.files(
  file.path('functions'),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Source gathered files into global environment
file_sources <-
  base::tryCatch(
    invisible(sapply(file_sources, source, .GlobalEnv)),
    error = function(e) {
      base::stop()
    }
  )

# Connect to openflow database
con<-DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'openflow',
  host = Sys.getenv("DB_HOST"),
  port = '5432',
  user = 'shiny_openflow_rw',
  password = Sys.getenv('DB_TOKEN')
)

# Read in Site Metadata
productList <- DBI::dbReadTable(con,"sitelist")
productList <- productList[order(productList$domain),]
Target_df <- DBI::dbReadTable(con,"targetgag")
siteID <- NULL
domainID <- NULL
osPubDateFormat <- "%Y-%m-%dT%H:%MZ"
waterDensity <- 999
gravity <- 9.80665
convKPatoPa <- 1000
# well_depth_file <- read.csv("data/gw_well_depths.csv", sep = ",") 

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
  readmeFile <- '../about.Rmd'
  include.q.stats <- FALSE
  # constrain.dates <- TRUE
}

# include.q.stats <- T # Include Q Stats: Set to TRUE if on internal server, and FALSE if on external server
# constrain.dates <- F # Constrain Dates: Set to TRUE if on external serer, and FALSE if in Github or on internal server
# readmeFile <- "about_internal.Rmd"

# light <- bslib::bs_theme(version = 4,bootswatch = "flatly")
# dark <- bslib::bs_theme(version = 4,bootswatch = "darkly")