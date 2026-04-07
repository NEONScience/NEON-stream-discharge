##############################################################################################
#' @title 

#' @author
#' Zachary Nickerson \email{nickerson@battelleecology.org} \cr

#' @description 

#' @return 

# changelog and author contributions / copyrights
#   Zachary Nickerson (YYYY - MM - DD)
#     original creation
##############################################################################################

# Set options
options(stringsAsFactors = F)

# Load libraries
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

pass <- 0

message("INFO! Launching openFlow")
message("INFO! Current working directory: ",getwd())
message("INFO! DB_HOST = ",Sys.getenv("DB_HOST"))

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
  user = 'shiny_openflow_ro',
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
include.q.stats <- TRUE