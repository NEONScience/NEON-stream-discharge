# Load required packages
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

# Set global options
options(stringsAsFactors = F)

# Set local options
include.q.stats <- T # Include Q Stats: Set to TRUE if on internal server, and FALSE if on external server
constrain.dates <- F # Constrain Dates: Set to TRUE if on external serer, and FALSE if in Github or on internal server
light <- bslib::bs_theme(version = 4,bootswatch = "flatly")
dark <- bslib::bs_theme(version = 4,bootswatch = "darkly")

# Read in data used in app
productList <- readr::read_csv(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/ZN_pullAllAuto/shiny-openFlow/aqu_dischargeDomainSiteList.csv"))
# siteID <- NULL
# domainID <- NULL
continuousDischarge_list <- base::readRDS("C:/Users/nickerson/Box/L4-Discharge-Development-And-Testing/outputList.rds")
rcPlotData <- base::readRDS(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/main/shiny-openFlow/rcPlottingData.rds","rb"))
histMedQ <- base::readRDS(base::url("https://storage.neonscience.org/neon-geobath-files/NEON_MEDIAN_Q_SHINY_APP_THROUGH_WY2020_VC.rds","rb"))


# Function to get pheno image
pheno.GET <- function(dp.id,
                      site.id,
                      domain.id,
                      date.time){
  
  # # Test GET
  # dp.id <- "DP1.20002"
  # site.id <- "PRIN"
  # domain.id <- "D11"
  # #UTC dateTime
  # date.time <- "2021-12-01T18:00:00Z"
  
  if(base::missing(dp.id)){
    stop('must provide dp.id to query Phenocam API')
  }
  if(base::missing(site.id)){
    stop('must provide site.id to query Phenocam API')
  }
  if(base::missing(domain.id)){
    stop('must provide domain.id to query Phenocam API')
  }
  if(base::missing(date.time)){
    stop('must provide date.time to query Phenocam API')
  }
  
  # browser()
  
  if(site.id == "TOOK_inflow" || site.id == "TOOK_outflow"){
    site.id <- "TOOK"
  }
  
  phenoGET <- httr::content(httr::GET(url = base::paste0("https://phenocam.nau.edu/neonapi/imageurl/NEON.",domain.id,".",site.id,".",dp.id,"/",date.time,"/")),
                            encoding = "UTF-8")
  
  if(base::is.null(phenoGET$url)){
    phenoURL <- NULL
  }else{
    phenoURL <- phenoGET$url
  }
  
  return(phenoURL)
}




#






















#
