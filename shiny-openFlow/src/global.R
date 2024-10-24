options(stringsAsFactors = F)

library(shiny)
library(shinyjs)
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
library(shinydashboard)
library(XML)
library(DBI)  #library added from Bola's app updates
library(RPostgres) #library added from Bola's app updates


# if(!require(neonStageQplot)){
#   devtools::install_github(repo = "NEONScience/NEON-stream-discharge/neonStageQPlot", dependencies = TRUE, force = TRUE)
#   library(neonStageQplot)
# }else{
#   library(neonStageQplot)
# }

#global ----


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

##***Bola addition to connect to the openFlow database***##

# Connect to openflow database
# con<-DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = 'openflow',
#   host = 'nonprod-commondb.gcp.neoninternal.org',
#   port = '5432',
#   user = 'shiny_openflow_rw',
#   password = Sys.getenv('DB_TOKEN')
# )

# Read in reference table from Github
# setwd("~/Github/NEON-stream-discharge/L4Discharge/AOSApp") # Code for testing locally - comment out when running app
#Global Vars
productList <- readr::read_csv(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge/main/shiny-openFlow/aqu_dischargeDomainSiteList.csv"))
# productList <- DBI::dbReadTable(con,"sitelist") #line added from Bola's app
siteID <- NULL
domainID <- NULL
osPubDateFormat <- "%Y-%m-%dT%H:%MZ"
waterDensity <- 999
gravity <- 9.80665
convKPatoPa <- 1000
well_depth_file <- read.csv("data/gw_well_depths.csv", sep = ",") 


###Used to pull inputs and outputs for cross section tab
#read reference Domain target table from GitHub
Target_df<- readr::read_csv(base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge-NCC189/xs-item/shiny-openFlow/src/data/Domains_targetGaugeHeight.csv"))
filter_target <- reactiveVal(NULL)
target2<- as.data.frame(cbind("No Domain Selected", "No Site Selected", "0", "0", "0", "No data"))
colnames(target2)<- names(Target_df)
Target_df<- rbind(target2, Target_df)


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

#Set DSCXS directory
XSDir<- "shiny-openFlow/src/data"


###eventually would be ideal to have all sites' DSCXS in one single df
#CUPEXS raw file
CUPEXS<- readr::read_csv((base::url("https://raw.githubusercontent.com/NEONScience/NEON-stream-discharge-NCC189/refs/heads/main/shiny-openFlow/src/data/D04_CUPE_surveyPts_20210408.csv")))
CUPE_dsc_pts_xs<-readr::read_csv("C:/Users/mviggiano/Documents/Github/NEON-stream-discharge-NCC189/shiny-openFlow/src/data/D04_CUPE_Transect_DSC.csv")

# include.q.stats <- T # Include Q Stats: Set to TRUE if on internal server, and FALSE if on external server
# constrain.dates <- F # Constrain Dates: Set to TRUE if on external serer, and FALSE if in Github or on internal server
# readmeFile <- "about_internal.Rmd"

# light <- bslib::bs_theme(version = 4,bootswatch = "flatly")
# dark <- bslib::bs_theme(version = 4,bootswatch = "darkly")

#shiny::shinyApp(ui = ui, server = server)
