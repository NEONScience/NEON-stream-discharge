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