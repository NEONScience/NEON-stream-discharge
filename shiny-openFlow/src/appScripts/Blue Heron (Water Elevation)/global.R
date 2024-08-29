### Project Info ####
#*********************************************#
# Project: Blue Heron                         #
# FS Contact: smithl3@battelleecology.org     #
# SCI Contact: ncatolico@battelleecology.org  #
#*********************************************#



# Use the code below to install neonUtilities and geoNEON packages for this program
# install.packages("neonUtilities") 
# devtools::install_github("NEONScience/NEON-geolocation/geoNEON")  # work with NEON spatial data

#Initialize package required list and see what is not installed.
packageList <- c("shiny","shinyWidgets","tidyverse","dplyr","neonUtilities","geoNEON","plotly","shinyjs","lubridate")
packages_not_installed <- packageList[!(packageList %in% installed.packages()[,"Package"])] 


#install any packages that have not been that are required
if(length(packages_not_installed) > 0){
  #Special exception for installing geoNEON library since it is hosted on GitHub
  if(grep("geoNEON",packages_not_installed)){
    devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
    packages_not_installed<- packages_not_installed[!grepl("geoNEON",packages_not_installed)]
    
  }
  if(length(packages_not_installed) > 0){
    install.packages(packages_not_installed)
  }
}

#Load the required libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
# library(dplyr)
library(neonUtilities)
library(geoNEON)
library(plotly)
library(shinyjs)
library(lubridate)



#Set directory to file path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Source function files
source('functions/def.read.cal.xml.rest.R')
source('functions/grabCalibrations.R')


#read in domain/site information
domainSite_info <- read.csv("data/domainSites_table.csv") %>% filter(!grepl("Terrestrial",field_site_type))
domainSite_info <- domainSite_info[order(domainSite_info$field_domain_id, domainSite_info$field_site_id),]

#Initialize load bar percentage
spatialDataLB_percent <- 0
spatialDataLB_title <- "Initializing data load"

#Initialize water variables
waterDensity <<- 999
gravity <<- 9.81

#Water pressure function, calculates water pressure from calibration and troll pressure values
waterPressure_calc <- function(calValCP0,calValCP1, calValCP2, trollPressure)
{
  waterPressure <- calValCP2*(trollPressure^2)+calValCP1*trollPressure+calValCP0
  return(waterPressure)
}

#water depth function, calculates water depth from water pressure, density, and gravity
waterDepth_calc <- function(waterPressure)
{
  waterDepth <- round(1000*(waterPressure/(999*9.81)),4) # 9.81 is gravity and 999 is the water density
  return(waterDepth)
}

waterColumnHeight_calc <- function(waterDepth, wellDepth, cableLength)
{
  if(!is.null(wellDepth) & !is.null(cableLength))
  {
    waterColumnHeight <- round(waterDepth + wellDepth - cableLength,4)
  }
}