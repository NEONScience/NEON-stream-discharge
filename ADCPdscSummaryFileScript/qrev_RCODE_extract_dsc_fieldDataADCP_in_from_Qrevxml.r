#' R script to extract data for the L0 ingest for <dsc_fieldDataADCP_in> ingest table from a Qrev.xml file
#' Second attempt... after tutorials
#' ^ This file is experimental, figureing out the Qrev file
#' Based on R script created by Laura Fields-Sommers (lfields-sommers@battelleecology.org) on 6/19/2019 for extracting data from a Qrev.xml file

#' 
#' DIRECTIONS
#' put script in a working directory with one Qrev.xml and one .mmt files for a single discharge measurement you wish to extract data from
#' open R session with local directory, and/or set your working directory as folder with .r script, and the associated Qrev.xml and .mmt files 
#' source the R script
#' OUTPUT will be a tab delimited .txt file ready to be ingested
#' Have to Update the Versions

# Record Version
VX <- "A"
# 28 samplingProtocol
samplingProtocol <-  'NEON.DOC.001085'

###############Warning: this is provisional script, subject to revision, and not curremtly intended for public use.  Please do not disseminate.   

##################### Packages ###########################
library(readr) #package used to export the created file
# r packages and options .mmt file
library(tidyverse)
library(xml2)
options(stringsAsFactors = FALSE)
# r packages and options Qrev.xml file
library(XML) #Package for parsing the XML file into R
library(methods) #base R package needed to be loaded to use the XML package
options(stringsAsFactors = FALSE)

######## Get list of file names in the directory that this script is located in ######
working_dir_file_list <- list.files()

########## Pull from .mmt File #########
require("tidyverse")
require("xml2")

# get list of .mmt files to parse
mmt_file_list <- working_dir_file_list[grep('(?i)\\.mmt', working_dir_file_list)]
mmt_xmlfile <- read_xml(mmt_file_list)
rawDataFileName <- gsub("(?i)\\.mmt",".zip",mmt_file_list)


# 3 stationID
#Out of place because #1 rawDataFileName needs to pull the site_id
site_id <- xml_find_all(mmt_xmlfile, xpath = '//Site_Information/Name') %>% 
  as_list() %>% unlist()
#4 letter code only
site_id <- sub("^([[:alpha:]]*).*", "\\1", site_id)

# 1 rawDataFileName
  #' Ingest Workbook Designating the rawDataFileName = REGEX in ingest as follows
  #'	[REQUIRE][MATCH_REGULAR_EXPRESSION('NEON_D[0-9]{2}_[A-Z]{4}_20[0-9]{2}(0[1-9]|1[012])(0[1-9]|1[0-9]|2[0-9]|3[01])_[RIVER|ADCP]_DISCHARGE_L0(_part[0-9])?_V[A-Z]\\.zip')] 
  #'	NEON_DXX_SITE__YYYYMMDD_ADCP_DISCHARGE_L0_VX
  #Domain number
DXX <- if(site_id == "HOPB") { print("D01")
} else if (site_id == "LEWI") { print("D02")
} else if (site_id == "POSE") { print("D02")
} else if (site_id == "FLNT") { print("D03")
} else if (site_id == "CUPE") { print("D04")
} else if (site_id == "GUIL") { print("D04")
} else if (site_id == "KING") { print("D06")
} else if (site_id == "MCDI") { print("D06")
} else if (site_id == "LECO") { print("D07")
} else if (site_id == "WALK") { print("D07")
} else if (site_id == "BLWA") { print("D08")
} else if (site_id == "MAYF") { print("D08")
} else if (site_id == "ARIK") { print("D10")
} else if (site_id == "BLUE") { print("D11")
} else if (site_id == "PRIN") { print("D11")
} else if (site_id == "BLDE") { print("D12")
} else if (site_id == "COMO") { print("D13")
} else if (site_id == "WLOU") { print("D13")
} else if (site_id == "SYCA") { print("D14")
} else if (site_id == "REDB") { print("D15")
} else if (site_id == "MART") { print("D16")
} else if (site_id == "MCRA") { print("D16")
} else if (site_id == "TECR") { print("D17")
} else if (site_id == "BIGC") { print("D17")
} else if (site_id == "OKSR") { print("D18")
} else if (site_id == "TOOK") { print("D18")
} else if (site_id == "CARI") { print("D19")
} 
  # Site Name 
SITE <- site_id
  # Sampling Date 
measDate <- xml_find_all(mmt_xmlfile, xpath = '//Measurement_Date')%>% 
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)
splitUp <- strsplit(measDate, "/")%>% unlist() 
YYYYMMDD <- paste0(splitUp[3], splitUp[1],splitUp[2])
  # Version of file: See above
  #Whole rawDataFileName
rawDataFileName <- paste0("NEON_", DXX, "_", SITE,"_", YYYYMMDD, "_ADCP_DISCHARGE_L0_V", VX)


# 2 time_zone       
# get time zone from .mmt file. If none is provided, use UTC
time_zone <- NA
time_zone <- xml_find_all(mmt_xmlfile, xpath = '//TimeZone') %>% 
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)
# rename time zones
if(!time_zone%in%OlsonNames()){
  time_zone <- ifelse(
    grepl('(?i)eastern', time_zone), 
    'US/Eastern', time_zone)
  time_zone <- ifelse(
    grepl('(?i)central', time_zone), 
    'US/Central', time_zone)
  time_zone <- ifelse(
    grepl('(?i)mountain', time_zone), 
    'US/Mountain', time_zone)
  time_zone <- ifelse(
    grepl('(?i)pacific', time_zone), 
    'US/Pacific', time_zone)
  time_zone <- ifelse(grepl('(?i)universal', time_zone), 'UTC', time_zone)
}

if(is.na(time_zone))   time_zone <- 'UTC'


# 4 aCollectedBy 
#Get field party from user entered info
field_party <- xml_find_all(mmt_xmlfile, xpath = '//Party') %>% 
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)
#Parse out field Part
#Parse out field_party initials 
x<-unlist(strsplit(field_party,","))
aCollectedBy<-x[1]
bCollectedBy<-x[2]

               
aCollectedBy <- x[1]

# 5 bCollectedBy
bCollectedBy<-x[2]
bCollectedBy <- gsub(" ", "", bCollectedBy)

# 6 startDate 
node_of_interest <- xml_find_all(mmt_xmlfile, xpath = '//Discharge_Summary/None')
num_children <- xml_length(node_of_interest)
df_transects <- data.frame()
for(i_node in 1:num_children){
  # Node for summary info in pdf?
  child_node <- xml_children(node_of_interest)[[i_node]]
  xml_length(child_node)
  xml_name(child_node)
  summary_list <- as_list(child_node)
  df<- data.frame(summary_list)
  names(df) <- names(summary_list)
  df_transects <- bind_rows(df_transects, df)
}
df_transects_keepers <- df_transects %>% filter(UseInSummary == 1)
# reformat start and end times
# NOTE: we read and write in time zone embedded in .mmt file
df_transects_keepers$StartTime_POSIXct <- as.POSIXct.numeric(
  as.numeric(df_transects_keepers$StartTime),
  origin = as.POSIXct('1970-01-01', tz = time_zone),
  tz = time_zone) %>%
  format('%Y-%m-%dT%H:%M:%S')
startDate <-  min(df_transects_keepers$StartTime_POSIXct)

# 7 endDate 
df_transects_keepers$EndTime_POSIXct <- as.POSIXct.numeric(
  as.numeric(df_transects_keepers$EndTime),
  origin = as.POSIXct('1970-01-01', tz = time_zone),
  tz = time_zone) %>%
  format('%Y-%m-%dT%H:%M:%S')
endDate <-max(df_transects_keepers$EndTime_POSIXct)

# 8 waterTemperature 
water_temp <- xml_find_all(mmt_xmlfile, xpath = '//Water_Temperature') %>% 
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)

# 9 Gauge Height
#Get values
gauge_height <- xml_find_all(mmt_xmlfile, xpath = '//Inside_Gage_Height') %>% 
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)

# 19 windSpeedPrior 
wind_speed_text <- xml_find_all(mmt_xmlfile, xpath = '//Wind_Speed') %>% 
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)
# get windspeed units and convert if in mph
# make windspeed in km/h
wind_speed <- NA
wind_speed_split <- character(0)
if(!is.na(wind_speed_text)) wind_speed_split <- wind_speed_text %>% strsplit(.,' ') %>% unlist() 
if(length(wind_speed_split) == 2){
  wind_speed_units <- wind_speed_split[2]
  if(grepl('(?i)kmh|km\\/h|kph|kilometer', wind_speed_units)){
    wind_speed <- wind_speed_split[1] %>% as.numeric()
    
  }else if(grepl('(?i)mph|mi\\/h|mile', wind_speed_units)){
    # convert to kph of units are mph
    wind_speed <- as.numeric(wind_speed_split[1]) * 1.60934
  }
}else if(length(wind_speed_split) == 1){
  # assume kph if no units are given
  wind_speed <- as.numeric(wind_speed_text)
}


# 20 windDirRelativeToFlow 
wind_direction <- xml_find_all(mmt_xmlfile, xpath = '//Wind_Direction') %>% 
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)

# 24 adcpCompassError 
#extract compass calibration information
#find compass calibration node with calibration Used
compass_calibration_node <- xml_find_all(mmt_xmlfile, xpath = '//Compass_Calibration')
#turn node into a list
compass_calibration_list <- list()
compass_calibration_list <- as_list(compass_calibration_node)[[1]]
if(length(compass_calibration_list) > 1){
  # which result was saved
  which_result_saved <- which(
    unlist(lapply(compass_calibration_list, 
                  function(x){
                    !grepl('(?i)This Calibration NOT Saved', x$Text)})))
  compass_calibration_text <- compass_calibration_list[[which_result_saved]]$Text %>% unlist()
  
}else{
  
  # handle use cases where compass calibration is either null or only 1 entry
  compass_calibration_text <- xml_find_all(mmt_xmlfile, xpath = '//Compass_Evaluation') %>% 
    as_list() %>% unlist() %>%
    ifelse(is.null(.), NA, .)
}
# extract compass error from text
prefix_text <- "Addition Of Single And Double Cycle Errors: "
compass_error_substring <- substring(compass_calibration_text, regexpr(prefix_text, compass_calibration_text) + nchar(prefix_text)) 
compass_error <- compass_error_substring %>%
  gregexpr('[0-9|\\.]+', .) %>% regmatches(compass_error_substring, .) %>% unlist() %>%
  ifelse(is.null(.), NA, .)


# 25 adcpCompassCalibrated from .mmt file
compass_calibration <- ifelse(!is.na(compass_error), 'Y','N')


########## Pull from QRev File #########
require("XML")
require("methods")

# get list of QRev.xml files to parse
Qrev_fileURL <- working_dir_file_list[grep('(?i)\\QRev.xml', working_dir_file_list)]
#read the QRev.xml Files, keeing the ojbect structure
Qrev_xmlfile <- read_xml(Qrev_fileURL)
Parse_Qrev <- xmlParse(Qrev_fileURL)

# 3 stationID
#site_id <- xml_find_all(mmt_xmlfile, xpath = '//Site_Information/Name') %>% 
 # as_list() %>% unlist() %>%
  #ifelse(is.null(.), NA, .)
#Another option is to get it from the Qrevfile: <Channel> <SiteInformation> <StationID> 
#site_id <- xml_find_all(Qrev_xmlfile, xpath = "//SiteInformation/SiteID")%>% 
 # as_list() %>% unlist() %>%
  #ifelse(is.null(.), NA, .)  

# 11 totalDischarge
#Path <Channel> <ChannelSummary> <Discharge> <Total     </Total>
total_discharge <- xml_find_all(Qrev_xmlfile, xpath = '//ChannelSummary/Discharge/Total') %>% 
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)  
  

# 12 totalDischargeUnits
#Path <Channel>  <ChannelSummary> <Discharge> <Total  unitsCode="cms" </Total>
disch_units <- xpathSApply(Parse_Qrev, "//ChannelSummary/Discharge/Total", xmlGetAttr, 'unitsCode')




 # 13 totalDischargeRU-> chance to estimated95%UC and will need to update the DPUG
    #Old way of doing this from Eric's .mmt code
        #Pull total Discharge from all transect nodes and make them numeric 
        #Path <Transect> <Discharge> <Total  </Total>
        #total_q <- xml_find_all(Qrev_xmlfile, xpath = '//Transect/Discharge/Total') %>% xml_text() %>% as.numeric()
        #Calculate the total Discharge Standard deviation from the transects
        #sd_totalQ <- sd(total_q, na.rm = TRUE)
#New work in Qrev calculated uncertainty
estimatedUC <- xml_find_all(Qrev_xmlfile, xpath = '//ChannelSummary/Uncertainty/Total')%>%
  as_list() %>% unlist() %>%
  ifelse(is.null(.), NA, .)  
# This is the estimated 95% uncertainty calculated in the QRev program. Definition provided for QRev estimated 95% uncertainty - The estimated 95 percent uncertainty from the discussed categories and combines them as the square roots of the sum of squares. The final value is only a guide because the algorithms for the various sources of uncertainty are only approximations and simple assumptions. 
# The name for this parameter in the file may change. 
# The DPUG for the output file will need to be updated for this parameter
# The units for this value is %






# 14 riverVelcityMaximum
#Path <ChannelSummary> <Other> <MaximumWaterSpeed          </MaximumWaterSpeed>
maxVeolocity <- xml_find_all(Qrev_xmlfile, xpath = '//ChannelSummary/Other/MaximumWaterSpeed') %>% 
    as_list() %>% unlist() %>%
    ifelse(is.null(.), NA, .)  
  

# 15 riverWidthMean
#Path <Channel> <ChannelSummary> <Other> <MeanWidth          </MeanWidth>
meanWidth <- xml_find_all(Qrev_xmlfile, xpath = '//ChannelSummary/Other/MeanWidth') %>% 
      as_list() %>% unlist() %>%
      ifelse(is.null(.), NA, .)  
  
 # 16 maxDepth
#Path <Channel> <ChannelSummary> <Other> <MaximumDepth             </MaximumDepth>
maxDepth <- xml_find_all(Qrev_xmlfile, xpath = '//ChannelSummary/Other/MaximumDepth') %>% 
      as_list() %>% unlist() %>%
      ifelse(is.null(.), NA, .)  
  
# 17 riverDepthMean
#Path <Channel>  <ChannelSummary> <Other> <MeanDepth            </MeanDepth>
meanDepth <- xml_find_all(Qrev_xmlfile, xpath = '//ChannelSummary/Other/MeanDepth') %>% 
      as_list() %>% unlist() %>%
      ifelse(is.null(.), NA, .)  
  
# 18 riverDischargeMeasDuration
#Path <Channel> <ChannelSummary> <Other> <Duration        </Duration>
mDuration <- xml_find_all(Qrev_xmlfile, xpath = '//ChannelSummary/Other/Duration') %>% 
      as_list() %>% unlist() %>%
      ifelse(is.null(.), NA, .) 


# 21 velocityUnits
#Path <Channel> <ChannelSummary> <Other> <MaximumWaterSpeed unitsCode="mps"      </MaximumWaterSpeed>
units_vel <- xpathSApply(Parse_Qrev, "//ChannelSummary/Other/MaximumWaterSpeed", xmlGetAttr, 'unitsCode')
velunits <- if (units_vel == "mps"){
  print("m/s")
} else{
  print("Units warning")
}
  
# 22 widthUnits
#Path <Channel> <ChannelSummary> <Other> <MeanWidth unitsCode="m"      </MeanWidth>
width_units <- xpathSApply(Parse_Qrev, "//ChannelSummary/Other/MeanWidth", xmlGetAttr, 'unitsCode')  
        
# 23 magneticVariation
#Path <Channel> <Processing> <Navigation> <MagneticVariation       </MagneticVariation>  
magnecticVar <- xml_find_all(Qrev_xmlfile, xpath = '//Processing/Navigation/MagneticVariation') %>% 
        as_list() %>% unlist() %>%
        ifelse(is.null(.), NA, .) 
      
      
# 26 stationaryMBT
# Path <Channel> <QA> <MovingBedTest> <TestType        </TestType>
  #Special note N if loop test, Y if stationary... shouldn't 'be stationary
MBT <- xml_find_all(Qrev_xmlfile, xpath = '//QA/MovingBedTest/TestType') %>% 
        as_list() %>% unlist() %>%
        ifelse(is.null(.), NA, .) 
      
if(MBT == "Stationary") {
        sMBT <- "Y"
      } else {
        sMBT <- "N"
      }


      
# 27 loopMBT
#Path <Channel> <QA> <MovingBedTest> <TestType      </TestType>
  #Special note  Y if loop test, N if stationary... shouldnt be stationary
  
if(MBT == "Loop") {
        lMBT <- "Y"
      } else {
        lMBT <- "N"
      }      
     

####### Manually Change #################

# 10 streamStageUnits
streamStageUnits <-  'm'


# 28 samplingProtocol
samplingProtocol <-  'NEON.DOC.001085'


####################### fill in the output data Frame ########################

outputDF <- data.frame(
  rawDataFileName = rawDataFileName,
  time_zone = time_zone,
  stationID = site_id,
  aCollectedBy = aCollectedBy,
  bCollectedBy = bCollectedBy,
  startDate = startDate,
  endDate = endDate,
  waterTemperature = water_temp,
  streamStage = gauge_height,
  streamStageUnits = streamStageUnits,
  totalDischarge = total_discharge,
  totalDischargeUnits = disch_units,
  estimated95percentUC = estimatedUC,
  riverVelocityMaximum = maxVeolocity,
  riverWidthMean = meanWidth,
  maxDepth = maxDepth,
  riverDepthMean = meanDepth,
  riverDischargeMeasDuration = mDuration,
  windSpeedPrior = wind_speed,
  windDirRelativeToFlow = wind_direction,
  velocityUnits = velunits,
  widthUnits = width_units,
  magneticVariation = magnecticVar,
  adcpCompassError = compass_error,
  adcpCompassCalibrated = compass_calibration,
  stationaryMBT = sMBT,
  loopMBT = lMBT,
  samplingProtocol = samplingProtocol
)



#totalDischargeRU = sd_totalQ, #replacing this with estimated95%UC

######## Export the data ##############
# Name the desired file in directory for the export file
output_dir_name <- 'R_SCRIPT_OUTPUTQrev'

# Create the new folder to house the export file
if(!file.exists(output_dir_name)) dir.create(paste(getwd(),output_dir_name, sep = '/'))

# Name the export file
outputFileName <- outputDF$rawDataFileName %>% 
  gsub("(?i).zip",".txt", .) %>% 
  paste0(output_dir_name,'/', .)

# Export the file
write_delim(outputDF, outputFileName, delim = '\t', na = 'NA', quote_escape = "none")



  
  
  
  
  
  
  
  
