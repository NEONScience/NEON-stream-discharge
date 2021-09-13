##############################################################################################
#' @title Example Script for use with neonUtilities 2.0+

#' @author
#' Kaelin M. Cawley \email{kcawley@battelleecology.org} \cr

#' @description This script downloads and formats salt based discharge data from the 
#' NEON data portal in order to calculate stream discharge.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords surface water, streams, rivers, discharge, flow

# changelog and author contributions / copyrights
#   Kaelin M. Cawley (2021-03-12)
#     original creation
##############################################################################################

#User Inputs
siteID <- "ARIK"
# siteID <- "COMO"

#String constants
sbdDPID <- "DP1.20193.001"

# Download Discharge Data
qInputList <- neonUtilities::loadByProduct(dpID = sbdDPID, site = siteID, check.size = FALSE)
names(qInputList)

sbd_backgroundFieldCondDataIn <- qInputList$sbd_backgroundFieldCondData
sbd_backgroundFieldSaltDataIn <- qInputList$sbd_backgroundFieldSaltData
sbd_conductivityFieldDataIn <- qInputList$sbd_conductivityFieldData
sbd_fieldDataIn <- qInputList$sbd_fieldData
sbd_plateauDataCondIn <- qInputList$sbd_plateauMeasurementFieldData
sbd_plateauDataSaltIn <- qInputList$sbd_plateauSampleFieldData
sbd_externalLabDataSaltIn <- qInputList$sbd_externalLabDataSalt

#ARIK testing
backgroundDataLogger = sbd_backgroundFieldCondDataIn
fieldDataSite = sbd_fieldDataIn
backgroundDataSalt = NULL
plateauDataCond = NULL
plateauDataSalt = NULL
externalLabDataSalt = NULL
processingInfo = NULL



plateauDataCond = sbd_plateauDataCondIn
plateauDataSalt = sbd_plateauDataSaltIn
externalLabDataSalt = sbd_externalLabDataSaltIn


sbdFormatted <- streamQ::def.format.Q(backgroundDataLogger = sbd_backgroundFieldCondDataIn,
                                      backgroundDataSalt = NULL,
                                      fieldDataSite = sbd_fieldDataIn,
                                      plateauDataCond = NULL,
                                      plateauDataSalt = NULL,
                                      externalLabDataSalt = NULL)

inputFile = sbdFormatted
conductivityData = sbd_conductivityFieldDataIn

dscSlug <- streamQ::def.calc.Q.slug(inputFile = sbdFormatted,
                                    conductivityData = sbd_conductivityFieldDataIn)


# inputFile = reaFormatted
# loggerData = reaInputList$rea_conductivityFieldData
# namedLocation = "namedLocation"
# injectionTypeName = "injectionType"
# eventID = "eventID"
# stationToInjectionDistance = "stationToInjectionDistance"
# plateauGasConc = "plateauGasConc"
# corrPlatSaltConc = "corrPlatSaltConc"
# hoboSampleID = "hoboSampleID"
# discharge = "fieldDischarge"
# waterTemp = "waterTemp"
# wettedWidth = "wettedWidth"
# plot = TRUE
# savePlotPath = NULL
# processingInfo = NULL

reaRatesCalc <- reaRate::def.calc.reaeration(inputFile = reaFormatted,
                                             loggerData = reaInputList$rea_conductivityFieldData,
                                             namedLocation = "namedLocation",
                                             injectionTypeName = "injectionType",
                                             eventID = "eventID",
                                             stationToInjectionDistance = "stationToInjectionDistance",
                                             plateauGasConc = "plateauGasConc",
                                             corrPlatSaltConc = "corrPlatSaltConc",
                                             hoboSampleID = "hoboSampleID",
                                             discharge = "fieldDischarge",
                                             waterTemp = "waterTemp",
                                             wettedWidth = "wettedWidth",
                                             plot = TRUE,
                                             savePlotPath = NULL,
                                             processingInfo = NULL)

outputDF <- reaRatesCalc$outputDF
outputDFClean <- outputDF[outputDF$k600 > 0,]
plot(outputDFClean$meanQ, outputDFClean$k600)
