
library(devtools)
library(roxygen2)

#setwd("C:/Users/kcawley/Documents/GitHub/NEON-stream-discharge")
#create("stageQCurve")

#dataDirKMC <- "C:/Users/kcawley/Downloads/NEON_discharge-stream-saltbased.zip"

#sbdFormatted <- def.format.Q(dataDir = dataDirKMC)
#write.csv(sbdFormatted, 'C:\\Users\\kcawley\\Documents\\GitHub\\biogeochemistryIPT\\reaeration\\Science Only\\rCodeForRelease\\streamQ\\inst\\extdata\\sbdFormatted.csv', row.names = F, na = '')

#sbdDataPlusInjQ <- def.calc.Q.inj(inputFile = sbdFormatted)

#sbdDataPlusSlugQ <- def.calc.Q.slug(inputFile = sbdDataPlusInjQ, dataDir = dataDirKMC, pick = T)
#sbdDataPlusSlugQ <- def.calc.Q.slug(inputFile = sbdDataPlusInjQ, dataDir = dataDirKMC, plot = T)
#sbdDataPlusSlugQ <- def.calc.Q.slug(inputFile = sbdDataPlusInjQ, dataDir = dataDirKMC)

setwd("C:/Users/kcawley/Documents/GitHub/NEON-stream-discharge/L4Discharge/stageQCurve")
#devtools::use_data(sbdFormatted, sbdFormatted, overwrite = T)
document()
devtools::check()

setwd("C:/Users/kcawley/Documents/GitHub/NEON-stream-discharge/L4Discharge")
install("stageQCurve")
library(stageQCurve)
